// #define UID_ERL_LOAD_NFS

/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sys.h"

#include "erl_vm.h"

#include "global.h"

#ifdef __rtems__
#include <machine/rtems-bsd-commands.h>
#include <rtems.h>
#include <rtems/bdbuf.h>
#include <rtems/bsd/bsd.h>
#include <rtems/console.h>
#include <rtems/libio.h>
#include <rtems/malloc.h>
#include <rtems/libcsupport.h>
#include <rtems/media.h>
#include <rtems/score/armv7m.h>
#include <rtems/shell.h>
#include <rtems/stringto.h>

#include <assert.h>
#include <bsp.h>
#include <bsp/bootcard.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sysexits.h>
#include <unistd.h>
#include <inttypes.h>

#include <inih/ini.h>

#include <grisp/eeprom.h>
#include <grisp/init.h>
#include <grisp/led.h>

#ifdef GRISP_PLATFORM_GRISP2
#include <bsp/fdt.h>
#include <libfdt.h>
#define FDT_MOUNTPOINT_NODE "/chosen"
#define FDT_MOUNTPOINT_PROPERTY "grisp-mountpoint"
#define DEFAULT_MNT "/media/mmcsd-1-0/"
#else
#define DEFAULT_MNT "/media/mmcsd-0-0/"
#endif
#define INI_FILENAME "grisp.ini"

#define STACK_SIZE_INIT_TASK (512 * 1024)

#define PRIO_DHCP (RTEMS_MAXIMUM_PRIORITY - 1)
#define PRIO_WPA (RTEMS_MAXIMUM_PRIORITY - 1)

#define SHELL_STACK_SIZE (RTEMS_MINIMUM_STACK_SIZE * 4)
#define CONSOLE_DEVICE_NAME "/dev/console"

#define BUFFER_SIZE 1024

#define WDOG_BASE_ADDR 0x020BC000  // Base address for WDOG on i.MX6
#define WDOG_WCR_OFFSET 0x00       // Watchdog Control Register
#define WDOG_WCR (*(volatile uint16_t *)(WDOG_BASE_ADDR + WDOG_WCR_OFFSET))
#define WDOG_WCR_WDE     (1 << 2)  // Watchdog enable

void parse_args(char *args);

#ifndef GRISP_PLATFORM_GRISP_NANO
static int start_dhcp = 0;
static int wlan_adhocmode = 0;
static int wlan_enable = 0;

static char *ip_self = "";
static char *wlan_ip_netmask = "";
#endif

/*
 * The optional shell option can be set to either erlang, rtems or none.
 * When setting it to rtems, the extra erlang arguments -noshell and -noinput
 * are enforced after the -- if the first part of the option is erl.rtems.
 *
 * e.g
 *
 * To select the console shell, set the shell option in the configuration
 * template grisp/grisp_base/files/grisp.ini.mustache
 *
 * Erlang shell:
 *     By default without any shell option, the default is to use the erlang
 *     shell, but it can be specified explcitly:
 *
 *         [erlang]
 *         args = erl.rtems -- <rest of emulator flags...>
 *         shell=erlang
 *
 * RTEMS shell :
 *
 *         [erlang]
 *         args = erl.rtems -- <rest of emulator flags...>
 *         shell=rtems
 *
 * No shell :
 *
 *         [erlang]
 *         args = erl.rtems -- <rest of emulator flags...>
 *         shell=none
 *
 */
static int shell = 0;

/*
 * Infrastructure mode by default.
 * Can be set to "adhoc" to allow ad hoc networking
 * (WANET) between nodes without the need for
 * Access Point connections.
 *
 * Example of grisp.ini parameters to setup an ad hoc
 * network :
 *
 *     [network]
 *     wlan=enable
 *     ip_self=169.254.16.1
 *     wlan_ip_netmask=255.255.0.0
 *     wlan_mode=adhoc
 *     wlan_adhocname=edge
 *     wlan_channel=6
 *     hostname=my_grisp_board_1
 *
 * This will create a network named "edge"
 * in channel 6 and the host "my_grisp_board_1"
 * will be using address "169.254.16.1/16"
 */
#ifndef GRISP_PLATFORM_GRISP_NANO
static char *wlan_mode = "infrastructure";
#endif

/*
* During experiments in adhoc mode, it has been
* observed that nodes tend to be more vulnerable
* to wireless interferences. This impact can be
* minimized with manual wireless channel selection.
* In Europe, the available channels in the 2.4GHz band
* range from 1 to 13.
* 802.11g WLAN standards state that each channel has a
* bandwidth of 20 MHz, separated from others
* by 5 MHz intervals. It also recommends a gap 25 MHz between
* the center of channel spectrums to avoid signal degradation.
*
* Thus the non-overlapping channels are 1,6 and 11 :
*
*        Frequency (MHz)
* Ch #  Low     Mid     Up

---------------------------------
*  1    2401    2412    2423
---------------------------------
*  2    2406    2417    2428
*  3    2411    2422    2433
*  4    2416    2427    2438
*  5    2421    2432    2443
---------------------------------
*  6    2426    2437    2448
---------------------------------
*  7    2431    2442    2453
*  8    2436    2447    2458
*  9    2441    2452    2463
* 10    2446    2457    2468
---------------------------------
* 11    2451    2462    2473
---------------------------------
* 12    2456    2467    2478
* 13    2461    2472    2483

* In further experiments, a noticeable
* improvement in maximum range has been
* observed when nodes communicate through channel 6.
* This is due to the fact that interferences were
* mostly found in channels 1 and 11. Hence a possible
* way to improve adhoc network quality is to scan
* the environment for interferences before selecting
* the channel that is as far as possible from interfering
* devices in the 2.4GHz band.
*/
#ifndef GRISP_PLATFORM_GRISP_NANO
static char *wlan_channel = "6";

static char *wlan_adhocname = "adhocnetwork";

static char *hostname = "grisp";

static char *wpa_supplicant_conf = NULL;
#endif

/*
 * RTEMS log priority can be set to either alert, crit, debug, emerg, err, info,
 * notice or warning. If not specified it is set to err. Example:
 *
 *     [rtems]
 *     log_priority=debug
 */
static char *rtems_log_priority = "err";

/*
 * Define exit and crash reboot policy. Could be set to either reboot or wait.
 * Example:
 *
 *     [erlang]
 *     on_exit=reboot
 *     on_crash=wait
 */
static int reboot_on_exit = 0;
static int reboot_on_crash = 0;

static char *erl_args;
static const char *default_erl_args = "erl.rtems -- "
                                      "-root otp "
                                      "-home home "
                                      "-boot start_sasl "
                                      "-pa .";

#define MAX_ARGC 256

static char *argv[MAX_ARGC];
static int argc;

// Function prototypes
void maybe_reboot(int should_reboot);
void fatal_extension(rtems_fatal_source source, bool is_internal, rtems_fatal_code error);
void fatal_atexit(void);
char* silence_erl_console(char *args);
void join_paths(const char *part1, const char *part2, char *result, size_t max_size);
int copy_file(const char *src_path, const char *dst_path);
int copy_directory(const char *src_dir, const char *dst_dir);
void setup_etc(const char *root_dir);

#ifndef GRISP_PLATFORM_GRISP_NANO
void set_grisp_hostname(struct grisp_eeprom *eeprom);
#endif

static char *strdupcat(const char *s1, const char *s2) {
  char *res;
  size_t len;

  len = strlen(s1) + strlen(s2) + 1;
  res = malloc(len);
  strlcpy(res, s1, len);
  strlcat(res, s2, len);

  return res;
}

void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset) {
  (void) addr;
  (void) len;
  (void) prot;
  (void) flags;
  (void) fd;
  (void) offset;
  errno = ENODEV;
  return MAP_FAILED;
}

int munmap(void *addr, size_t len) {
  (void) addr;
  (void) len;
  errno = EINVAL;
  return -1;
}

void maybe_reboot(int should_reboot) {
#ifdef GRISP_PLATFORM_GRISP2
  /* TODO: Test if calling bsp_reset() would work */
  if (should_reboot != 0) {
    printk("[ERL] Rebooting...\n");
    sleep(2);
    WDOG_WCR = WDOG_WCR_WDE;
  }
#elif defined(GRISP_PLATFORM_GRISP_NANO)
  if (should_reboot != 0) {
    printk("[ERL] Rebooting...\n");
    sleep(2);
    bsp_reset(RTEMS_FATAL_SOURCE_BSP, 0);
  }
#else
  (void) should_reboot;
#endif
  while (1);
}

void fatal_extension(rtems_fatal_source source, bool is_internal, rtems_fatal_code error) {
  printk("\n\nfatal extension: source=%d, is_internal=%d, error=%d\n",
         source, is_internal, error);
  if (source == RTEMS_FATAL_SOURCE_EXCEPTION)
    rtems_exception_frame_print((const rtems_exception_frame *)error);

  /* rtems_stack_checker_report_usage(); */
  maybe_reboot(reboot_on_crash);
}

void fatal_atexit(void) {
  printk("\n\n[ERL] Beam exited\n");
  /* rtems_stack_checker_report_usage(); */
  maybe_reboot(reboot_on_exit);
}

char* silence_erl_console(char *args) {
  char *pos, *new_args;
  size_t added_length, new_len, length_before;

  /* Check if the arguments are for the erlang VM */
  if (strncmp(args, "erl.rtems", strlen("erl.rtems")) != 0) {
      // If not, return the original string
      return args;
  }

  /* Find the position of the "--" */
  pos = strstr(args, "--");
  if (!pos) {
    /* If "--" is not found, return the original string */
    return args;
  }

  /* Calculate the new string length */
  added_length = strlen(" -noshell -noinput");
  new_len = strlen(args) + added_length;
  new_args = (char*) malloc(new_len + 1); /* +1 for the null terminator */
  if (!new_args) {
    /* Memory allocation failed, return the original string */
    return args;
  }

  /* Copy the parts of the original string and insert the new strings */
  length_before = (size_t)(pos - args + 2); /* +2 to include the "--" */
  strncpy(new_args, args, length_before);
  strcpy(new_args + length_before, " -noshell -noinput");
  strcpy(new_args + length_before + added_length, pos + 2);

  /* Release the original string */
  free(args);

  return new_args;
}

static int ini_file_handler(void *arg, const char *section, const char *name,
                            const char *value) {
  int ok = 0;

#ifndef GRISP_PLATFORM_GRISP_NANO

  const char *rootdir = (const char*)arg;

  if (strcmp(section, "network") == 0) {
    if (strcmp(name, "hostname") == 0) {
      hostname = strdup(value);
      ok = 1;
    } else if (strcmp(name, "ip_self") == 0) {
      if (strcmp(value, "dhcp") == 0) {
        start_dhcp = 1;
        ok = 1;
      } else {
        ip_self = strdup(value); // Set ip from ini file
        printf("=== Ip is %s ===\n", ip_self);
        ok = 1;
      }
    } else if (strcmp(name, "wlan_ip_netmask") == 0) {
      wlan_ip_netmask = strdup(value); // Set netmask from ini file
      ok = 1;
    } else if (strcmp(name, "wlan_channel") == 0) {
      wlan_channel = strdup(value); // Set channel from ini file
      ok = 1;
    } else if (strcmp(name, "wlan_mode") == 0) {
      if (strcmp(value, "adhoc") == 0) {
        wlan_adhocmode = 1;
        ok = 1;
      }
      wlan_mode = strdup(value); // Set wlanmode from ini file
      ok = 1;
    } else if (strcmp(name, "wlan_adhocname") == 0) {
      wlan_adhocname = strdup(value); // Set SSID from ini file
      ok = 1;
    } else if (strcmp(name, "wlan") == 0) {
      if (strcmp(value, "enable") == 0) {
        wlan_enable = 1;
        ok = 1;
      } else if (strcmp(value, "disable") == 0) {
        wlan_enable = 0;
        ok = 1;
      }
    } else if (strcmp(name, "wpa") == 0) {
      wpa_supplicant_conf = strdupcat(rootdir, value);
      ok = 1;
    }
  } else
#endif
  if (strcmp(section, "erlang") == 0) {
    if (strcmp(name, "args") == 0) {
      free(erl_args);
      erl_args = strdup(value);
      ok = 1;
    } else if (strcmp(name, "shell") == 0) {
      if (strcmp(value, "erlang") == 0) {
        shell = 0;
        ok = 1;
      } else if (strcmp(value, "rtems") == 0) {
        shell = 1;
        ok = 1;
      } else if (strcmp(value, "none") == 0) {
        shell = 2;
        ok = 1;
      }
#if defined(GRISP_PLATFORM_GRISP2) || defined(GRISP_PLATFORM_GRISP_NANO)
    } else if (strcmp(name, "on_exit") == 0) {
      if (strcmp(value, "reboot") == 0) {
        reboot_on_exit = 1;
        ok = 1;
      } else if (strcmp(value, "wait") == 0) {
        reboot_on_exit = 0;
        ok = 1;
      }
    } else if (strcmp(name, "on_crash") == 0) {
      if (strcmp(value, "reboot") == 0) {
        reboot_on_crash = 1;
        ok = 1;
      } else if (strcmp(value, "wait") == 0) {
        reboot_on_crash = 0;
        ok = 1;
      }
#endif
    }
  } else if (strcmp(section, "rtems") == 0) {
    if (strcmp(name, "log_priority") == 0) {
      if ((strcmp(value, "alert") == 0)
           || (strcmp(value, "crit") == 0)
           || (strcmp(value, "debug") == 0)
           || (strcmp(value, "emerg") == 0)
           || (strcmp(value, "err") == 0)
           || (strcmp(value, "info") == 0)
           || (strcmp(value, "notice") == 0)
           || (strcmp(value, "warning") == 0)) {
        rtems_log_priority = strdup(value);
        ok = 1;
      }
    }
  } else
    ok = 1;

  if (!ok) {
    printf("erl_main: error in configuration file: "
           "section \"%s\", name \"%s\", value \"%s\"\n",
           section, name, value);
    ok = 1;
  }

  return ok;
}

static void evaluate_ini_file(const char *rootdir, const char *ini_file) {
  int rv;

  rv = ini_parse(ini_file, ini_file_handler, (void*)rootdir);
  if (rv == -1) {
    printf("[ERL] WARNING: %s not found, using defaults\n", ini_file);
  }

  /* If the shell is not the Erlang console, we need to silence it. */
  if (shell != 0)
    erl_args = silence_erl_console(erl_args);
}

#ifndef GRISP_PLATFORM_GRISP_NANO
static void default_network_ifconfig_lo0(void) {
  int exit_code;
  char *lo0[] = {"ifconfig", "lo0",           "inet", "127.0.0.1",
                 "netmask",  "255.255.255.0", NULL};
  char *lo0_inet6[] = {"ifconfig",  "lo0", "inet6", "::1",
                       "prefixlen", "128", "alias", NULL};

  exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(lo0), lo0);
  assert(exit_code == EX_OK);

  exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(lo0_inet6), lo0_inet6);
  assert(exit_code == EX_OK);
}

static void create_wlandev(void) {
  int exit_code;
  char *ifcfg[] = {"ifconfig", "wlan0", "create", "wlandev",
                   "rtwn0",    "up",    NULL};

  exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg), ifcfg);
  if (exit_code != EXIT_SUCCESS) {
    printf("ERROR while creating wlan0 in adhoc mode.");
  }
}

static void create_wlandev_adhoc(void) {
  int exit_code;
  char *ifcfg[] = {"ifconfig", "wlan0", "create", "wlandev", "rtwn0",
                   "wlanmode", "adhoc", "channel",
                   // "6:ht/40",
                   wlan_channel, "up", NULL};

  char *ifcfg_adhoc_params[] = {"ifconfig", "wlan0",        "inet",
                                ip_self,    "netmask",      wlan_ip_netmask,
                                "ssid",     wlan_adhocname, NULL};

  exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg), ifcfg);
  if (exit_code != EXIT_SUCCESS) {
    printf("ERROR while creating wlan0 in adhoc mode.");
  }

  exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_adhoc_params),
                                         ifcfg_adhoc_params);
  if (exit_code != EXIT_SUCCESS) {
    printf("ERROR while setting up edge wlan0 in adhoc mode.");
  }
}
#endif

void parse_args(char *args) {
  char *p;
  char *last;

  for (p = strtok_r(args, " \t", &last); p; p = strtok_r(NULL, " \t", &last)) {
    if (argc >= MAX_ARGC) {
      printf("[ERL] ERROR: Too many erl arguments\n");
      exit(-1);
    }

    argv[argc++] = p;
  }
}

#ifndef GRISP_PLATFORM_GRISP_NANO
void set_grisp_hostname(struct grisp_eeprom *eeprom) {
  static char *dynhostname;
  int dynhostname_len = 0;
  int rv = 0;

  dynhostname_len = snprintf(NULL, 0, "%s-%06u", hostname, eeprom->serial);
  assert(dynhostname_len > 0);
  dynhostname = malloc(dynhostname_len + 1);
  assert(dynhostname != NULL);
  rv = snprintf(dynhostname, dynhostname_len + 1, "%s-%06u", hostname,
                eeprom->serial);
  assert(rv > 0);
  hostname = dynhostname;
}
#endif

void join_paths(const char *part1, const char *part2, char *result, size_t max_size) {
  size_t len1 = strlen(part1);
  size_t len2 = strlen(part2);
  size_t total_len = len1 + len2 + 1;

  if (part1[len1 - 1] == '/') {
    total_len--;
  }

  if (total_len >= max_size) {
    len2 = max_size - len1 - 2;
  }

  strncpy(result, part1, max_size - 1);
  result[max_size - 1] = '\0';

  if (part1[len1 - 1] != '/' && len1 < max_size - 1) {
    strncat(result, "/", max_size - strlen(result) - 1);
  }

  strncat(result, part2, max_size - strlen(result) - 1);
}

int copy_file(const char *src_path, const char *dst_path) {
  char buffer[BUFFER_SIZE];
  size_t bytes;
  FILE *src_file, *dst_file;

  printf("[ERL] Copying file %s to %s\n", src_path, dst_path);

  src_file = fopen(src_path, "rb");
  if (!src_file) {
    perror("source file fopen error during file copy");
    return -1;
  }

  dst_file = fopen(dst_path, "wb");
  if (!dst_file) {
    perror("destination file fopen error during file copy");
    fclose(src_file);
    return -1;
  }

  while ((bytes = fread(buffer, 1, BUFFER_SIZE, src_file)) > 0) {
    fwrite(buffer, 1, bytes, dst_file);
  }

  fclose(src_file);
  fclose(dst_file);

  return 0;
}

int copy_directory(const char *src_dir, const char *dst_dir) {
    DIR *dir;
    struct stat st;
    struct dirent *entry;
    char src_path[PATH_MAX];
    char dst_path[PATH_MAX];

    printf("[ERL] Copying directory %s to %s\n", src_dir, dst_dir);

    dir = opendir(src_dir);
    if (!dir) {
        perror("source directory opendir error during directory copy");
        return -1;
    }

    if (stat(dst_dir, &st) == -1) {
        mkdir(dst_dir, 0755);
    }

    while ((entry = readdir(dir)) != NULL) {
      if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
        continue;
      }

      join_paths(src_dir, entry->d_name, src_path, sizeof(src_path));
      join_paths(dst_dir, entry->d_name, dst_path, sizeof(dst_path));

      if (entry->d_type == DT_DIR) {
        copy_directory(src_path, dst_path);
      } else {
        copy_file(src_path, dst_path);
      }
    }

    closedir(dir);
    return 0;
}

void setup_etc(const char *root_dir) {
  char etc_dir[PATH_MAX];
  struct stat st;

  join_paths(root_dir, "etc", etc_dir, sizeof(etc_dir));

  if (stat(etc_dir, &st) == 0 && S_ISDIR(st.st_mode)) {
    printf("[ERL] Setting up /etc from %s\n", etc_dir);
    copy_directory(etc_dir, "/etc");
  } else {
    if (errno != ENOENT) {
      perror("stat failed during etc setup");
    } else {
      printf("[ERL] Directory %s not found\n", etc_dir);
    }
  }
}

static void static_print_rtems_memory_info(void) {
  Heap_Information_block info;
  malloc_info(&info);
  printf("  Number of used blocks: %8" PRIuPTR "\n"
         "  Largest used block:    %8" PRIuPTR "\n"
         "  Total bytes used:      %8" PRIuPTR "\n"
         "  Number of free blocks: %8" PRIuPTR "\n"
         "  Largest free block:    %8" PRIuPTR "\n"
         "  Total bytes free:      %8" PRIuPTR "\n",
         info.Used.number, info.Used.largest, info.Used.total,
         info.Free.number, info.Free.largest, info.Free.total);
}

static void Init(rtems_task_argument arg) {
  rtems_status_code sc = RTEMS_SUCCESSFUL;
  int rv = 0;
  static char pwd[1024];
  const char *rootdir;
  static char inifile[192];
  char *p;
  struct grisp_eeprom eeprom = {0};

#ifndef GRISP_PLATFORM_GRISP_NANO
  static char dhcpfile[192];
#endif

#ifdef GRISP_PLATFORM_GRISP2

  const void *fdt;
  int node_offset;
  int len;

#endif

  (void) arg;

  printf("[ERL] Initializing\n");

  atexit(fatal_atexit);
  rtems_bsd_setlogpriority(rtems_log_priority);

#ifndef GRISP_PLATFORM_GRISP_NANO
  grisp_led_set1(true, true, true);
  grisp_led_set2(false, false, false);
#endif

  printf("[ERL] Initializing buses\n");
  grisp_init_buses();

#ifndef GRISP_PLATFORM_GRISP_NANO
  printf("[ERL] Initializing EEPROM\n");
  grisp_eeprom_init();
  rv = grisp_eeprom_get(&eeprom);
#ifndef GRISP_PLATFORM_GRISP_BASE /* GRiSP1 checksum isn't correct, skip */
  if (rv == 0) {
#endif
    grisp_eeprom_dump(&eeprom);
    set_grisp_hostname(&eeprom);
#ifndef GRISP_PLATFORM_GRISP_BASE
  } else {
    printf("[ERL] ERROR: Invalid EEPROM\n");
  }
#endif
#else
  /* FIXME: EEPROM would have to be re-implemented using STM HAL */
  (void) rv;
  (void) eeprom;
#endif

  printf("[ERL] Mounting SD card asynchronously\n");
#ifdef GRISP_PLATFORM_GRISP_NANO
  grisp_init_sd_card(DEFAULT_MNT);
#else
  grisp_init_sd_card();
#endif
  printf("[ERL] Lowering self priority\n");
  grisp_init_lower_self_prio();
  printf("[ERL] Initializing libbsd\n");
  grisp_init_libbsd();
#ifndef GRISP_PLATFORM_GRISP_NANO
  printf("[ERL] Running ifconfig on lo0\n");
  default_network_ifconfig_lo0();
#endif

  /* Wait for the SD card */
#ifndef GRISP_PLATFORM_GRISP_NANO
  grisp_led_set1(true, false, true);
#endif
  sc = grisp_init_wait_for_sd();
  if (sc == RTEMS_SUCCESSFUL) {
    printf("[ERL] SD card mounted\n");
  } else {
    printf("[ERL] ERROR: SD card could not be mounted after timeout\n");
#ifndef GRISP_PLATFORM_GRISP_NANO
    grisp_led_set1(true, false, false);
#endif
  }

#ifdef GRISP_PLATFORM_GRISP2

  fdt = bsp_fdt_get();
  node_offset = fdt_path_offset(fdt, FDT_MOUNTPOINT_NODE);
  rootdir = fdt_getprop(fdt, node_offset, FDT_MOUNTPOINT_PROPERTY, &len);
  if (rootdir == NULL) {
    printf("[ERL] No GRiSP mountpoint in the FDT\n");
    rootdir = DEFAULT_MNT;
  } else {
    printf("[ERL] GRiSP mountpoint from the FDT: %s\n", rootdir);
  }

#else

  rootdir = DEFAULT_MNT;

#endif

  strlcpy(inifile, rootdir, 192);
  strlcat(inifile, INI_FILENAME, 192);

  printf("[ERL] Reading %s\n", inifile);
  erl_args = strdup(default_erl_args);
  evaluate_ini_file(rootdir, inifile);

  printf("[ERL] Booting with arg: %s\n", erl_args);
  parse_args(erl_args);

#if defined(GRISP_PLATFORM_GRISP2) || defined(GRISP_PLATFORM_GRISP_NANO)
  if (reboot_on_exit == 1) {
    printf("[ERL] On exit: reboot\n");
  } else {
    printf("[ERL] On exit: wait\n");
  }

  if (reboot_on_crash == 1) {
    printf("[ERL] On crash: reboot\n");
  } else {
    printf("[ERL] On crash: wait\n");
  }
#endif

#ifndef GRISP_PLATFORM_GRISP_NANO
  sethostname(hostname, strlen(hostname));
  printf("[ERL] hostname: %s\n", hostname);
#endif

  setup_etc(rootdir);

#ifndef GRISP_PLATFORM_GRISP_NANO
  if (start_dhcp) {
    printf("[ERL] Starting DHCP\n");
    grisp_led_set1(false, true, true);
    grisp_init_dhcpcd(PRIO_DHCP);
  }

  if (wlan_enable) {
    printf("[ERL] Initializing WLAN\n");
    grisp_led_set1(true, true, false);
    rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(4000));
    if (start_dhcp) {
      if (wpa_supplicant_conf != NULL) {
        printf("[ERL] WLAN mode: WPA\n");
        grisp_led_set1(true, false, true);
        grisp_init_wpa_supplicant(wpa_supplicant_conf, PRIO_WPA,
                                  create_wlandev);
      } else {
        printf("[ERL] WLAN mode: unsecured (!)\n");
        create_wlandev();
      }
    } else if (wlan_adhocmode) {
      printf("[ERL] WLAN mode: ad-hoc\n");
      create_wlandev_adhoc();
    }
  }
  grisp_led_set1(false, true, false);
#endif

  printf("[ERL] mkdir /tmp\n");
  rv = mkdir("/tmp", 0755);
  assert(rv == 0);

  printf("[ERL] mkdir /tmp/log\n");
  rv = mkdir("/tmp/log", 0755);
  assert(rv == 0);

  printf("[ERL] mkdir /home\n");
  rv = mkdir("/home", 0755);
  assert(rv == 0);

  printf("[ERL] Setting environment\n");
  setenv("BINDIR", "otp/lib/erlang/bin", 1);
  setenv("ROOTDIR", "otp", 1);
  setenv("PROGNAME", "erl.rtems", 1);
  setenv("HOME", "/home", 1);

  /* Need to change the directory here because some dunderheaded
     library changes it back to root otherwise */

  printf("[ERL] chdir(%s)\n", rootdir);
  rv = chdir(rootdir);
  if (rv < 0)
    perror("can't chdir");

  p = getcwd(pwd, 1024);
  if (p == NULL)
    printf("[ERL] getcwd error\n");
  else
    printf("[ERL] getcwd: %s\n", p);

  printf("[ERL] Heap information:\n");
  static_print_rtems_memory_info();

  printf("[ERL] Starting BEAM\n");

  // Erlang shell
  if (shell == 0) {
    printf("[ERL] Erlang shell mode\n");
    erl_start(argc, argv);

  // RTEMS shell
  } else if (shell == 1) {
#ifndef GRISP_PLATFORM_GRISP_NANO
    grisp_led_set2(false, true, false);
#endif
    printf("[ERL] Starting RTEMS shell\n");

    sc = rtems_shell_init("SHLL"  /* task name */
        , SHELL_STACK_SIZE        /* task stack size */
        , 10                      /* task priority */
        , CONSOLE_DEVICE_NAME     /* device name */
        , true                    /* run forever */
        , false                   /* wait for shell to terminate */
        , NULL);                  /* login check function,
                                   use NULL to disable a login check */
    assert(sc == RTEMS_SUCCESSFUL);
    erl_start(argc, argv);

  // No shell
  } else if (shell == 2) {
    printf("[ERL] No shell mode\n");
    erl_start(argc, argv);
  }

  printf("\n\n[ERL] BEAM terminated\n");
  sleep(2);
  exit(0);
}

/*
 * Configure LibBSD.
 */
#include <grisp/libbsd-nexus-config.h>

#ifdef GRISP_PLATFORM_GRISP_NANO
#define RTEMS_BSD_CONFIG_DOMAIN_PAGE_MBUFS_SIZE (1 * 1024 * 1024)
#endif

#define RTEMS_BSD_CONFIG_TERMIOS_KQUEUE_AND_POLL
#define RTEMS_BSD_CONFIG_INIT

#include <machine/rtems-bsd-config.h>

/*
 * Configure RTEMS.
 */
#define CONFIGURE_MICROSECONDS_PER_TICK 1000

#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_STUB_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_ZERO_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK

#define CONFIGURE_FILESYSTEM_DOSFS
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 64
/* increase max file size in IMFS to 64MB */
#define CONFIGURE_IMFS_MEMFILE_BYTES_PER_BLOCK 256

#define CONFIGURE_UNLIMITED_OBJECTS
#define CONFIGURE_UNIFIED_WORK_AREAS
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 8

#define CONFIGURE_INIT_TASK_STACK_SIZE STACK_SIZE_INIT_TASK
#define CONFIGURE_INIT_TASK_INITIAL_MODES RTEMS_DEFAULT_MODES
#define CONFIGURE_INIT_TASK_ATTRIBUTES RTEMS_FLOATING_POINT
#define CONFIGURE_INIT_TASK_PRIORITY 10

#ifdef GRISP_PLATFORM_GRISP_NANO
#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (16 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 2
#define CONFIGURE_BDBUF_MAX_WRITE_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (512 * 1024)
#else
#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (32 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (1 * 1024 * 1024)
#endif
#define CONFIGURE_BDBUF_READ_AHEAD_TASK_PRIORITY 97
#define CONFIGURE_SWAPOUT_TASK_PRIORITY 97

#define CONFIGURE_STACK_CHECKER_ENABLED

#define CONFIGURE_INITIAL_EXTENSIONS                                           \
  { .fatal = fatal_extension }

#define CONFIGURE_MALLOC_DIRTY

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE

#define CONFIGURE_MINIMUM_TASK_STACK_SIZE (64 * 1024)

#define CONFIGURE_INIT

#include <rtems/confdefs.h>

/*
 * Configure Shell.
 */
#include <rtems/netcmds-config.h>
#include <bsp/irq-info.h>
#define CONFIGURE_SHELL_COMMANDS_INIT

/* Disable the commands that do not work */
#define CONFIGURE_SHELL_NO_COMMAND_RTC
#define CONFIGURE_SHELL_NO_COMMAND_MDUMP
#define CONFIGURE_SHELL_NO_COMMAND_WDUMP
#define CONFIGURE_SHELL_NO_COMMAND_LDUMP
#define CONFIGURE_SHELL_NO_COMMAND_MEDIT
#define CONFIGURE_SHELL_NO_COMMAND_MFILL
#define CONFIGURE_SHELL_NO_COMMAND_MMOVE

#ifdef GRISP_PLATFORM_GRISP_NANO
  #define GRISP_NET_CMDS
#else
  #define GRISP_NET_CMDS \
    &rtems_shell_ARP_Command, \
    &rtems_shell_PFCTL_Command, \
    &rtems_shell_PING_Command, \
    &rtems_shell_IFCONFIG_Command, \
    &rtems_shell_ROUTE_Command, \
    &rtems_shell_NETSTAT_Command, \
    &rtems_shell_DHCPCD_Command, \
    &rtems_shell_HOSTNAME_Command, \
    &rtems_shell_WLANSTATS_Command, \
    &rtems_shell_STARTFTP_Command,
#endif

#define CONFIGURE_SHELL_USER_COMMANDS \
  &bsp_interrupt_shell_command, \
  GRISP_NET_CMDS \
  &rtems_shell_SYSCTL_Command, \
  &rtems_shell_VMSTAT_Command, \
  &rtems_shell_BLKSTATS_Command

#define CONFIGURE_SHELL_COMMANDS_ALL

#include <rtems/shellconfig.h>

#else
int main(int argc, char **argv) {
  erl_start(argc, argv);
  return 0;
}
#endif
