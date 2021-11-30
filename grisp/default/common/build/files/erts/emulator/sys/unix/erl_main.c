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
#include <rtems/ftpd.h>
#include <rtems/libio.h>
#include <rtems/malloc.h>
#include <rtems/media.h>
#include <rtems/score/armv7m.h>
#include <rtems/shell.h>
#include <rtems/stringto.h>

#include <assert.h>
#include <bsp.h>
#include <sys/mman.h>
#include <sysexits.h>
#include <unistd.h>

#include <inih/ini.h>

#include <grisp/eeprom.h>
#include <grisp/init.h>
#include <grisp/led.h>

#define MNT "/media/mmcsd-0-0/"
#define INI_FILE (MNT "grisp.ini")
#define DHCP_CONF_FILE (MNT "dhcpcd.conf")

#define STACK_SIZE_INIT_TASK (64 * 1024)

#define PRIO_DHCP (RTEMS_MAXIMUM_PRIORITY - 1)
#define PRIO_WPA (RTEMS_MAXIMUM_PRIORITY - 1)

void parse_args(char *args);

static int start_dhcp = 0;
static int wlan_adhocmode = 0;
static int wlan_enable = 0;

static char *ip_self = "";
static char *wlan_ip_netmask = "";

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
static char *wlan_mode = "infrastructure";

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
static char *wlan_channel = "6";

static char *wlan_adhocname = "adhocnetwork";

static char *hostname = "grisp";

static char *wpa_supplicant_conf = NULL;

static char *erl_args;
static const char *default_erl_args = "erl.rtems -- "
                                      "-root otp "
                                      "-home home "
                                      "-boot start_sasl "
                                      "-pa .";

#define MAX_ARGC 256

static char *argv[MAX_ARGC];
static int argc;

static char *strdupcat(char *s1, char *s2) {
  char *res;

  res = malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(res, s1);
  strcat(res, s2);

  return res;
}

void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset) {
  errno = ENODEV;
  return MAP_FAILED;
}

int munmap(void *addr, size_t len) {
  errno = EINVAL;
  return -1;
}

void fatal_extension(uint32_t source, uint32_t is_internal, uint32_t error) {
  printk("\n\nfatal extension: source=%ld, is_internal=%ld, error=%ld\n",
         source, is_internal, error);
  if (source == RTEMS_FATAL_SOURCE_EXCEPTION)
    rtems_exception_frame_print((const rtems_exception_frame *)error);

  /* rtems_stack_checker_report_usage(); */
  while (1) {
  }
}

void fatal_atexit(void) {
  printk("Erlang VM exited\n");
  /* rtems_stack_checker_report_usage(); */
  while (1) {
  }
}

static int ini_file_handler(void *arg, const char *section, const char *name,
                            const char *value) {
  int ok = 0;

  printf("grisp.ini: "
         "section \"%s\", name \"%s\", value \"%s\"\n",
         section, name, value);
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
      wpa_supplicant_conf = strdupcat(MNT, value);
      ok = 1;
    }
  } else if (strcmp(section, "erlang") == 0) {
    if (strcmp(name, "args") == 0) {
      printf("erl args: "
             "section \"%s\", name \"%s\", value \"%s\"\n",
             section, name, value);
      erl_args = strdup(value);
      ok = 1;
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

static void evaluate_ini_file(const char *ini_file) {
  int rv;

  rv = ini_parse(ini_file, ini_file_handler, NULL);
  if (rv == -1) {
    printf("[ERL] WARNING: %s not found, using defaults\n", ini_file);
  }
}

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

static void Init(rtems_task_argument arg) {
  printf("[ERL] Initializing\n");
  rtems_status_code sc = RTEMS_SUCCESSFUL;
  int rv = 0;
  static char pwd[1024];
  char *p;
  struct grisp_eeprom eeprom = {0};

  atexit(fatal_atexit);

  grisp_led_set1(true, true, true);
  grisp_led_set2(false, false, false);

  printf("[ERL] Initializing buses\n");
  grisp_init_buses();

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

  printf("[ERL] Mounting SD card asynchronously\n");
  grisp_init_sd_card();
  printf("[ERL] Lowering self priority\n");
  grisp_init_lower_self_prio();
  printf("[ERL] Initializing libbsd\n");
  grisp_init_libbsd();
  printf("[ERL] Running ifconfig on lo0\n");
  default_network_ifconfig_lo0();

  /* Wait for the SD card */
  grisp_led_set1(true, false, true);
  sc = grisp_init_wait_for_sd();
  if (sc == RTEMS_SUCCESSFUL) {
    printf("[ERL] SD card mounted\n");
  } else {
    printf("[ERL] ERROR: SD card could not be mounted after timeout\n");
    grisp_led_set1(true, false, false);
  }

  printf("[ERL] Reading %s", INI_FILE);
  erl_args = strdup(default_erl_args);
  evaluate_ini_file(INI_FILE);
  printf("[ERL] Booting with arg: %s\n", erl_args);
  parse_args(erl_args);

  sethostname(hostname, strlen(hostname));
  printf("[ERL] hostname: %s\n", hostname);

  if (start_dhcp) {
    printf("[ERL] Starting DHCP\n");
    grisp_led_set1(false, true, true);
    if (!access(DHCP_CONF_FILE, F_OK))
      grisp_init_dhcpcd_with_config(PRIO_DHCP, DHCP_CONF_FILE);
    else
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

  printf("[ERL] chdir(%s)\n", MNT);
  rv = chdir(MNT);
  if (rv < 0)
    perror("can't chdir");

  p = getcwd(pwd, 1024);
  if (p == NULL)
    printf("[ERL] getcwd error\n");
  else
    printf("[ERL] getcwd: %s\n", p);

  printf("[ERL] Starting BEAM\n");
  erl_start(argc, argv);
  printf("[ERL] BEAM exited\n");
  sleep(2);
  exit(0);
}

/*
 * Configure LibBSD.
 */
#include <grisp/libbsd-nexus-config.h>

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

#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (32 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (1 * 1024 * 1024)
#define CONFIGURE_BDBUF_READ_AHEAD_TASK_PRIORITY 97
#define CONFIGURE_SWAPOUT_TASK_PRIORITY 97

#define CONFIGURE_STACK_CHECKER_ENABLED

#define CONFIGURE_INITIAL_EXTENSIONS                                           \
  { .fatal = fatal_extension }

#define CONFIGURE_MALLOC_DIRTY

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE

#define CONFIGURE_INIT_TASK_STACK_SIZE (512 * 1024)
#define CONFIGURE_INIT_TASK_PRIORITY 10

#define CONFIGURE_MINIMUM_TASK_STACK_SIZE (64 * 1024)

#define CONFIGURE_INIT

#include <rtems/confdefs.h>

#else
int main(int argc, char **argv) {
  erl_start(argc, argv);
  return 0;
}
#endif
