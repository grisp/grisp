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
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/shell.h>
#include <rtems/console.h>
#include <rtems/malloc.h>
#include <machine/rtems-bsd-commands.h>
#include <bsp.h>
#include <assert.h>
#include <rtems/libio.h>
#include <sysexits.h>
#include <sys/mman.h>

#include <inih/ini.h>

#include <grisp/pin-config.h>
#include <grisp/led.h>
#include <grisp/init.h>

#define MNT "/media/mmcsd-0-0/"
#define INI_FILE (MNT "grisp.ini")
#define DHCP_CONF_FILE (MNT "dhcpcd.conf")

#define PRIO_DHCP		(RTEMS_MAXIMUM_PRIORITY - 1)
#define PRIO_WPA		(RTEMS_MAXIMUM_PRIORITY - 1)

void parse_args(char *args);

const Pin atsam_pin_config[] = {GRISP_PIN_CONFIG};
const size_t atsam_pin_config_count = PIO_LISTSIZE(atsam_pin_config);
const uint32_t atsam_matrix_ccfg_sysio = GRISP_MATRIX_CCFG_SYSIO;

static int start_dhcp = 0;
static int wlan_enable = 0;

static char *ip_self = "";

static char *hostname = "defaulthostname";

static char *wpa_supplicant_conf = NULL;

static char *erl_args = "erl.rtems -- -root otp"
    " -home home -boot start_sasl -pa .";

#define MAX_ARGC 256

static char *argv[MAX_ARGC];
static int argc;


static char *strdupcat (char *s1, char *s2)
{
  char *res;

  res = malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(res, s1);
  strcat(res, s2);

  return res;
}

void *
mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset)
{
  errno = ENODEV;
  return MAP_FAILED;
}

int
munmap(void *addr, size_t len)
{
  errno = EINVAL;
  return -1;
}

void
fatal_extension(uint32_t source, uint32_t is_internal, uint32_t error)
{
  printk ("fatal extension: source=%ld, is_internal=%ld, error=%ld\n",
	  source, is_internal, error);
  if (source == RTEMS_FATAL_SOURCE_EXCEPTION)
    rtems_exception_frame_print((const rtems_exception_frame *)error);

  /* rtems_stack_checker_report_usage(); */
  while(1)
    {
    }
}

void
fatal_atexit(void)
{
  printk ("Erlang VM exited\n");
  /* rtems_stack_checker_report_usage(); */
  while(1)
    {
    }
}

static int ini_file_handler(void *arg, const char *section, const char *name,
			    const char *value)
{
  int ok = 0;

  printf ("grisp.ini: "
	  "section \"%s\", name \"%s\", value \"%s\"\n",
	  section, name, value);
  if (strcmp(section, "network") == 0) {
      if (strcmp(name, "hostname") == 0) {
	  hostname = strdup(value);
	  ok = 1;
      }
      else if (strcmp(name, "ip_self") == 0) {
    	  if (strcmp(value, "dhcp") == 0) {
    	      start_dhcp = 1;
    	      ok = 1;
    	  } else {
          // TODO : check IP address format
          ip_self = strdup(value); // Set ip from ini file
          printf("=== Ip is %s ===\n", ip_self);
          ok = 1;
        }
      }
      else if (strcmp(name, "wlan") == 0) {
	  if (strcmp(value, "enable") == 0) {
	      wlan_enable = 1;
	      ok = 1;
	  }
	  else if (strcmp(value, "disable") == 0) {
	      wlan_enable = 0;
	      ok = 1;
	  }
      }
      else if (strcmp(name, "wpa") == 0) {
	wpa_supplicant_conf = strdupcat(MNT, value);
	ok = 1;
      }
  }
  else if (strcmp(section, "erlang") == 0) {
      if (strcmp(name, "args") == 0) {
	  printf ("erl args: "
		  "section \"%s\", name \"%s\", value \"%s\"\n",
		  section, name, value);
	  erl_args = strdup(value);
	  ok = 1;
      }
  }
  else
    ok = 1;

  if (!ok) {
      printf ("erl_main: error in configuration file: "
	      "section \"%s\", name \"%s\", value \"%s\"\n",
	      section, name, value);
      ok = 1;
    }

  return ok;
}

static void evaluate_ini_file(const char *ini_file)
{
    int rv;

    rv = ini_parse(ini_file, ini_file_handler, NULL);
    if (rv == -1) {
	printf("WARNING: Can't find ini file %s -> using defaults\n", ini_file);
    }
}

static void
default_network_ifconfig_lo0(void)
{
   int exit_code;
   char *lo0[] = {
       "ifconfig",
       "lo0",
       "inet",
       "127.0.0.1",
       "netmask",
       "255.255.255.0",
       NULL
   };
   char *lo0_inet6[] = {
       "ifconfig",
       "lo0",
       "inet6",
       "::1",
       "prefixlen",
       "128",
       "alias",
       NULL
   };

   exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(lo0), lo0);
   assert(exit_code == EX_OK);

   exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(lo0_inet6), lo0_inet6);
   assert(exit_code == EX_OK);
}

static void
create_wlandev(void)
{
	int exit_code;
	char *ifcfg[] = {
    "ifconfig",
    "wlan0",
    "create",
    "wlandev",
    "rtwn0",
    "wlanmode",
    "adhoc",
    "channel",
    // "6:ht/40",
    "6",
    "up",
    NULL
	};

  char *ifcfg_adhoc_params[] = {
    "ifconfig",
    "wlan0",
    "inet",
    ip_self,
    "netmask",
    "255.255.0.0",
    "ssid",
    "edge",
    NULL
  };

	exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg), ifcfg);
	if(exit_code != EXIT_SUCCESS) {
		printf("ERROR while creating wlan0.");
	}

	exit_code = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_adhoc_params), ifcfg_adhoc_params);
	if(exit_code != EXIT_SUCCESS) {
		printf("ERROR while setting up edge wlan0.");
	}
}

void parse_args(char *args)
{
    char *p;
    char *last;

    for (p = strtok_r(args, " \t", &last);
	 p;
	 p = strtok_r(NULL, " \t", &last))
    {
	if (argc >= MAX_ARGC) {
	        printf("ERROR: too many erl arguments\n");
		exit(-1);
	}

	argv[argc++] = p;
    }
}

static void Init(rtems_task_argument arg)
{
  rtems_status_code sc = RTEMS_SUCCESSFUL;
  int rv = 0;
  static char pwd[1024];
  char *p;

  atexit(fatal_atexit);

  grisp_led_set1(false, false, false);
  grisp_led_set2(true, true, true);
  printf("mounting sd card\n");
  grisp_init_sd_card();
  grisp_init_lower_self_prio();
  grisp_init_libbsd();
  printf("ifconfig lo0\n");
  default_network_ifconfig_lo0();

  /* Wait for the SD card */
  grisp_led_set2(true, false, true);
  sc = grisp_init_wait_for_sd();
  if(sc == RTEMS_SUCCESSFUL) {
    printf("sd card mounted\n");
  } else {
    printf("ERROR: SD could not be mounted after timeout\n");
    grisp_led_set2(true, false, false);
  }

  evaluate_ini_file(INI_FILE);
  printf("%s\n", erl_args);
  parse_args(erl_args);

  if(start_dhcp) {
      grisp_led_set2(false, true, true);
      grisp_init_dhcpcd_with_config(PRIO_DHCP, DHCP_CONF_FILE);
  }
  if (wlan_enable) {
      grisp_led_set2(false, false, true);
      rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(4000));
      create_wlandev();
  }
  if (wpa_supplicant_conf != NULL) {
    grisp_led_set2(true, false, true);
    grisp_init_wpa_supplicant(wpa_supplicant_conf, PRIO_WPA);
  }
  grisp_led_set2(false, true, false);

  printf("mkdir /tmp\n");
  rv = mkdir("/tmp", 0755);
  assert(rv == 0);

  printf("mkdir /tmp/log\n");
  rv = mkdir("/tmp/log", 0755);
  assert(rv == 0);

  printf("mkdir /home\n");
  rv = mkdir("/home", 0755);
  assert(rv == 0);

  printf("Setting environment\n");
  setenv("BINDIR", "otp/lib/erlang/bin", 1);
  setenv("ROOTDIR", "otp", 1);
  setenv("PROGNAME", "erl.rtems", 1);
  setenv("HOME", "/home", 1);

  /* Need to change the directory here because some dunderheaded
     library changes it back to root otherwise */

  printf("chdir(%s)\n", MNT);
  rv = chdir(MNT);
  if (rv < 0)
    perror("can't chdir");

  printf("\nerl_main: starting ...\n");

  p = getcwd(pwd, 1024);
  if (p == NULL)
    printf("getcwd error\n");
  else
    printf("getcwd: %s\n", p);

  sethostname(hostname, strlen(hostname));
  printf("hostname: %s\n", hostname);

  printf("starting erlang runtime\n");
  erl_start(argc, argv);
  printf("erlang runtime exited\n");
  sleep(2);
  exit(0);
}

/*
 * Configure LibBSD.
 */
#include <grisp/libbsd-nexus-config.h>

#define RTEMS_BSD_CONFIG_INIT
#define RTEMS_BSD_CONFIG_TERMIOS_KQUEUE_AND_POLL

#include <machine/rtems-bsd-config.h>

#define CONFIGURE_MICROSECONDS_PER_TICK 1000

#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_STUB_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_ZERO_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK

#define CONFIGURE_FILESYSTEM_DOSFS

/* increase max file size in IMFS to 64MB */
#define CONFIGURE_IMFS_MEMFILE_BYTES_PER_BLOCK 256

#define CONFIGURE_LIBIO_MAXIMUM_FILE_DESCRIPTORS 64

#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 8

#define CONFIGURE_STACK_CHECKER_ENABLED

#define CONFIGURE_UNLIMITED_OBJECTS
#define CONFIGURE_UNIFIED_WORK_AREAS

#define CONFIGURE_INITIAL_EXTENSIONS { .fatal = fatal_extension }

#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (16 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (1 * 1024 * 1024)

#define CONFIGURE_MALLOC_DIRTY

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE

#define CONFIGURE_INIT_TASK_STACK_SIZE (512 * 1024)
#define CONFIGURE_INIT_TASK_PRIORITY 10

#define CONFIGURE_MINIMUM_TASK_STACK_SIZE (64 * 1024)

#define CONFIGURE_INIT

#include <rtems/confdefs.h>

#else
  int
    main(int argc, char **argv)
  {
    erl_start(argc, argv);
    return 0;
  }
#endif
