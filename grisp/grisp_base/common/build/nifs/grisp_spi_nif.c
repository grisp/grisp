#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <bsp.h>
#include <dev/spi/spi.h>
#include <erl_nif.h>
#include <errno.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <grisp/init.h>

#include "sys.h"

#define CPOL_HIGH 1
#define CPHA_TRAILING 2
#define CS_DISABLE 4

/* Make sure to keep this at sync with the -define(res_max_size.. in spi.erl */
#define RES_MAX_SIZE 256

static ErlNifResourceType *grisp_spi;

typedef struct {
  int fd;
} grisp_spi_state;

static ERL_NIF_TERM raise_err(ErlNifEnv *env, char *reason, int err_num) {
  ERL_NIF_TERM err_atom;
  ERL_NIF_TERM err_str;
  ERL_NIF_TERM err_tuple;
  err_atom = enif_make_atom(env, reason);
  err_str = enif_make_string(env, strerror(err_num), ERL_NIF_LATIN1);
  err_tuple = enif_make_tuple2(env, err_atom, err_str);
  return enif_raise_exception(env, err_tuple);
}

static void grisp_spi_dtor(ErlNifEnv *env, void *obj) {
  close(((grisp_spi_state *)obj)->fd);
}

int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  int rv;
  grisp_spi = enif_open_resource_type(env, NULL, "grisp_spi",
                                      (ErlNifResourceDtor *)grisp_spi_dtor,
                                      ERL_NIF_RT_CREATE, NULL);

  assert(grisp_spi != NULL);

  return 0;
}

static ERL_NIF_TERM spi_open_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret;
  uint32_t speed = 100000;
  int err_num;

  grisp_spi_state *state =
      enif_alloc_resource(grisp_spi, sizeof(grisp_spi_state));

  state->fd = open(GRISP_SPI_DEVICE, O_RDWR);

  if (state->fd == -1) {
    err_num = errno;
    return raise_err(env, "open_spi", err_num);
  }

  if (ioctl(state->fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed) != 0) {
    err_num = errno;
    return raise_err(env, "ioctl", err_num);
  }

  ret = enif_make_resource(env, (void *)state);
  enif_release_resource(state);

  return ret;
}

static ERL_NIF_TERM spi_ioctl_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  int rv;
  int cs;
  int mode;
  int err_num;
  grisp_spi_state *state;
  ErlNifBinary buf;
  ERL_NIF_TERM res;

  struct spi_ioc_transfer msg;

  if (!enif_get_resource(env, argv[0], grisp_spi, (void **)&state))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &cs))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &mode))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[3], &buf))
    return enif_make_badarg(env);

  msg.rx_buf = enif_make_new_binary(env, buf.size, &res);

  msg.speed_hz = 100000;
  msg.delay_usecs = 0;
  msg.bits_per_word = 8;
  msg.cs_change = 1;
  msg.rx_nbits = 0;
  msg.tx_nbits = 0;

  msg.cs = cs;
  msg.mode = ((mode & CPOL_HIGH) ? SPI_CPOL : 0) |
             ((mode & CPHA_TRAILING) ? SPI_CPHA : 0) |
             ((mode & CS_DISABLE) ? SPI_NO_CS : 0);

  msg.tx_buf = buf.data;
  msg.len = buf.size;

  rv = ioctl(state->fd, SPI_IOC_MESSAGE(1), &msg);
  if (rv != 0) {
    err_num = errno;
    return raise_err(env, "ioctl", err_num);
  }
  return res;
}

static ErlNifFunc nif_funcs[] = {{"open_nif", 0, spi_open_nif},
                                 {"ioctl_nif", 4, spi_ioctl_nif}};

ERL_NIF_INIT(grisp_spi, nif_funcs, &load, NULL, NULL, NULL)
