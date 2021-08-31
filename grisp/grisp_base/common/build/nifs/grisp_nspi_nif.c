#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <bsp.h>
#include <bsp/atsam-spi.h>
#include <bsp/spi.h>
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

#include "sys.h"

#define CPOL_LOW 0
#define CPOL_HIGH 1
#define CPHA_LEADING 0
#define CPHA_TRAILING 2

/* Make sure to keep this at sync with the -define(res_max_size.. in spi.erl */
#define RES_MAX_SIZE 256

static ErlNifResourceType *spi_data;

typedef struct {
  int fd;
} grisp_spi_data;

static const atsam_spi_config spi_config = {.spi_peripheral_id = ID_SPI0,
                                            .spi_regs = SPI0};

static void spi_data_dtor(ErlNifEnv *env, void *obj) {
  close(((grisp_spi_data *)obj)->fd);
}

int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  spi_data = enif_open_resource_type(env, NULL, "spi_data",
                                     (ErlNifResourceDtor *)spi_data_dtor,
                                     ERL_NIF_RT_CREATE, NULL);

  assert(spi_data != NULL);

  int rv;

  /* bus registration */
  rv = spi_bus_register_atsam(ATSAM_SPI_0_BUS_PATH, &spi_config);

  if (rv != 0)
    perror("SPI bus registration failed");
  assert(rv == 0);

  return 0;
}

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret;
  uint32_t speed = 100000;

  grisp_spi_data *data = enif_alloc_resource(spi_data, sizeof(grisp_spi_data));

  data->fd = open(ATSAM_SPI_0_BUS_PATH, O_RDWR);

  if (data->fd == -1)
    return enif_raise_exception(env, enif_make_atom(env, "open_spi"));

  if (ioctl(data->fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed) != 0)
    return enif_raise_exception(env, enif_make_atom(env, "ioctl"));

  ret = enif_make_resource(env, data);
  enif_release_resource(data);

  return ret;
}

static ERL_NIF_TERM command_nif(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int rv;
  int cs;
  int mode;
  grisp_spi_data *data;
  ErlNifBinary buf;
  ErlNifBinary res;

  struct spi_ioc_transfer msg;

  if (!enif_get_resource(env, argv[0], spi_data, (void **)&data))
    return enif_raise_exception(env, enif_make_atom(env, "badarg_ref"));
  if (!enif_get_int(env, argv[1], &cs))
    return enif_raise_exception(env, enif_make_atom(env, "badarg_cs"));

  if (!enif_get_int(env, argv[2], &mode))
    return enif_raise_exception(env, enif_make_atom(env, "badarg_mode"));

  if (!enif_inspect_binary(env, argv[3], &buf))
    return enif_raise_exception(env, enif_make_atom(env, "badarg_bin"));

  if (!enif_alloc_binary(buf.size, &res))
    return enif_raise_exception(env, enif_make_atom(env, "alloc_bin"));

  msg.speed_hz = 100000;
  msg.delay_usecs = 1;
  msg.bits_per_word = 8;
  msg.cs_change = 1;
  msg.rx_nbits = 0;
  msg.tx_nbits = 0;

  msg.cs = cs;
  msg.mode = ((mode & CPOL_HIGH) ? SPI_CPOL : 0) |
             ((mode & CPHA_TRAILING) ? SPI_CPHA : 0);

  msg.tx_buf = buf.data;
  msg.rx_buf = res.data;
  msg.len = buf.size;

  rv = ioctl(data->fd, SPI_IOC_MESSAGE(1), &msg);
  if (rv != 0)
    return enif_raise_exception(env, enif_make_atom(env, "ioctl"));

  return enif_make_binary(env, &res);
}

static ErlNifFunc nif_funcs[] = {{"open_nif", 0, open_nif},
                                 {"command_nif", 4, command_nif}};

ERL_NIF_INIT(grisp_nspi, nif_funcs, &load, NULL, NULL, NULL)
