#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <bsp.h>
#include <erl_nif.h>
#include <errno.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include <dev/i2c/i2c.h>
#include <erl_nif.h>

#include <bsp.h>

/* NIF interface declarations */
int i2c_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
int i2c_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
                ERL_NIF_TERM load_info);

static ERL_NIF_TERM am_bus_open_failed;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_invalid_bus;
static ERL_NIF_TERM am_invalid_message;
static ERL_NIF_TERM am_invalid_message_addr;
static ERL_NIF_TERM am_invalid_message_buf;
static ERL_NIF_TERM am_invalid_message_flags;
static ERL_NIF_TERM am_invalid_message_len;
static ERL_NIF_TERM am_invalid_message_list;
static ERL_NIF_TERM am_invalid_message_type;
static ERL_NIF_TERM am_ioctl_failed;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_read;
static ERL_NIF_TERM am_reverse_failed;
static ERL_NIF_TERM am_write;

#define RAISE_TERM(type, term)                                                 \
  enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg)                                                    \
  RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static ErlNifResourceType *i2c_data_rt;

typedef struct i2c_data {
  int fd;
} i2c_data;

static void i2c_data_dtor(ErlNifEnv *env, void *obj) {
  close(((i2c_data *)obj)->fd);
}

int i2c_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  i2c_data_rt = enif_open_resource_type(env, NULL, "i2c_data",
                                     (ErlNifResourceDtor *)i2c_data_dtor,
                                     ERL_NIF_RT_CREATE, NULL);
  assert(i2c_data_rt != NULL);

  am_bus_open_failed = enif_make_atom(env, "bus_open_failed");
  am_error = enif_make_atom(env, "error");
  am_invalid_bus = enif_make_atom(env, "invalid_bus");
  am_invalid_message = enif_make_atom(env, "invalid_message");
  am_invalid_message_addr = enif_make_atom(env, "invalid_message_addr");
  am_invalid_message_buf = enif_make_atom(env, "invalid_message_buf");
  am_invalid_message_flags = enif_make_atom(env, "invalid_message_flags");
  am_invalid_message_len = enif_make_atom(env, "invalid_message_len");
  am_invalid_message_list = enif_make_atom(env, "invalid_message_list");
  am_invalid_message_type = enif_make_atom(env, "invalid_message_type");
  am_ioctl_failed = enif_make_atom(env, "ioctl_failed");
  am_ok = enif_make_atom(env, "ok");
  am_read = enif_make_atom(env, "read");
  am_reverse_failed = enif_make_atom(env, "reverse_failed");
  am_write = enif_make_atom(env, "write");

  return 0;
}

int i2c_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
                ERL_NIF_TERM load_info) {
  return 0;
}

static ERL_NIF_TERM i2c_open_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret;
  ErlNifBinary bus;
  i2c_data *data;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &bus)) {
    return RAISE_TERM(am_invalid_bus, argv[0]);
  }

  data = enif_alloc_resource(i2c_data_rt, sizeof(i2c_data));

  data->fd = open((char *)bus.data, O_RDWR);

  if (data->fd < 0) {
    return RAISE_STRERROR(am_bus_open_failed);
  }

  ret = enif_make_resource(env, data);
  enif_release_resource(data);
  return ret;
}

static ERL_NIF_TERM i2c_transfer_nif(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  i2c_data *data;
  unsigned int nmsgs;
  int rv = -1;
  ERL_NIF_TERM head, tail, list, resps, rev_resps, resp;
  int arity = -1;
  const ERL_NIF_TERM *elems;
  int i = 0;
  i2c_msg *msgs;
  unsigned int chip_addr, flags, len;
  ErlNifBinary buf;
  uint8_t *readbuf;

  if (!enif_get_resource(env, argv[0], i2c_data_rt, (void **)&data)) {
    return RAISE_TERM(am_invalid_bus, argv[0]);
  }
  if (!enif_get_list_length(env, argv[1], &nmsgs)) {
    return RAISE_TERM(am_invalid_message_list, argv[1]);
  }
  if (nmsgs <= 0) {
    return enif_make_list(env, 0);
  }

  msgs = (i2c_msg *)calloc(nmsgs, sizeof(i2c_msg));
  resps = enif_make_list(env, 0);

  struct i2c_rdwr_ioctl_data payload = {
      .msgs = msgs,
      .nmsgs = nmsgs,
  };

  list = argv[1];
  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_tuple(env, head, &arity, &elems) || arity != 4) {
      return RAISE_TERM(am_invalid_message, head);
    }
    if (!enif_get_uint(env, elems[1], &chip_addr) || chip_addr > UINT16_MAX) {
      return RAISE_TERM(am_invalid_message_addr, head);
    }
    if (!enif_get_uint(env, elems[2], &flags) || flags > UINT16_MAX) {
      return RAISE_TERM(am_invalid_message_flags, head);
    }

    msgs[i].addr = (uint16_t)chip_addr;

    if (enif_is_identical(elems[0], am_read)) {
      // Read
      if (!enif_get_uint(env, elems[3], &len) || len > UINT16_MAX) {
        return RAISE_TERM(am_invalid_message_len, head);
      }
      readbuf = enif_make_new_binary(env, len, &resp);
      msgs[i].flags = (uint16_t)flags | I2C_M_RD;
      msgs[i].len = (uint16_t)len;
      msgs[i].buf = readbuf;
    } else if (enif_is_identical(elems[0], am_write)) {
      // Write
      if (!enif_inspect_iolist_as_binary(env, elems[3], &buf) || buf.size < 0) {
        return RAISE_TERM(am_invalid_message_buf, head);
      }
      msgs[i].flags = (uint16_t)flags & ~I2C_M_RD;
      msgs[i].len = (uint16_t)buf.size;
      msgs[i].buf = buf.data;

      resp = am_ok;
    } else {
      return RAISE_TERM(am_invalid_message_type, head);
    }

    resps = enif_make_list_cell(env, resp, resps);

    list = tail;
    i++;
  }

  rv = ioctl(data->fd, I2C_RDWR, &payload);
  free(msgs);
  if (rv != 0) {
    return enif_make_tuple2(
        env, am_error,
        enif_make_tuple2(
            env, am_ioctl_failed,
            enif_make_string(env, strerror(errno), ERL_NIF_LATIN1)));
  }

  if (!enif_make_reverse_list(env, resps, &rev_resps)) {
    return RAISE_TERM(am_reverse_failed, resps);
  }
  return rev_resps;
}

static ErlNifFunc nif_funcs[] = {
    {"i2c_open_nif", 1, i2c_open_nif},
    {"i2c_transfer_nif", 2, i2c_transfer_nif}};

ERL_NIF_INIT(grisp_i2c, nif_funcs, &i2c_load, NULL, &i2c_upgrade, NULL)
