#define STATIC_ERLANG_NIF 1
#include <erl_nif.h>

#include <errno.h>
#include <fcntl.h>
#include <rtems/libio.h>
#include <rtems/rtems/clock.h>
#include <string.h>
#include <unistd.h>

static ERL_NIF_TERM clock_get_ticks_per_second(ErlNifEnv *env, int argc,
                                               const ERL_NIF_TERM argv[]) {
  return enif_make_long(env, rtems_clock_get_ticks_per_second());
}

static ERL_NIF_TERM clock_get_ticks_since_boot(ErlNifEnv *env, int argc,
                                               const ERL_NIF_TERM argv[]) {
  return enif_make_long(env, rtems_clock_get_ticks_since_boot());
}

static ERL_NIF_TERM clock_get_tod(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  rtems_time_of_day now;
  rtems_status_code sc = rtems_clock_get_tod(&now);

  if (sc == RTEMS_SUCCESSFUL) {
    return enif_make_tuple7(
        env, enif_make_uint(env, now.year), enif_make_uint(env, now.month),
        enif_make_uint(env, now.day), enif_make_uint(env, now.hour),
        enif_make_uint(env, now.minute), enif_make_uint(env, now.second),
        enif_make_uint(env, now.ticks));
  } else if (sc == RTEMS_NOT_DEFINED) {
    return enif_raise_exception(env, enif_make_atom(env, "RTEMS_NOT_DEFINED"));
  } else if (sc == RTEMS_INVALID_ADDRESS) {
    return enif_raise_exception(env,
                                enif_make_atom(env, "RTEMS_INVALID_ADDRESS"));
  } else {
    return enif_make_badarg(env);
  }
}

// struct rtems_tod_control {
//     uint32_t year;   /* greater than 1987 */
//     uint32_t month;  /* 1 - 12 */
//     uint32_t day;    /* 1 - 31 */
//     uint32_t hour;   /* 0 - 23 */
//     uint32_t minute; /* 0 - 59 */
//     uint32_t second; /* 0 - 59 */
//     uint32_t ticks;  /* elapsed between seconds */
// };
static ERL_NIF_TERM clock_set(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  int arity = -1;
  const ERL_NIF_TERM *ptr;
  rtems_time_of_day time;
  rtems_status_code sc;

  if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &ptr)) {
    return enif_make_badarg(env);
  }

  if (arity != 7) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_tuple_len"));
  }

  if (!enif_get_uint(env, ptr[0], &time.year)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_year"));
  }
  if (!enif_get_uint(env, ptr[1], &time.month)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_month"));
  }
  if (!enif_get_uint(env, ptr[2], &time.day)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_day"));
  }
  if (!enif_get_uint(env, ptr[3], &time.hour)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_hour"));
  }
  if (!enif_get_uint(env, ptr[4], &time.minute)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_minute"));
  }
  if (!enif_get_uint(env, ptr[5], &time.second)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_second"));
  }
  if (!enif_get_uint(env, ptr[6], &time.ticks)) {
    return enif_raise_exception(env, enif_make_atom(env, "invalid_ticks"));
  }

  sc = rtems_clock_set(&time);

  if (sc == RTEMS_SUCCESSFUL) {
    return enif_make_atom(env, "RTEMS_SUCCESSFUL");
  } else if (sc == RTEMS_INVALID_ADDRESS) {
    return enif_raise_exception(env,
                                enif_make_atom(env, "RTEMS_INVALID_ADDRESS"));
  } else if (sc == RTEMS_INVALID_CLOCK) {
    return enif_raise_exception(env,
                                enif_make_atom(env, "RTEMS_INVALID_CLOCK"));
  } else {
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM unmount_nif(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int status;
  ErlNifBinary path;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &path))
    return enif_make_badarg(env);

  status = unmount((char *)path.data);

  if (status < 0)
    return enif_make_tuple2(
        env, enif_make_atom(env, "error"),
        enif_make_string(env, strerror(errno), ERL_NIF_LATIN1));

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM pwrite_nif(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int fd;
  int size, offset;
  int bytes_written;
  int close_status;
  int err;
  ErlNifBinary device_path, buffer;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &device_path))
    return enif_make_badarg(env);

  if (!enif_inspect_iolist_as_binary(env, argv[1], &buffer))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &offset))
    return enif_make_badarg(env);

  fd = open(device_path.data, O_RDWR);
  if (fd < 0) {
    err = errno;
    return enif_make_tuple3(
        env, enif_make_atom(env, "error"), enif_make_atom(env, "open"),
        enif_make_string(env, strerror(err), ERL_NIF_LATIN1));
  }

  bytes_written = pwrite(fd, buffer.data, buffer.size, offset);
  if (bytes_written < 0) {
    err = errno;
    return enif_make_tuple3(
        env, enif_make_atom(env, "error"), enif_make_atom(env, "pwrite"),
        enif_make_string(env, strerror(err), ERL_NIF_LATIN1));
  }

  close_status = close(fd);
  if (close_status < 0) {
    err = errno;
    return enif_make_tuple3(
        env, enif_make_atom(env, "error"), enif_make_atom(env, "close"),
        enif_make_string(env, strerror(err), ERL_NIF_LATIN1));
  }

  return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                          enif_make_int(env, bytes_written));
}

static ErlNifFunc nif_funcs[] = {
    {"clock_get_ticks_per_second", 0, clock_get_ticks_per_second},
    {"clock_get_ticks_since_boot", 0, clock_get_ticks_since_boot},
    {"clock_get_tod_nif", 0, clock_get_tod},
    {"clock_set_nif", 1, clock_set},
    {"unmount_nif", 1, unmount_nif},
    {"pwrite_nif", 3, pwrite_nif},
};

ERL_NIF_INIT(grisp_rtems, nif_funcs, NULL, NULL, NULL, NULL)
