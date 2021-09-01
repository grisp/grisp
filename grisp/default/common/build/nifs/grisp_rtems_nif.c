#define STATIC_ERLANG_NIF 1
#include <erl_nif.h>

#include <rtems/rtems/clock.h>

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

static ErlNifFunc nif_funcs[] = {
    {"clock_get_ticks_per_second", 0, clock_get_ticks_per_second},
    {"clock_get_ticks_since_boot", 0, clock_get_ticks_since_boot},
    {"clock_get_tod_nif", 0, clock_get_tod},
    {"clock_set_nif", 1, clock_set}};

ERL_NIF_INIT(grisp_rtems, nif_funcs, NULL, NULL, NULL, NULL)
