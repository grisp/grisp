#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <erl_nif.h>

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_ok;

#define RAISE_TERM(type, term)                                                 \
  enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg)                                                    \
  RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static int pwm_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  am_error = enif_make_atom(env, "error");
  am_invalid_value = enif_make_atom(env, "invalid_value");
  am_ok = enif_make_atom(env, "ok");
  return 0;
}
static ERL_NIF_TERM pwm_get_register32_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  uint32_t address;
  uint32_t value;

  if (!enif_get_uint(env, argv[0], &address)) {
    return RAISE_TERM(am_invalid_value, argv[0]);
  }
  value = *(uint32_t *)(uintptr_t)address;
  return enif_make_uint(env, value);
}

static ERL_NIF_TERM pwm_set_register32_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  uint32_t address;
  uint32_t value;

  if (!enif_get_uint(env, argv[0], &address)) {
    return RAISE_TERM(am_invalid_value, argv[0]);
  }

  if (!enif_get_int(env, argv[1], &value)) {
    return RAISE_TERM(am_invalid_value, argv[1]);
  }
  *(uint32_t *)(uintptr_t)address = value;
  return am_ok;
}

static ErlNifFunc nif_funcs[] = {
                                 {"pwm_get_register32_nif", 1, &pwm_get_register32_nif},
                                 {"pwm_set_register32_nif", 2, &pwm_set_register32_nif},
                                };

ERL_NIF_INIT(grisp_pwm, nif_funcs, &pwm_load, NULL, NULL, NULL)
