#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <bsp/fdt.h>
#include <bsp/imx-gpio.h>
#include <errno.h>
#include <libfdt.h>
#include <pthread.h>

#include <erl_nif.h>

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_gpio_init_failed;
static ERL_NIF_TERM am_invalid_path;
static ERL_NIF_TERM am_invalid_pin;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_not_null_terminated;
static ERL_NIF_TERM am_ok;

#define RAISE_TERM(type, term)                                                 \
  enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg)                                                    \
  RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static ErlNifResourceType *gpio_pin_rt;

typedef struct gpio_pin {
  struct imx_gpio_pin imx;
} gpio_pin;

static int gpio_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  gpio_pin_rt = enif_open_resource_type(env, NULL, "gpio_pin", NULL,
                                        ERL_NIF_RT_CREATE, NULL);
  assert(gpio_pin_rt != NULL);

  am_error = enif_make_atom(env, "error");
  am_gpio_init_failed = enif_make_atom(env, "gpio_init_failed");
  am_invalid_path = enif_make_atom(env, "invalid_path");
  am_invalid_pin = enif_make_atom(env, "invalid_pin");
  am_invalid_value = enif_make_atom(env, "invalid_value");
  am_not_null_terminated = enif_make_atom(env, "not_null_terminated");
  am_ok = enif_make_atom(env, "ok");

  return 0;
}

static ERL_NIF_TERM gpio_open_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  const void *fdt;
  ErlNifBinary path;
  int node;
  gpio_pin *pin;
  ERL_NIF_TERM resource;

  pin = enif_alloc_resource(gpio_pin_rt, sizeof(*pin));

  fdt = bsp_fdt_get();

  if (!enif_inspect_iolist_as_binary(env, argv[0], &path)) {
    return RAISE_TERM(am_invalid_path, argv[0]);
  }

  if (path.data[path.size - 1] != 0) {
    return RAISE_TERM(am_not_null_terminated, argv[0]);
  }

  /* now we know path.data is a \0 terminated string, we only use ASCII so all
   * encodings work */

  node = fdt_path_offset(fdt, (char *)path.data);
  if (imx_gpio_init_from_fdt_property(&(pin->imx), node, "gpios",
                                      IMX_GPIO_MODE_OUTPUT,
                                      0) != RTEMS_SUCCESSFUL) {
    return RAISE_STRERROR(am_gpio_init_failed);
  }
  imx_gpio_set_output(&(pin->imx), 0); // TODO: Remove?

  resource = enif_make_resource(env, pin);
  enif_release_resource(pin);

  return resource;
}

static ERL_NIF_TERM gpio_set_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  int out_value;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }

  if (!enif_get_int(env, argv[1], &out_value)) {
    return RAISE_TERM(am_invalid_value, argv[1]);
  }

  imx_gpio_set_output(&(pin->imx), out_value);

  return am_ok;
}

static ERL_NIF_TERM gpio_get_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  uint32_t in_value;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }

  in_value = imx_gpio_get_input(&(pin->imx));

  return enif_make_uint(env, in_value);
}

static ERL_NIF_TERM gpio_toggle_nif(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }

  imx_gpio_toggle_output(&(pin->imx));

  return am_ok;
}

static ErlNifFunc nif_funcs[] = {{"gpio_open_nif", 1, &gpio_open_nif},
                                 {"gpio_set_nif", 2, &gpio_set_nif},
                                 {"gpio_get_nif", 1, &gpio_get_nif},
                                 {"gpio_toggle_nif", 1, &gpio_toggle_nif}};

ERL_NIF_INIT(grisp_ngpio, nif_funcs, &gpio_load, NULL, NULL, NULL)
