#define STATIC_ERLANG_NIF 1

#include <bsp/fdt.h>
#include <libfdt.h>
#include <pthread.h>
#include <bsp/imx-gpio.h>

#include <erl_nif.h>

static struct ErlNifResourceType *res_type;

struct gpio_pin
{
  struct imx_gpio_pin imx;
};

static ERL_NIF_TERM pin_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  const void *fdt;
  rtems_status_code sc;
  ErlNifBinary path;
  int node;
  struct gpio_pin *pin;
  ERL_NIF_TERM term;

  pin = enif_alloc_resource(res_type, sizeof(*pin));
  
  fdt = bsp_fdt_get();

  if (!enif_inspect_binary(env, argv[0], &path)) {
    return enif_make_badarg(env);
  }

  if (path.data[path.size - 1] != 0) {
    return enif_make_badarg(env);
  }

  /* now we know path.data is a \0 terminated string, we only use ASCII so all encodings work */

  node = fdt_path_offset(fdt, path.data);
  if (imx_gpio_init_from_fdt_property(&(pin->imx), node, "gpios", IMX_GPIO_MODE_OUTPUT, 0) != RTEMS_SUCCESSFUL) {
    return enif_make_badarg(env);
  }
  imx_gpio_set_output(&(pin->imx), 0);
  
  term = enif_make_resource(env, pin);
  enif_release_resource(pin);

  return term;
}

static ERL_NIF_TERM set_output(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct gpio_pin *pin;
  int out_value;

  if (!enif_get_resource(env, argv[0], res_type, &pin)) {
        return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &out_value)) {
    return enif_make_badarg(env);
  }

  imx_gpio_set_output(&(pin->imx), out_value);

  return enif_make_atom(env, "ok");
}

  
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  res_type = enif_open_resource_type(env, NULL, "gpio_pin", NULL, ERL_NIF_RT_CREATE, NULL);

  if (res_type == NULL)
    return 1;                    /* fail to load */
  else
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
 {"pin_init", 1, &pin_init},
    {"set_output", 2, &set_output}
};

ERL_NIF_INIT(grisp_ngpio, nif_funcs, &load, NULL, NULL, NULL)

