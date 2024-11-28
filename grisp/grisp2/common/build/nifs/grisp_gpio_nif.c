#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <errno.h>
#include <string.h>

#include <bsp/fdt.h>
#include <bsp/imx-gpio.h>
#include <libfdt.h>
#include <pthread.h>


#include <erl_nif.h>


static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_gpio_init_failed;
static ERL_NIF_TERM am_index;
static ERL_NIF_TERM am_input;
static ERL_NIF_TERM am_invalid_attributes;
static ERL_NIF_TERM am_invalid_index;
static ERL_NIF_TERM am_invalid_mode;
static ERL_NIF_TERM am_invalid_path;
static ERL_NIF_TERM am_invalid_pin;
static ERL_NIF_TERM am_fdt_setprop_failed;
static ERL_NIF_TERM am_fdt_config_failed;
static ERL_NIF_TERM am_invalid_pwm;
static ERL_NIF_TERM am_invalid_property;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_not_null_terminated;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_output;
static ERL_NIF_TERM am_path;
static ERL_NIF_TERM am_property;

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
  am_index = enif_make_atom(env, "index");
  am_input = enif_make_atom(env, "input");
  am_invalid_attributes = enif_make_atom(env, "invalid_attributes");
  am_invalid_index = enif_make_atom(env, "invalid_index");
  am_invalid_mode = enif_make_atom(env, "invalid_mode");
  am_invalid_path = enif_make_atom(env, "invalid_path");
  am_invalid_pin = enif_make_atom(env, "invalid_pin");
  am_fdt_setprop_failed = enif_make_atom(env, "fdt_setprop_failed");
  am_fdt_config_failed = enif_make_atom(env, "fdt_config_failed");
  am_invalid_pwm = enif_make_atom(env, "invalid_pwm");
  am_invalid_property = enif_make_atom(env, "invalid_property");
  am_invalid_value = enif_make_atom(env, "invalid_value");
  am_not_null_terminated = enif_make_atom(env, "not_null_terminated");
  am_ok = enif_make_atom(env, "ok");
  am_output = enif_make_atom(env, "output");
  am_path = enif_make_atom(env, "path");
  am_property = enif_make_atom(env, "property");

  return 0;
}

static ERL_NIF_TERM gpio_set_pwm_pin_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM resource;
  ERL_NIF_TERM *t_path, *t_prop, *t_index, *mode;
  int arity = -1;
  ErlNifBinary path, prop;
  int index, node, pwm_id;
  enum imx_gpio_mode gpio_mode;
  gpio_pin *pin;
  const void *fdt;
  struct pincfg {
    uint32_t mux_reg;
    uint32_t padconf_reg;
    uint32_t input_reg;
    uint32_t mux_val;
    uint32_t padconf_val;
    uint32_t input_val;
  };


  // Attributes
  if (!enif_get_map_value(env, argv[0], am_path, &t_path)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_inspect_iolist_as_binary(env, t_path, &path)) {
    return RAISE_TERM(am_invalid_path, t_path);
  }
  if (path.data[path.size - 1] != 0) {
    return RAISE_TERM(am_not_null_terminated, &t_path);
  }

  if (!enif_get_map_value(env, argv[0], am_property, &t_prop)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_inspect_iolist_as_binary(env, t_prop, &prop)) {
    return RAISE_TERM(am_invalid_property, t_prop);
  }
  if (prop.data[prop.size - 1] != 0) {
    return RAISE_TERM(am_not_null_terminated, t_prop);
  }

  if (!enif_get_map_value(env, argv[0], am_index, &t_index)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_get_int(env, t_index, &index)) {
    return RAISE_TERM(am_invalid_index, t_index);
  }

  // PWM ID
  if (!enif_get_int(env, argv[1], &pwm_id)) {
    return RAISE_TERM(am_invalid_value, argv[1]);
  }

  pin = enif_alloc_resource(gpio_pin_rt, sizeof(*pin));
  fdt = bsp_fdt_get();
  node = fdt_path_offset(fdt, (char *)path.data);

  uint32_t pwm_pin_cfg_fdt[6];
  // taken from here: https://github.com/grisp/grisp2-rtems-toolchain/blob/master/external/fdt/sys/gnu/dts/arm/imx6ul-pinfunc.h
  switch (pwm_id) {
    case 1:
      pwm_pin_cfg_fdt[0] = 0x007c;
      pwm_pin_cfg_fdt[1] = 0;
      pwm_pin_cfg_fdt[2] = 0x0000;
      pwm_pin_cfg_fdt[3] = 0;
      pwm_pin_cfg_fdt[4] = 0x0308;
      pwm_pin_cfg_fdt[5] = 0;
      break;
    case 2:
      pwm_pin_cfg_fdt[0] = 0x0080;
      pwm_pin_cfg_fdt[1] = 0;
      pwm_pin_cfg_fdt[2] = 0x0000;
      pwm_pin_cfg_fdt[3] = 0;
      pwm_pin_cfg_fdt[4] = 0x030c;
      pwm_pin_cfg_fdt[5] = 0;
      break;
    case 3:
      pwm_pin_cfg_fdt[0] = 0x006c;
      pwm_pin_cfg_fdt[1] = 1;
      pwm_pin_cfg_fdt[2] = 0x0000;
      pwm_pin_cfg_fdt[3] = 0;
      pwm_pin_cfg_fdt[4] = 0x02f8;
      pwm_pin_cfg_fdt[5] = 0;
      break;
    case 4:
      pwm_pin_cfg_fdt[0] = 0x0070;
      pwm_pin_cfg_fdt[1] = 1;
      pwm_pin_cfg_fdt[2] = 0x0000;
      pwm_pin_cfg_fdt[3] = 0;
      pwm_pin_cfg_fdt[4] = 0x02fc;
      pwm_pin_cfg_fdt[5] = 0;
      break;
    default:
      return RAISE_TERM(am_invalid_pwm, argv[1]);
  }

  // Add the fsl,pins property to the node in the FDT
  int setprop_ret = fdt_setprop(fdt, node, "fsl,pins", pwm_pin_cfg_fdt, sizeof(pwm_pin_cfg_fdt));
  if (setprop_ret != 0) {
    return RAISE_TERM(am_fdt_setprop_failed, [setprop_ret]);
  };

  // Call the configuration function
  int conf_ret = imx_iomux_configure_pins(fdt, node);
  if (conf_ret != 0) {
    return RAISE_TERM(am_fdt_config_failed, [conf_ret]);
  };
  return am_ok;
}



static ERL_NIF_TERM gpio_open_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM resource;
  ERL_NIF_TERM *t_path, *t_prop, *t_index, *mode;
  int arity = -1;
  ErlNifBinary path, prop;
  int index, node, value;
  enum imx_gpio_mode gpio_mode;
  gpio_pin *pin;
  const void *fdt;

  // Attributes
  if (!enif_get_map_value(env, argv[0], am_path, &t_path)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_inspect_iolist_as_binary(env, t_path, &path)) {
    return RAISE_TERM(am_invalid_path, t_path);
  }
  if (path.data[path.size - 1] != 0) {
    return RAISE_TERM(am_not_null_terminated, &t_path);
  }

  if (!enif_get_map_value(env, argv[0], am_property, &t_prop)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_inspect_iolist_as_binary(env, t_prop, &prop)) {
    return RAISE_TERM(am_invalid_property, t_prop);
  }
  if (prop.data[prop.size - 1] != 0) {
    return RAISE_TERM(am_not_null_terminated, t_prop);
  }

  if (!enif_get_map_value(env, argv[0], am_index, &t_index)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_get_int(env, t_index, &index)) {
    return RAISE_TERM(am_invalid_index, t_index);
  }

  // Mode
  if (enif_is_identical(argv[1], am_input)) {
    gpio_mode = IMX_GPIO_MODE_INPUT;
  } else if (enif_get_tuple(env, argv[1], &arity, &mode) && arity == 2) {
    if (!enif_is_identical(mode[0], am_output)) {
      return RAISE_TERM(am_invalid_mode, argv[1]);
    }
    if (!enif_get_int(env, mode[1], &value) || value < 0 || value > 1) {
      return RAISE_TERM(am_invalid_value, argv[1]);
    }
    gpio_mode = IMX_GPIO_MODE_OUTPUT;
  } else {
    return RAISE_TERM(am_invalid_mode, argv[1]);
  }

  // Initialization
  pin = enif_alloc_resource(gpio_pin_rt, sizeof(*pin));
  fdt = bsp_fdt_get();
  node = fdt_path_offset(fdt, (char *)path.data);
  if (imx_gpio_init_from_fdt_property(&(pin->imx), node, (char *)prop.data,
                                      gpio_mode, index) != RTEMS_SUCCESSFUL) {
    return RAISE_STRERROR(am_gpio_init_failed);
  }
  imx_gpio_set_output(&(pin->imx), value);

  resource = enif_make_resource(env, pin);
  enif_release_resource(pin);

  return resource;
}

static ERL_NIF_TERM gpio_set_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  int value;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }

  if (!enif_get_int(env, argv[1], &value)) {
    return RAISE_TERM(am_invalid_value, argv[1]);
  }

  imx_gpio_set_output(&(pin->imx), value);

  return am_ok;
}

static ERL_NIF_TERM gpio_get_register32_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  uint32_t address;
  uint32_t value;

  if (!enif_get_uint(env, argv[0], &address)) {
    return RAISE_TERM(am_invalid_value, argv[0]);
  }
  value = *(uint32_t *)(uintptr_t)address;
  return enif_make_uint(env, value);
}

static ERL_NIF_TERM gpio_set_register32_nif(ErlNifEnv *env, int argc,
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

static void pattern_loop(rtems_task_argument arg) {
  gpio_pin *pin = (gpio_pin *)arg;
  for (int i = 0; i < 10000; i++) {
        imx_gpio_set_output(&(pin->imx), 1);
        imx_gpio_set_output(&(pin->imx), 0);
    }
  rtems_task_delete(RTEMS_SELF);
}

static ERL_NIF_TERM gpio_set_pattern_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  rtems_id task_id;
  rtems_status_code status;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }


    // Create the task
    status = rtems_task_create(
        rtems_build_name('T', 'S', 'K', '1'),
        1,                          // priority
        RTEMS_MINIMUM_STACK_SIZE,    // stack size
        RTEMS_DEFAULT_MODES,
        RTEMS_DEFAULT_ATTRIBUTES,
        &task_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        printf("Pattern task creation failed with status: %d\n", status);
        return;
    }

    // Start the task
    status = rtems_task_start(task_id, pattern_loop, (rtems_task_argument)(uintptr_t)pin);
    if (status != RTEMS_SUCCESSFUL) {
        printf("Pattern task start failed with status: %d\n", status);
    }

  return am_ok;
}

static ERL_NIF_TERM gpio_get_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  uint32_t value;

  if (!enif_get_resource(env, argv[0], gpio_pin_rt, (void **)&pin)) {
    return RAISE_TERM(am_invalid_pin, argv[0]);
  }

  value = imx_gpio_get_input(&(pin->imx));

  return enif_make_uint(env, value);
}

static ErlNifFunc nif_funcs[] = {{"gpio_open_nif", 2, &gpio_open_nif},
                                 {"gpio_set_nif", 2, &gpio_set_nif},
                                 {"gpio_set_pattern_nif", 1, &gpio_set_pattern_nif},
                                 {"gpio_get_register32_nif", 1, &gpio_get_register32_nif},
                                 {"gpio_set_register32_nif", 2, &gpio_set_register32_nif},
                                 {"gpio_set_pwm_pin_nif", 2, &gpio_set_pwm_pin_nif},
                                 {"gpio_get_nif", 1, &gpio_get_nif}};

ERL_NIF_INIT(grisp_gpio, nif_funcs, &gpio_load, NULL, NULL, NULL)
