#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <errno.h>
#include <string.h>

#include <bsp/pin-config.h>

#include <erl_nif.h>

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_gpio_init_failed;
static ERL_NIF_TERM am_index;
static ERL_NIF_TERM am_input;
static ERL_NIF_TERM am_invalid_attributes;
static ERL_NIF_TERM am_invalid_direction;
static ERL_NIF_TERM am_invalid_index;
static ERL_NIF_TERM am_invalid_mode;
static ERL_NIF_TERM am_invalid_pin;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_not_null_terminated;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_output;

#define RAISE_TERM(type, term)                                                 \
  enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg)                                                    \
  RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static ErlNifResourceType *gpio_pin_rt;

typedef struct gpio_pin {
  int index;
} gpio_pin;

#define N_PINS (sizeof(pins) / sizeof(pins[0]))

static Pin pins[] = {
    /* 0: gpio1 pin1 */
    {PIO_PC12, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},
    /* 1: gpio1 pin2 */
    {PIO_PC13, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},
    /* 2: gpio1 pin3 */
    {PIO_PA21, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},
    /* 3: gpio1 pin4 */
    {PIO_PD30, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},
    /* 4: gpio2 pin1 */
    {PIO_PD0, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},
    /* 5: gpio2 pin2 */
    {PIO_PD1, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},
    /* 6: gpio2 pin3 */
    {PIO_PD2, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},
    /* 7: gpio2 pin4 */
    {PIO_PD3, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},
    /* 8:  led1 r */
    {PIO_PA24, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 9:  led1 g */
    {PIO_PA17, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 10: led1 b */
    {PIO_PA23, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 11: led2 r */
    {PIO_PA13, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 12  led2 g */
    {PIO_PA5, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 13: led2 b */
    {PIO_PA11, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT},
    /* 14: jumper 1 */
    {PIO_PA6, PIOA, ID_PIOA, PIO_INPUT, PIO_PULLUP},
    /* 15: jumper 2 */
    {PIO_PD9, PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},
    /* 16: jumper 3 */
    {PIO_PC17, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},
    /* 17: jumper 4 */
    {PIO_PD11, PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},
    /* 18: jumper 5 */
    {PIO_PC10, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},
    /* 19: spi1 pin 7 (interrupt) */
    {PIO_PA2, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},
    /* 20: spi1 pin 8 (reset) */
    {PIO_PC9, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},
    /* 21: spi1 pin 9 */
    {PIO_PA9, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},
    /* 22: spi1 pin 10 */
    {PIO_PA10, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},
    /* 23: SS1 */
    {PIO_PD12C_SPI0_NPCS2, PIOD, ID_PIOD, PIO_PERIPH_C, PIO_DEFAULT},
    /* 24: SS2 */
    {PIO_PD27B_SPI0_NPCS3, PIOD, ID_PIOD, PIO_PERIPH_B, PIO_DEFAULT},
    /* 25: uart_1_cts */
    {PIO_PB2C_CTS0, PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT},
    /* 26: uart_2_txd */
    {PIO_PB1C_TXD0, PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT},
    /* 27: uart_3_rxd */
    {PIO_PB0C_RXD0, PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT},
    /* 28: uart_4_rts */
    {PIO_PB3C_RTS0, PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT}};

static int gpio_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  gpio_pin_rt = enif_open_resource_type(env, NULL, "gpio_pin", NULL,
                                        ERL_NIF_RT_CREATE, NULL);
  assert(gpio_pin_rt != NULL);

  am_error = enif_make_atom(env, "error");
  am_gpio_init_failed = enif_make_atom(env, "gpio_init_failed");
  am_index = enif_make_atom(env, "index");
  am_input = enif_make_atom(env, "input");
  am_invalid_attributes = enif_make_atom(env, "invalid_attributes");
  am_invalid_direction = enif_make_atom(env, "invalid_direction");
  am_invalid_index = enif_make_atom(env, "invalid_index");
  am_invalid_mode = enif_make_atom(env, "invalid_mode");
  am_invalid_pin = enif_make_atom(env, "invalid_pin");
  am_invalid_value = enif_make_atom(env, "invalid_value");
  am_not_null_terminated = enif_make_atom(env, "not_null_terminated");
  am_ok = enif_make_atom(env, "ok");
  am_output = enif_make_atom(env, "output");

  return 0;
}

static ERL_NIF_TERM gpio_open_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  gpio_pin *pin;
  ERL_NIF_TERM resource;
  ERL_NIF_TERM *mode, *t_index;
  int arity = -1;
  int index = -1;
  int value;

  // Parse attributes: integer()
  if (!enif_get_map_value(env, argv[0], am_index, &t_index)) {
    return RAISE_TERM(am_invalid_attributes, argv[0]);
  }
  if (!enif_get_int(env, t_index, &index)) {
    return RAISE_TERM(am_invalid_index, t_index);
  }

  // Parse mode: input | {output, 0 | 1}
  if (enif_is_identical(argv[1], am_input)) {
    pins[index].type = PIO_INPUT;
  } else if (enif_get_tuple(env, argv[1], &arity, &mode) && arity == 2) {
    if (!enif_is_identical(mode[0], am_output)) {
      return RAISE_TERM(am_invalid_mode, argv[1]);
    }
    if (!enif_get_int(env, mode[1], &value) || value < 0 || value > 1) {
      return RAISE_TERM(am_invalid_value, argv[1]);
    }
    if (value == 0) {
      pins[index].type = PIO_OUTPUT_0;
    } else if (value == 1) {
      pins[index].type = PIO_OUTPUT_1;
    }
  } else {
    return RAISE_TERM(am_invalid_mode, argv[1]);
  }

  // Initialize
  pin = enif_alloc_resource(gpio_pin_rt, sizeof(*pin));
  pin->index = index;

  if (!PIO_Configure(pins + pin->index, 1)) {
    return RAISE_STRERROR(am_gpio_init_failed);
  }

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

  if (value == 1) {
    PIO_Set(pins + pin->index);
  } else if (value == 0) {
    PIO_Clear(pins + pin->index);
  } else {
    return RAISE_TERM(am_invalid_value, argv[1]);
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

  value = PIO_Get(pins + pin->index);

  return enif_make_uint(env, value);
}

static ErlNifFunc nif_funcs[] = {{"gpio_open_nif", 2, &gpio_open_nif},
                                 {"gpio_set_nif", 2, &gpio_set_nif},
                                 {"gpio_get_nif", 1, &gpio_get_nif}};

ERL_NIF_INIT(grisp_gpio, nif_funcs, &gpio_load, NULL, NULL, NULL)
