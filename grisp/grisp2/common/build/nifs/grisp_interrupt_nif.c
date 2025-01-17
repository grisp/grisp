#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <rtems.h>
#include <rtems/irq.h>
#include <bsp/irq.h>
#include <erl_nif.h>

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_interrupt;

typedef struct MessageData {
    ErlNifEnv* caller_env;
    ErlNifPid to_pid;
    ERL_NIF_TERM msg;
    uint32_t status_register_address;
    uint32_t status_register_value;
} MessageData;

#define RAISE_TERM(type, term) \
    enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg) \
    RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static int interrupt_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    am_error = enif_make_atom(env, "error");
    am_invalid_value = enif_make_atom(env, "invalid_value");
    am_ok = enif_make_atom(env, "ok");
    am_interrupt = enif_make_atom(env, "interrupt");
    return 0;
}

static ERL_NIF_TERM interrupt_handler(uint32_t interrupt_vector, MessageData msg) {

    *(uint32_t *)(uintptr_t)msg.status_register_address = msg.status_register_value;
    enif_send(msg.caller_env, &msg.to_pid, NULL, msg.msg);
    return am_ok;
}

static ERL_NIF_TERM interrupt_install_handler_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    rtems_status_code status;
    uint32_t interrupt_vector;
    ErlNifPid pid;
    MessageData message_data;
    uint32_t status_register_address;
    uint32_t status_register_value;
    char isr_name[12];
    if (!enif_get_uint(env, argv[0], &interrupt_vector)) {
        return RAISE_TERM(am_invalid_value, argv[0]);
    }
    if (!enif_get_local_pid(env, argv[1], &pid)) {
        return RAISE_TERM(am_invalid_value, argv[1]);
    }
    if (!enif_get_uint(env, argv[2], &status_register_address)) {
      return RAISE_TERM(am_invalid_value, argv[0]);
    }
    if (!enif_get_int(env, argv[3], &status_register_value)) {
      return RAISE_TERM(am_invalid_value, argv[1]);
    }
    message_data.caller_env = env;
    message_data.to_pid = pid;
    message_data.msg = enif_make_tuple2(env, am_interrupt, enif_make_int(env, interrupt_vector));
    message_data.status_register_address = status_register_address;
    message_data.status_register_value = status_register_value;
    sprintf(isr_name, "ISR IRQ %d", interrupt_vector);

    *(uint32_t *)(uintptr_t)status_register_address = status_register_value;

    status = rtems_interrupt_handler_install(
        interrupt_vector,       // IRQ number
        &isr_name,              // ISR name
        RTEMS_INTERRUPT_SHARED, // ISR mode
        interrupt_handler,      // ISR handler function
        &message_data           // Argument passed to the ISR (optional)
    );

    if (status != RTEMS_SUCCESSFUL) {
        RAISE_TERM(am_invalid_value, argv[1]);
    }
    return am_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"interrupt_install_handler_nif", 4, interrupt_install_handler_nif}
};

ERL_NIF_INIT(grisp_interrupt, nif_funcs, interrupt_load, NULL, NULL, NULL)
