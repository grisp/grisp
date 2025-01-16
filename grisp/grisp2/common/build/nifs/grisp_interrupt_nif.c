#define STATIC_ERLANG_NIF 1

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <erl_nif.h>

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_invalid_value;
static ERL_NIF_TERM am_ok;

typedef struct MessageData {
    ErlNifEnv* caller_env;
    ErlNifPid to_pid;
    ERL_NIF_TERM msg;
} MessageData;

#define RAISE_TERM(type, term) \
    enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg) \
    RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static int interrupt_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    am_error = enif_make_atom(env, "error");
    am_invalid_value = enif_make_atom(env, "invalid_value");
    am_ok = enif_make_atom(env, "ok");
    return 0;
}

static ERL_NIF_TERM interrupt_handler(uint32_t interrupt_vector, MessageData msg) {
    enif_send(msg.caller_env, &msg.to_pid, NULL, msg.msg);
    return am_ok;
}

static ERL_NIF_TERM interrupt_install_handler_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    uint32_t interrupt_vector;
    ErlNifPid pid;
    MessageData message_data;
    if (!enif_get_uint(env, argv[0], &interrupt_vector)) {
        return RAISE_TERM(am_invalid_value, argv[0]);
    }
    if (!enif_get_local_pid(env, argv[1], &pid)) {
        return RAISE_TERM(am_invalid_value, argv[1]);
    }
    message_data.caller_env = env;
    message_data.to_pid = pid;
    message_data.msg = am_ok;
    interrupt_handler(interrupt_vector, message_data);
    return am_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"interrupt_install_handler_nif", 2, interrupt_install_handler_nif}
};

ERL_NIF_INIT(grisp_interrupt, nif_funcs, interrupt_load, NULL, NULL, NULL)
