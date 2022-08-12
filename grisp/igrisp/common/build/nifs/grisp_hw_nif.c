#define STATIC_ERLANG_NIF 1

#include <erl_nif.h>
#include <grisp.h>

/* NIF interface declarations */
int hw_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

static ERL_NIF_TERM am_platform;

int hw_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  am_platform = enif_make_atom(env, "igrisp");
  return 0;
}

static ERL_NIF_TERM hw_platform_nif(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  return am_platform;
}

static ErlNifFunc nif_funcs[] = {{"hw_platform_nif", 0, hw_platform_nif}};

ERL_NIF_INIT(grisp_hw, nif_funcs, &hw_load, NULL, NULL, NULL)
