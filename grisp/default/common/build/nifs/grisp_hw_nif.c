#define STATIC_ERLANG_NIF 1

#include <erl_nif.h>

#include <bsp.h>
#if defined LIBBSP_ARM_ATSAM_BSP_H
#define GRISP_PLATFORM "grisp_base"
#elif defined LIBBSP_ARM_IMX_BSP_H
#define GRISP_PLATFORM "grisp2"
#endif

static ERL_NIF_TERM platform_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, GRISP_PLATFORM);
}

static ErlNifFunc nif_funcs[] = {{"platform", 0, platform_nif}};

ERL_NIF_INIT(grisp_hw, nif_funcs, NULL, NULL, NULL, NULL)
