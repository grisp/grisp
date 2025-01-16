-module(grisp_interrupt).
-include("grisp_docs.hrl").
-include("grisp_nif.hrl").

%API
-export([install_handle/2]).

% Callbacks
-ifndef(DOC).
-on_load(on_load/0).
-endif.

-ifndef(DOC).
on_load() -> ?NIF_LOAD.
-endif.

install_handle(InterruptVector, Pid) when is_integer(InterruptVector), is_pid(Pid) ->
    interrupt_install_handler_nif(InterruptVector, Pid).

-ifdef(TEST).
interrupt_install_handle_nif(_, _) -> ok.
-else.
interrupt_install_handler_nif(InterruptVector, Pid) -> ?NIF_STUB([InterruptVector, Pid]).
-endif.
