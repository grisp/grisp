-module(grisp_interrupt).
-include("grisp_docs.hrl").
-include("grisp_nif.hrl").

%API
-export([install_handle/4]).

% Callbacks
-ifndef(DOC).
-on_load(on_load/0).
-endif.

-ifndef(DOC).
on_load() -> ?NIF_LOAD.
-endif.

install_handle(InterruptVector, Pid, StatusRegisterAddress, <<StatusRegisterValue:32/big>>)
    when is_integer(InterruptVector), is_pid(Pid), is_number(StatusRegisterValue) ->
    interrupt_install_handler_nif(InterruptVector, Pid, StatusRegisterAddress, StatusRegisterValue).

-ifdef(TEST).
interrupt_install_handle_nif(_, _, _, _) ->
    ok.
-else.
interrupt_install_handler_nif(InterruptVector, Pid, StatusRegisterAddress, StatusRegisterValue) ->
    ?NIF_STUB([InterruptVector, Pid, StatusRegisterAddress, StatusRegisterValue]).
-endif.
