% @doc GRiSP hardware access.
-module(grisp_hw).

% API
-export([platform/0]).

% Callbacks
-export([init/0]).

-on_load(init/0).

%--- API -----------------------------------------------------------------------

% @doc Return the platform name as an atom.
-spec platform() -> atom().
platform() -> erlang:nif_error("NIF library not loaded").

%--- Callbacks -----------------------------------------------------------------

init() -> ok = erlang:load_nif(atom_to_list(?MODULE), 0).
