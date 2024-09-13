-module(ieee802154_pib).

-export([init/1]).
-export([get/2]).
-export([set/3]).
-export([reset/1]).

-include("ieee802154_pib.hrl").

%--- API ----------------------------------------------------------------------

init(PhyMod) ->
    {PhyMod, default_attributes()}.

-spec get(State, Attribute) -> Result  when
    State      :: pib_state(),
    Attribute  :: pib_attribute(),
    Result     :: Value | {error, unsupported_attribute},
    Value      :: term().
get({_, Attributes}, Attribute) when is_map_key(Attribute, Attributes) ->
    maps:get(Attribute, Attributes);
get(_, _) ->
    {error, unsupported_attribute}.

-spec set(State, Attribute, Value) -> Results when
    State      :: {PhyMod, Attributes},
    PhyMod     :: module(),
    Attributes :: pib_attributes(),
    Attribute  :: pib_attribute(),
    Value      :: term(),
    Results    :: {ok, NewState} | {error, NewState, Error},
    NewState   :: pib_state(),
    Error      :: pib_set_error().
set({PhyMod, Attributes}, mac_extended_address, Value) ->
    PhyMod:write(eui, #{eui => Value}), % TODO check the range/type/value given
    {ok, {PhyMod, Attributes#{mac_extended_address => Value}}};
set({PhyMod, Attributes}, mac_short_address, Value) ->
    PhyMod:write(panadr, #{short_addr => Value}),
    {ok, {PhyMod, Attributes#{mac_short_address => Value}}};
set({PhyMod, Attributes}, mac_pan_id, Value) ->
    PhyMod:write(panadr, #{pan_id => Value}),
    {ok, {PhyMod, Attributes#{mac_pan_id => Value}}};
set({PhyMod, Attributes}, Attribute, Value)
  when is_map_key(Attribute, Attributes) ->
    NewAttributes = maps:update(Attribute, Value, Attributes),
    {ok, {PhyMod, NewAttributes}};
set(State, _, _) ->
    % TODO detect if PIB is a read only attribute
    {error, State, unsupported_attribute}.

reset({PhyMod, _}) ->
    {PhyMod, default_attributes()}.

%--- Internal ------------------------------------------------------------------
default_attributes() ->
    #{
      cw0 => 2, % cf. p.22 standard
      mac_extended_address => <<16#FFFFFFFF00000000:64>>,
      % mac_max_BE => 8,
      mac_max_BE => 5,
      mac_max_csma_backoffs => 4,
      % mac_min_BE => 5,
      mac_min_BE => 3,
      mac_pan_id => <<16#FFFF:16>>,
      mac_short_address => <<16#FFFF:16>>
     }.