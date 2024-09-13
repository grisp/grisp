%% @doc This module is an utility module helping in the creation of remote node

-module(ieee802154_node).

-export([boot_network_node/0, boot_network_node/1, stop_network_node/2]).
-export([boot_ieee802154_node/4, stop_ieee802154_node/2]).
-export([boot_ieee802154_node/5]).
-export([boot_node/1]).
-export([get_project_cwd/0]).

-include_lib("common_test/include/ct.hrl").

-include("ieee802154.hrl").

% -define(ROBOT_REL_DIR, "/_build/default/rel/robot").
-define(LIB_DIR, "/_build/default/lib").

-type mac_address_type() :: mac_short_address | mac_extended_address.
-type mac_address() :: <<_:16>> | <<_:64>>.

%% @equiv boot_network_node(#{}).
-spec boot_network_node() -> node().
boot_network_node() ->
    boot_network_node(#{}).

%% @doc Boot the network simulation node
%% This node is necessary to simulate the real UWB physical network
%% At startup, the mock_phy_network register themselves to the network to receive the tx frames
boot_network_node(Args) ->
    {Pid, Network} = boot_node(network),
    erpc:call(Network, network_simulation, start, [{}, Args]),
    ping_node(network_loop, Network),
    {Pid, Network}.

%% @doc Stops the network node
%% This function stops the network process and then stops the node
-spec stop_network_node(Network, NetPid) -> ok when
      Network :: node(),
      NetPid  :: pid().
stop_network_node(Network, NetPid) ->
    erpc:call(Network, network_simulation, stop, [{}]),
    peer:stop(NetPid).

%% @doc Boots a node and initialize a IEEE 802.15.4 stack inside.
%% the stack will use the mock_phy_network to simulate communications over UWB
%% The network node needs to be started before calling this function
%% The rx callback function used is a placeholder
%% @equiv boot_ieee802154_node(Name, Network, AddressType, Address, fun() -> ok end).
-spec boot_ieee802154_node(Name, Network, AddressType, Address) -> node() when
      Name :: atom(),
      Network :: node(),
      AddressType :: mac_address_type(), 
      Address :: mac_address().
boot_ieee802154_node(Name, Network, AddressType, Address) ->
    boot_ieee802154_node(Name, Network, AddressType, Address, fun(_, _, _, _) -> ok end).

%% @doc Boots a node and initialize a IEEE 802.15.4 stack inside
%% the stack will use the mock_phy_network to simulate communications over UWB
%% The network node needs to be started before calling this function
%% The Callback function is the callback used at the reception of a frame 
%% when the rx loop is used
-spec boot_ieee802154_node(Name, Network, AddressType, Address, Callback) -> {pid(), node()} when
      Name        :: atom(),
      Network     :: node(),
      AddressType :: mac_address_type(), 
      Address     :: mac_address(),
      Callback    :: function().
boot_ieee802154_node(Name, Network, AddressType, Address, Callback) ->
    {Pid, Node} = boot_node(Name),
    erpc:call(Node, mock_phy_network, start, [spi2, #{network => Network}]), % Starting the the mock driver/physical layer
    erpc:call(Node, ieee802154, start, [#ieee_parameters{phy_layer = mock_phy_network, duty_cycle = ieee802154_duty_cycle_non_beacon, input_callback = Callback}]),
    erpc:call(Node, mock_top_layer, start, []),
    erpc:call(Node, ieee802154, set_pib_attribute, [AddressType, Address]),
    {Pid, Node}.

%% @doc Stops a IEEE 802.15.4 node
-spec stop_ieee802154_node(Node, NodePid) -> ok when
      Node    :: node(),
      NodePid :: pid().
stop_ieee802154_node(Node, NodePid) ->
    erpc:call(Node, ieee802154, stop, []),
    peer:stop(NodePid).

%% @private
%% @doc Boots a remote node using the code of the project
-spec boot_node(Name) -> {pid(), node()} when
      Name :: atom().
boot_node(Name) ->
    ProjectCWD = get_project_cwd(),
    %Flags = ["-pa", ProjectCWD ++ ?ROBOT_REL_DIR ++ "/lib/robot-0.1.0/ebin"],
    Flags = ["-pa", ProjectCWD ++ ?LIB_DIR ++ "/grisp/ebin"],
    {ok, Pid, NodeName} = ?CT_PEER(#{name => Name, args => Flags}),
    unlink(Pid),
    {Pid, NodeName}.

%% @private
%% @doc Gets the working directory of the project
-spec get_project_cwd() -> string().
get_project_cwd() -> 
    {ok, Path} = file:get_cwd(),
    filename:dirname(filename:dirname(filename:dirname(filename:dirname(Path)))).

%% @private
%% @doc Pings a remote node and wait for a 'pong' answer
%% This can be used to check if the node has been correctly started
-spec ping_node(ResiteredName, Node) -> ok | error when
      ResiteredName :: atom(),
      Node          :: pid().
ping_node(RegisteredName, Node) ->
    register(ping, self()),
    {RegisteredName, Node} ! {ping, ping, node()},
    receive pong -> ct:pal("Node: ~w says pong", [Node])
    after 2000 -> error(network_node_not_started)
    end,
    unregister(ping).
