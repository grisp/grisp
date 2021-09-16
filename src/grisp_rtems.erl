% @doc GRiSP RTEMS functions.
-module(grisp_rtems).

% API
-export([clock_get_ticks_per_second/0]).
-export([clock_get_ticks_since_boot/0]).
-export([clock_get_tod/0]).
-export([clock_set/1]).

% Callbacks
-export([init/0]).

-on_load(init/0).

%--- Types ---------------------------------------------------------------------

-type time_of_day() :: {calendar:datetime(), Ticks::non_neg_integer()}.
-export_type([time_of_day/0]).

%--- API -----------------------------------------------------------------------

% @doc <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-ticks-per-second">rtems_clock_get_ticks_per_second</a>-spec clock_get_ticks_per_second() -> integer().
-spec clock_get_ticks_per_second() -> integer().
clock_get_ticks_per_second() -> erlang:nif_error("NIF library not loaded").

% @doc <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-ticks-since-boot">rtems_clock_get_ticks_since_boot</a>
-spec clock_get_ticks_since_boot() -> integer().
clock_get_ticks_since_boot() -> erlang:nif_error("NIF library not loaded").

% @doc <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-tod">rtems_clock_get_tod</a>
-spec clock_get_tod() -> time_of_day().
clock_get_tod() ->
    {Year, Month, Day, Hour, Minute, Second, Ticks} = clock_get_tod_nif(),
    {{{Year, Month, Day}, {Hour, Minute, Second}}, Ticks}.

% @doc <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-set">rtems_clock_set</a>
-spec clock_set(time_of_day()) -> integer().
clock_set({{{Year, Month, Day}, {Hour, Minute, Second}}, Ticks}) ->
    clock_set_nif({Year, Month, Day, Hour, Minute, Second, Ticks}).

%--- Callbacks -----------------------------------------------------------------

init() -> ok = erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

clock_set_nif(_TimeOfDay) -> erlang:nif_error("NIF library not loaded").

clock_get_tod_nif() -> erlang:nif_error("NIF library not loaded").
