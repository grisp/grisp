% @doc Writes 'Hello, World!' to the MTDS display.
%
% Run grisp:add_device(spi2, pmod_mtds) before running this demo.

-module(mtds_hello).
-export([run/0]).

run() ->
    {ok, DS} = pmod_mtds:surface_display(),
    pmod_mtds:color_fg(DS, {0.0, 1.0, 0.0}),
    pmod_mtds:color_bg(DS, {0.0, 0.0, 0.0}),
    pmod_mtds:draw_rop(DS, 12),
    pmod_mtds:font(DS, pmod_mtds:stock_font(console)),
    pmod_mtds:text(DS, "Hello, World!", {2, 2}),
    pmod_mtds:surface_release(DS).
