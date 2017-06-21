# GRiSP Examples

## Basic Example

```erlang
application:start(grisp).

grisp_led:color(1, green).

grisp_led:color(2, red).

grisp_led:flash(1, blue).

Random = fun() -> {random:uniform(2) - 1, random:uniform(2) - 1, random:uniform(2) - 1} end.

grisp_led:pattern(1, [{50, Random}]).
grisp_led:pattern(2, [{50, Random}]).
```
