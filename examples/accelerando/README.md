# Accelerometer Demo

## Usage

```erlang-repl
% Configure PmodACL2
1> application:set_env(grisp, devices, [{spi1, pmod_acl2}]).

2> application:start(grisp).

% Accelerando
3> A = accelerando:run().

exit(A, kill).
```
