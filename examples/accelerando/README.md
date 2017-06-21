# Accelerometer Demo

## Usage

```erlang
% Configure PmodACL2
application:set_env(grisp, devices, [{spi1, pmod_acl2}]).

application:start(grisp).

% Accelerando
accelerando:run().

exit(A, kill).
```
