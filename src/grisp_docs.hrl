% Disable new documentation syntax on versions below 27
-if(?OTP_RELEASE >= 27).
-define(moduledoc(Docstring), -moduledoc(Docstring)).
-define(doc(Docstring), -doc(Docstring)).
-else.
-define(moduledoc(Str), -compile([])).
-define(doc(Str), -compile([])).
-endif.
