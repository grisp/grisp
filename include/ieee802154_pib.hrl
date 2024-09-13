-type pib_state() :: {module(), pib_attributes()}.

-type pib_attribute() :: cw0
                       | mac_extended_address
                       | mac_max_BE
                       | mac_min_BE
                       | mac_max_csma_backoffs
                       | mac_pan_id
                       | mac_short_address.

-type cw0() :: 1 | 2.
-type mac_extended_address() :: <<_:64>>.
-type mac_max_BE() :: 3..8.
-type mac_max_csma_backoff() :: 0..5.
-type mac_min_BE() :: 0..8.
-type mac_pan_id() :: <<_:16>>.
-type mac_short_address() :: <<_:16>>.

-type pib_attributes() :: #{cw0 := cw0(),
                            mac_extended_address := mac_extended_address(),
                            mac_max_BE := mac_max_BE(),
                            mac_min_BE := mac_min_BE(),
                            mac_max_csma_backoffs := mac_max_csma_backoff(),
                            mac_pan_id := mac_pan_id(),
                            mac_short_address := mac_short_address()}.

-type pib_set_error() :: read_only | unsupported_attribute | invalid_parameter.

-export_type([pib_state/0]).