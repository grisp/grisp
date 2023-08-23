% @doc GRiSP SPI API.
%
% <a href="https://en.wikipedia.org/wiki/Serial_Peripheral_Interface">Serial
% Peripheral Interface (SPI)</a> is a synchronous serial communication
% protocol, where a single controller device can control many responder
% devices. With this API, the GRiSP board acts as a controller and any connected
% device is a responder.
%
% SPI has four lines: clock, MOSI, MISO and chip select. MOSI is the data line
% from the controller device to the responder devices. MISO is the data line
% from the responder devices to the controller device. The controller device is
% regulating the communication speed and synchronization by controlling the
% chip select and clock lines.
%
% == Chip Select ==
%
% By default, if the bus aliases `spi1' or `spi2' is used with {@link open/1}
% the chip select pin is automatically configured to the default SPI slot chip
% select pin. When using {@link open/2} the chip select pin can be any GPIO pin
% (see {@link grisp_gpio}).
%
% == Clock ==
%
% The clock line in SPI is a digital I/O line that pulses at a certain
% frequency. The default SPI clock frequency in GRiSP is 0.1 MHz.
%
% The clock line polarity and phase should be configured by setting the
% mandatory `clock' mode option ({@link clock()}) for each message. A polarity
% of `low' means the clock line is idling at `0' and a polarity of `high' means
% the clock line is idling at `1'. The phase denotes at which edge of the clock
% pulse the actual protocol values should be written and read. A phase of
% `leading' means that the controller and responder devices should read or
% write values as the pulse starts. A phase of `trailing' means the devices
% should read or write values as the current pulse ends.
%
% The settings for the clock line is specific per device and needs to be
% consulted from the official specification of the device.
%
% == Request &amp; Response ==
%
% From the controller point of view, requests are always written to the MOSI
% line and response data is always read from the MISO line.
%
% Because SPI is synchronous, for every request byte sent a response byte is
% always received. Many responder devices respond silently with zeroes until a
% request is fully received, then send their response while ignoring the rest
% of the bytes being received. Other devices can start sending data already
% before the controller has finished sending a request, and even reply with
% data while the controller is writing its own data.
%
% This module provides two message formats. One format ({@link message_raw()})
% returns the full response, all bytes received from the responder device
% including ones sent during the initial request.
%
% The other format ({@link message_simple()}) automatically pads the request so
% that response bytes can be received, and strips the response of an initial
% number of bytes. This makes it simpler to communicate with responder devices
% that wait for a fully received request before replying.
-module(grisp_spi).

-include("grisp_nif.hrl").

% API
-export([open/1]).
-export([open/2]).
-export([transfer/2]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

-define(CPOL_HIGH, 1).
-define(CPHA_TRAILING, 2).
-define(CS_DISABLE, 4).

%--- Types ---------------------------------------------------------------------

-type pin() :: {cs, integer()} | {gpio, grisp_gpio:pin()}.
-type bus() :: spi1 | spi2.
% SPI bus identifier.
-type clock() :: {Polarity :: low | high, Phase :: leading | trailing}.
% SPI clock configuration.
-type mode() :: #{clock := clock()}.
% SPI transfer mode.
-type message_raw() :: {Mode::mode(), Message::binary()}.
% A message where the request is unmodified and which will yield a response
% binary of the same size.
-type message_simple() :: {
    Mode::mode(),
    Message::binary(),
    Skip::non_neg_integer(),
    Pad::non_neg_integer()
}.
% A message where the request itself is padded with `Pad' number of `0' bytes,
% and will yield a response binary that is stripped of its first `Skip'
% number of bytes.
-type message() :: message_raw() | message_simple().
% SPI message.
-type response() :: binary().
% SPI response.
-opaque ref() :: {reference(), pin()}.
% Reference to an opened SPI bus.

-export_type([bus/0]).
-export_type([clock/0]).
-export_type([mode/0]).
-export_type([message/0]).
-export_type([response/0]).
-export_type([ref/0]).

%--- API -----------------------------------------------------------------------

% @doc Opens an SPI bus with the default chip select pin.
%
% The respective pin 1 is used as `DefaultPin' for each slot.
%
% @equiv open(Slot, DefaultPin)
% @see grisp_gpio
-spec open(Bus::bus()) -> ref().
open(spi1) -> open(spi1, spi1_pin1);
open(spi2) -> open(spi2, spi2_pin1).

% @doc Opens an SPI bus.
%
% Chip select pin `CS' can be any valid GPIO output pin.
%
% @see grisp_gpio
-spec open(bus(), grisp_gpio:pin()) -> ref().
open(_Bus, CS) -> {open_nif(), pin(grisp_hw:platform(), CS)}.

% @doc Transfers SPI messages on a bus.
%
% A list of responses is returned in the same order as their respective
% messages.
%
% There are two forms of messages, raw binary messages or a simple message with
% padding and skipping.
%
% <ul>
%   <li>
%      {@link message_raw()}<br/>
%      A raw binary message `{Mode, Binary}' sends the data and generates a
%      response of the same length.
%   </li>
%   <li>
%     {@link message_simple()}<br/>
%     A simple message tuple `{Mode, Binary, Skip, Pad}' additionally contains
%     the amount of bytes to skip in the response and an amount of padding
%     bytes to add to the request. The response length will be the length of
%     the request plus the pad length minus the skip length.
%   </li>
% </ul>
%
% See <a href="#Request_&amp;_Response">Request &amp; Response</a> for more
% information.
-spec transfer(ref(), [message()]) -> [response()].
transfer(Ref, Messages) -> [message(Ref, M) || M <- Messages].

%--- Callbacks -----------------------------------------------------------------

% @private
on_load() -> ?NIF_LOAD.

%--- Internal ------------------------------------------------------------------

pin(grisp_base, spi1_pin1) -> {cs, 2};
pin(grisp_base, spi2_pin1) -> {cs, 3};
pin(grisp2, spi1_pin1) -> {cs, 0};
pin(grisp2, spi2_pin1) -> {cs, 1};
pin(grisp2, spi2_pin9) -> {cs, 2};
pin(grisp2, spi2_pin10) -> {cs, 3};
pin(_Platform, Pin) -> {gpio, grisp_gpio:open(Pin, #{mode => {output, 1}})}.

message({Bus, Pin}, {Mode, Message, Skip, Pad}) ->
    chip_select(Pin, mode(Mode), fun(CS, M) ->
        Padding = binary:copy(<<16#ff>>, Pad),
        Request = <<Message/binary, Padding/binary>>,
        Result = ioctl_nif(Bus, CS, M, Request),
        <<_:Skip/binary, Response/binary>> = Result,
        Response
    end);
message({Bus, Pin}, {Mode, Message}) ->
    chip_select(Pin, mode(Mode), fun(CS, M) ->
        ioctl_nif(Bus, CS, M, Message)
    end).

chip_select({cs, Pin}, Mode, Fun) ->
    Fun(Pin, Mode);
chip_select({gpio, Pin}, Mode, Fun) ->
    grisp_gpio:set(Pin, 0),
    try
        case grisp_hw:platform() of
            grisp_base -> Fun(0, Mode);
            grisp2 -> Fun(0, Mode bor ?CS_DISABLE)
        end
    after
        grisp_gpio:set(Pin, 1)
    end.

mode(#{clock := {low, leading}}) -> 0;
mode(#{clock := {low, trailing}}) -> ?CPHA_TRAILING;
mode(#{clock := {high, leading}}) -> ?CPOL_HIGH;
mode(#{clock := {high, trailing}}) -> ?CPOL_HIGH bor ?CPHA_TRAILING.

open_nif() -> ?NIF_STUB([]).

ioctl_nif(Obj, CS, Mode, Msg) -> ?NIF_STUB([Obj, CS, Mode, Msg]).
