# GRiSP Erlang Runtime

Create amazing Internet of Things designs without soldering or dropping down to C. The GRiSP project makes building internet-connected hardware devices easier with Erlang!

* Real bare-metal Erlang virtual machine (no operating system!)
* Hard real-time event handling, using open source code
* Digilent Pmod™ compatible connectors for sensors and actuators

This repository contains the Erlang runtime and support code for the GRiSP hardware platform.

## Concepts

A GRiSP board has many slots. Two SPI slots, one UART slot, two GPIO slots and one I<sup>2</sup>C and one 1-Wire slot respectively. It also has two RGB leds and a JTAG connection.

* **Slot** - A physical slot where a component can be connected. E.g. <em>SPI1</em> where a Pmod can be connected.
* **Pmod** - A peripheral device that implements the [Digilent Pmod™ connection form factor and interface][1].
* **SPI** - Serial Peripheral Interface. Synchronous serial communication interface.
* **UART** - Universal Asynchronous Receiver-Transmitter. Asynchronous serial communication interface.
* **GPIO** - General Purpose Input/Output. Digital signal pin interface used to interface with single pins.
* **I<sup>2</sup>C<!-- I2C -->** - Inter-Intergrated Circuit. Short-distance syncronous serial computer bus.
* **1-Wire** - Long-distance serial communication bus.
* **LED Position** - Integer representing one of the two LEDs available on the GRiSP. Either `1` or `2` for the first or second LED.
* **JTAG** - On-chip instrumentation and debugging interface.

# Usage

The easiest way to get started is to use the [Rebar 3][2] or [Mix][3] plug-ins for GRiSP.

## Testing

The project has a [hardware emulation layer in software][4] that allows you to use the runtime locally on a normal computer. To start a local shell for the runtime use:

```
$ rebar3 as test shell
```


[1]: https://reference.digilentinc.com/reference/pmod/specification?redirect=1
[2]: https://github.com/grisp/rebar3_grisp
[3]: https://github.com/grisp/mix_grisp
[4]: https://github.com/grisp/grisp_emulation
