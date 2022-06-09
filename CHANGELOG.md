# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Removed redundant command line argument boot debug output

## [2.1.0] - 2022-06-07

### Added

- It is now possible to use UART (lower row) and I2C PMOD ports as GPIO pins
- PmodHB5 driver now supports the new available GPIO pins

## [2.0.0] - 2022-02-01

***Note! This release is not compatible with GRiSP 1 yet!** GRiSP 1
   compatibility will get added in a later patch release.*

### Added

- `grisp_hw` module for hardware access functions
- `grisp_rtems` module containing NIFs for selected RTEMS functions

### Changed

- `grisp_gpio` interface has been updated and simplified
- `grisp_spi` interface has been updated and simplified
- `grisp_i2c` interface has been updated and simplified
- `pmod_gps` now validates the NMEA sentences and parses the GGA ones
- `pmod_gps` API changed, it now returns a pre-parsed tuple, and for the GGA a
  parsed map of the sentence fields

### Removed

- **Breaking change!** The old GPIO API has been removed in favor of a new API
- **Breaking change!** The old SPI API has been removed in favor of a new API
- **Breaking change!** The old I2C API has been removed in favor of a new API

## [1.2.1] - 2020-07-02

### Fixed

- Updated to `grisp_emulation` 0.1.2 with proper PmodGYRO emulation
  ([\#76](https://github.com/grisp/grisp/issues/76))

## [1.2.0] - 2020-03-03

### Added

* Add link to totorial to Pmod NAV doc [\#59](https://github.com/grisp/grisp/issues/59)
* Add documentation [\#71](https://github.com/grisp/grisp/pull/71) ([maehjam](https://github.com/maehjam))
- Add documentation [\#68](https://github.com/grisp/grisp/pull/68) ([maehjam](https://github.com/maehjam))
- Add documentation for PmodNAV [\#67](https://github.com/grisp/grisp/pull/67) ([maehjam](https://github.com/maehjam))

### Fixed

* add\_device fails for PmodHYGRO [\#69](https://github.com/grisp/grisp/issues/69)
- Fix for add\_device, fix of read message, add documentation [\#70](https://github.com/grisp/grisp/pull/70) ([maehjam](https://github.com/maehjam))
- Fix edoc syntax [\#66](https://github.com/grisp/grisp/pull/66) ([nextl00p](https://github.com/nextl00p))

## [1.1.6] - 2019-09-27

### Changed

- Remove deprecated maintainers section [\#65](https://github.com/grisp/grisp/pull/65) ([nextl00p](https://github.com/nextl00p))

## [1.1.5] - 2019-09-27

### Added

* Make it possible to configure UART pins as GPIO [\#37](https://github.com/grisp/grisp/issues/37)
* Implement read command for pmod\_gyro [\#61](https://github.com/grisp/grisp/pull/61) ([GalaxyGorilla](https://github.com/GalaxyGorilla))
* Gps [\#57](https://github.com/grisp/grisp/pull/57) ([aytchell](https://github.com/aytchell))
* Add support for OTP 22 [\#54](https://github.com/grisp/grisp/pull/54) ([sylane](https://github.com/sylane))
* Add missing SPI pins [\#56](https://github.com/grisp/grisp/pull/56) ([Theuns-Botha](https://github.com/Theuns-Botha))
* Examples for grisp\_led:pattern/2 using functions [\#50](https://github.com/grisp/grisp/pull/50) ([Laymer](https://github.com/Laymer))
* Add Feature wireless ad hoc network mode [\#41](https://github.com/grisp/grisp/pull/41) ([Laymer](https://github.com/Laymer))
*  Add Digilent Pmod\_ALS ambient light sensor driver.  [\#40](https://github.com/grisp/grisp/pull/40) ([Laymer](https://github.com/Laymer))
* Add pmod hygro driver [\#31](https://github.com/grisp/grisp/pull/31) ([sebb7](https://github.com/sebb7))

### Fixed

* Edoc doesn't build on master [\#60](https://github.com/grisp/grisp/issues/60)
* grisp\_led:pattern documentation is missing the Fun argument [\#49](https://github.com/grisp/grisp/issues/49)
* Data decoding for pmod\_maxsonar is wrong [\#42](https://github.com/grisp/grisp/issues/42)
* Quickfix for typo [\#64](https://github.com/grisp/grisp/pull/64) ([Laymer](https://github.com/Laymer))
- Quickfix decoding pattern [\#51](https://github.com/grisp/grisp/pull/51) ([Laymer](https://github.com/Laymer))
- Split out emulation layer [\#46](https://github.com/grisp/grisp/pull/46) ([Theuns-Botha](https://github.com/Theuns-Botha))
- Fixed data decoding for 'Digilent PmodMAXSONAR' [\#43](https://github.com/grisp/grisp/pull/43) ([aytchell](https://github.com/aytchell))

## [1.1.4] - 2018-07-30

### Fixed

- PmodNAV magnetometer fails initialization "sometimes" [\#11](https://github.com/grisp/grisp/issues/11)

## [1.1.3] - 2018-07-30

### Changed

* Make embedded mode the default [\#34](https://github.com/grisp/grisp/pull/34) ([nextl00p](https://github.com/nextl00p))

### Fixed

* erlang:get\_stacktrace/0 is deprecated in OTP 21 [\#33](https://github.com/grisp/grisp/issues/33)
* deprecated erlang:get\_stacktrace/0 function [\#36](https://github.com/grisp/grisp/pull/36) ([getong](https://github.com/getong))

## [1.1.2] - 2018-06-21

* Add support for OTP 21.0

## [1.1.1] - 2018-06-06

### Added

* Add support for OTP 21.0-rc1 [\#27](https://github.com/grisp/grisp/pull/27) ([sylane](https://github.com/sylane))
* \[WIP\] Update to support RTEMS 5.0 [\#26](https://github.com/grisp/grisp/pull/26) ([eproxus](https://github.com/eproxus))

### Fixed

* Start Erlang runtime when source dependencies are included [\#25](https://github.com/grisp/grisp/issues/25)
* Writing to a file hangs the system after listing files [\#24](https://github.com/grisp/grisp/issues/24)

## [1.1.0] - 2018-05-24

### Added

* Add support for DHCP configuration file [\#23](https://github.com/grisp/grisp/pull/23) ([sylane](https://github.com/sylane))
* Make onewire driver concurrency safe [\#21](https://github.com/grisp/grisp/pull/21) ([eproxus](https://github.com/eproxus))
* Add version to OTP xcomp file [\#20](https://github.com/grisp/grisp/pull/20) ([sylane](https://github.com/sylane))
* Add board config and OTP cross-compilation config [\#18](https://github.com/grisp/grisp/pull/18) ([sylane](https://github.com/sylane))

## [1.0.1] - 2017-12-19

### Fixed

* Can't compile sample application on Ubuntu 16.04 server [\#17](https://github.com/grisp/grisp/issues/17)
* Ubuntu 16.04 is missing build-essential package as dependency [\#16](https://github.com/grisp/grisp/issues/16)
* Build failing on Mac 10.10.5 [\#15](https://github.com/grisp/grisp/issues/15)

## [1.0.0] - 2017-11-17

### Fixed

* grisp\_spi\_drv is no gen\_server [\#12](https://github.com/grisp/grisp/issues/12)
* Fix display of error message during boot, even when correct hostname … [\#14](https://github.com/grisp/grisp/pull/14) ([nextl00p](https://github.com/nextl00p))

## [0.1.1] - 2017-11-08

### Fixed

* Added missing files to Hex package

## [0.1.0] - 2017-11-06

### Added

* Added emulator instructions in README [\#8](https://github.com/grisp/grisp/pull/8) ([nextl00p](https://github.com/nextl00p))
* Quickcheck: model for 3 type of crashes plus clustering [\#4](https://github.com/grisp/grisp/pull/4) ([ThomasArts](https://github.com/ThomasArts))
* Quickcheck model for LEDs [\#1](https://github.com/grisp/grisp/pull/1) ([ThomasArts](https://github.com/ThomasArts))

### Changed

* Raw refactoring of onewire interface [\#6](https://github.com/grisp/grisp/pull/6) ([ThomasArts](https://github.com/ThomasArts))

### Fixed

* Supervision restart strategies [\#3](https://github.com/grisp/grisp/issues/3)
* grisp\_led is accepting interval 0 \(as well as negative intervals\) [\#2](https://github.com/grisp/grisp/issues/2)
* Fixed wrong registers in rotation vector [\#9](https://github.com/grisp/grisp/pull/9) ([nextl00p](https://github.com/nextl00p))
* Fix for \#2. Negative intervals are now treated by turning off leds [\#5](https://github.com/grisp/grisp/pull/5) ([nextl00p](https://github.com/nextl00p))


[Unreleased]: https://github.com/grisp/grisp/compare/2.1.0...HEAD
[2.1.0]: https://github.com/grisp/grisp/compare/2.0.0...2.1.0
[2.0.0]: https://github.com/grisp/grisp/compare/1.2.1...2.0.0
[1.2.1]: https://github.com/grisp/grisp/compare/1.2.0...1.2.1
[1.2.0]: https://github.com/grisp/grisp/compare/1.1.6...1.2.0
[1.1.6]: https://github.com/grisp/grisp/compare/1.1.5...1.1.6
[1.1.5]: https://github.com/grisp/grisp/compare/1.1.4...1.1.5
[1.1.4]: https://github.com/grisp/grisp/compare/1.1.3...1.1.4
[1.1.3]: https://github.com/grisp/grisp/compare/1.1.2...1.1.3
[1.1.2]: https://github.com/grisp/grisp/compare/1.1.1...1.1.2
[1.1.1]: https://github.com/grisp/grisp/compare/1.1.0...1.1.1
[1.1.0]: https://github.com/grisp/grisp/compare/1.0.1...1.1.0
[1.0.1]: https://github.com/grisp/grisp/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/grisp/grisp/compare/0.1.1...1.0.0
[0.1.1]: https://github.com/grisp/grisp/compare/0.1.0...0.1.1
[0.1.0]: https://github.com/grisp/grisp/compare/09339d122828df2ee9c26338d578519fc084b29b...0.1.0
