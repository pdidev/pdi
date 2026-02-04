# Changelog for PDI Set-value plugin
All notable changes to the Set-value plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security


## [1.10.0] - 2026-01-31

### Added
* Added macOS CI [#556](https://github.com/pdidev/pdi/issues/556)

### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22, and spdlog 1.9.2 [#613](https://github.com/pdidev/pdi/issues/613)

### Fixed
* Correct some tests that would incorrectly compare floats with abs instead of
  fabs, rounding down to zero and hence never failing, even in the case of error


## [1.8.1] - 2025-01-23

### Fixed
* Stop claiming to support old cmake versions we do not
  [#507](https://github.com/pdidev/pdi/issues/507)


## [1.8.0] - 2024-11-28

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16, Python 3.8, numpy 1.17
  [#465](https://github.com/pdidev/pdi/issues/465)

### Removed
* Removed support for the PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.5.0] - 2022-03-30

### Added
* Support for changing logger level and pattern during execution.
  [#363](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/363)

### Changed
* Updated dependencies, now require cmake-3.10.


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)


## [1.1.0] - 2021-03-02

### Added
* support for new operation types to the plugin:
  - `event` to trigger a named event,
  - `release` to release data
* support to trigger operations on finalization

### Changed
* Shared data are not released by default on finalize


## [1.0.1] - 2021-02-27

### Fixed
* Fixed a bug where the order of set/share/expose operations specified in YAML
  would not be respected.


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
