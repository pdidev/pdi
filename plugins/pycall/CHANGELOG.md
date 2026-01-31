# Changelog for PDI Pycall plugin
All notable changes to the Pycall plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security


## [1.10.0] - 2026-01-31

### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22, pybind11 2.9.1, and Python 3.10.6
  [#613](https://github.com/pdidev/pdi/issues/613)


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
  CMake 3.16, Python 3.8, numpy 1.17, pybind11 2.4
  [#465](https://github.com/pdidev/pdi/issues/465)


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10, and python-3.6.


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
