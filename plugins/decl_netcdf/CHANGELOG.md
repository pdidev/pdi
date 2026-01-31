# Changelog for PDI Decl'NetCDF plugin
All notable changes to the Decl'NetCDF plugin will be documented in this file.

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
  CMake 3.22, and NetCDF 4.8.1 [#613](https://github.com/pdidev/pdi/issues/613)

### Fixed
* Fixed a error incorrectly raised when using the scalar format in yaml write
  configuration [#636](https://github.com/pdidev/pdi/issues/636)


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
  CMake 3.16, NetCDF 4.7
  - directly use vendored gtest
  [#465](https://github.com/pdidev/pdi/issues/465)

### Removed
* Removed support for the PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.6.0] - 2023-03-08

### Changed
* NetCDF 4.6.2 is now required for parallel NetCDF support.
* Remove {fmt} as a direct dependency, use that provided by spdlog to fix
  [#431](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/431)
  (reverts
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413))


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10, hdf5-1.10.0, and netcdf-4.6.0.
* Indent if required by the user, not automatically whenever astyle is found.

### Fixed
* Fixed an issue where parallel NetCDF would not be detected correctly with
  NetCDF too old and the wrong tests would be run
  [#383](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/383)
* Fixed an issue where NetCDF would not be detected
  [#412](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/412)
* Add {fmt} as a direct dependancy to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)


## [1.4.1] - 2021-11-10

### Changed
* updated FindHDF5 to a backport from CMake 3.21.

### Fixed
* fixed a bug where macro definitions imposed by NetCDF would lead to duplicate
  `-D`, hence preventing compilation.


## [1.4.0] - 2021-11-09

### Fixed
* Removed (unsupported by Clang < 11.0) default argument from function parameter
  before parameter pack.
  [#370](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/370)


## [1.3.0] - 2021-07-30

### Added
* Record datatype support
  [#328](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/328)
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)

### Changed

* Refactored tests, testing now depend on cmake 3.10 & gtest
  [#236](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/236)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
