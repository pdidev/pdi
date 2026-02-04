# Changelog for PDI Decl'HDF5 plugin
All notable changes to the Decl'HDF5 plugin will be documented in this file.

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
* Possibility to add a regex for dataset name in datasets section
  [#582](https://github.com/pdidev/pdi/issues/582)
* Fix HDF5 compression test for MacOS 26
  [#627](https://github.com/pdidev/pdi/issues/627)

### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22, and HDF5 1.10.7 [#613](https://github.com/pdidev/pdi/issues/613)

### Fixed
* fix HDF5 API version compatibility issue during error handling
  [#567](https://github.com/pdidev/pdi/issues/567)


## [1.9.0] - 2025-03-07

### Added
* Possibility to choose parallel MPI-I/O mode: either COLLECTIVE or INDEPENDENT
  [#419](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/419)


## [1.8.0] - 2024-11-28

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16
  - directly use vendored gtest & benchmark
  [#465](https://github.com/pdidev/pdi/issues/465)

### Removed
* Removed support for the PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.6.0] - 2023-03-08

### Changed
* Remove {fmt} as a direct dependency, use that provided by spdlog to fix
  [#431](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/431)
  (reverts
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413))

### Fixed
* Correctly delete files when using parallel HDF5 to prevent a crash
  [#428](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/428)


## [1.5.4] - 2022-06-11

### Fixed
* Correctly handle the case where HDF5 was already searched and not found
  [#427](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/427)


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10, and hdf5-1.10.0.

### Fixed
* Add {fmt} as a direct dependancy to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)


## [1.4.3] - 2021-11-30

### Fixed
* Fix an issue where HDF5 installed with cmake would not be correctly detected
  [#356](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/346)


## [1.4.1] - 2021-11-10

### Changed
* updated FindHDF5 to a backport from CMake 3.21.


## [1.3.0] - 2021-07-30

### Added
* Support specifying a policy when the file or dataset to write to already
  exists [#121](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/121)
* Support for dataset chunking, deflate and fletcher32 filters
  [#324](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/324)
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)

### Changed
* Refactored tests, testing now depend on cmake 3.10 & gtest
  [#236](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/236)

### Fixed
* Support building tests when building in sequential with no MPI present
  (second try)
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)
* Fixed compilation with GCC-11
  [#353](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/353)
* Correctly detect parallel HDF5 built with cmake
  [#356](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/356)
* Do not depend on Fortran if testing is not enabled
  [#358](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/358)


## [1.2.2] - 2021-07-06

### Fixed
* When building serial decl_hdf5, enabling tests does not require MPI
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)


## [1.2.0] - 2021-06-15

### Added
* Add more verbose log messages for the `trace` level
  [#306](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/306)
* Support writing and reading data to/from dataset of different dimensions if
  the number of elements matches
  [#308](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/308)

### Fixed
* Correctly include MPI in tests and do not run MPI-based tests when HDF5 is
  built in sequential version
  [#274](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/274)


## [1.1.0] - 2021-03-02

### Added
* Support for reading and writing attributes of datasets & datagroups
* Getting dataset size before reading it


## [1.0.1] - 2021-02-27

### Fixed
* Fixed a bug where a null value for a read/write dataset yaml subtree would
  lead to an error.


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
