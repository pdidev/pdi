# Changelog for PDI MPI plugin
All notable changes to the MPI plugin will be documented in this file.

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
  CMake 3.22 [#613](https://github.com/pdidev/pdi/issues/613)


## [1.8.0] - 2024-11-28

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16
  [#465](https://github.com/pdidev/pdi/issues/465)

### Removed
* Removed support for the PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.5.1] - 2022-04-01

### Fixed
* Fixed an issue where transtyping would fail when the size of the integer used
  for the Fortran size is different from that of a Fortran integer:
  [#293](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/293)


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10.


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)

### Changed
* Communicator transtyping now uses the specific `Comm_c2f` keyworkd for C to
  Fortran transtyping and `Comm_f2c` for Fortran to C transtyping instead of 
  `transtype` for both directions
  [#291](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/291)

### Deprecated
* `transtype` map key in specification tree for communicator transtyping
  [#291](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/291)

### Fixed
* Fixed an issue for `MPI_Comm` transtyping when the memory layout of the C &
  Fortran types were the same:
  [#291](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/291)

### Security


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable

### Fixed
* Workaround issue #291: MPI transtype fails when sizeof(MPI_Comm) == sizeof(MPI_Fint)
