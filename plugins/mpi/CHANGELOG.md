# Changelog for PDI MPI plugin
All notable changes to the MPI plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Updated dependencies, now require cmake-3.10.

### Deprecated

### Removed

### Fixed

### Security


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
