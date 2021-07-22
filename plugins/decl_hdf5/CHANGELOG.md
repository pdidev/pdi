# Changelog for PDI Decl'HDF5 plugin
All notable changes to the Decl'HDF5 plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added
* Support specifying a policy when the file or dataset to write to already
  exists [#121](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/121)


### Changed

### Deprecated

### Removed

### Fixed
* Support building tests when building in sequential with no MPI present
  (second try)
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)

### Security


## [1.2.2]

### Fixed
* When building serial decl_hdf5, enabling tests does not require MPI
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)


## [1.2.0]

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
