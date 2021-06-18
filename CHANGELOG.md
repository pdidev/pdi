# Changelog for the PDI distribution
All notable changes to the PDI distribution will be documented in this file.

Please note this is the list for the distribution mechanism. The list for each
sub-project is located in the sub-project `CHANGELOG.md` file. For example, the
changes for PDI library proper are in [`pdi/CHANGELOG.md`](pdi/CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [1.2.1]

### Fixed
* Fix an issue where embedded HDF5 would not be installed in the correct path
  [#346](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/346)


## [1.2.0]

### Fixed
* Fix an issue where embedded HDF5 would not compile with parallel support
  [#336](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/336)


## [1.0.0] - 2021-01-28

### Added
* A new `tests` sub-project contains tests that depend on multiple plugins

### Changed
* Python support now depends on python 3
* Fortran support now requires python 3.5 with venv support
* Fortran support now requires version 1.0.6 of Zpp minimum
* Bundle version 1.0.6 of Zpp
