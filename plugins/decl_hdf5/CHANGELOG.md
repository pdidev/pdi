# Changelog for PDI Decl'HDF5 plugin
All notable changes to the Decl'HDF5 plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added
* Add more verbose log messages for the `trace` level
* Writing and reading data to/from dataset of different dimensions if number of elements match.

### Changed

### Deprecated

### Removed

### Fixed

### Security


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
