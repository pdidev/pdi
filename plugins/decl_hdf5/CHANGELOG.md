# Changelog for PDI Decl'HDF5 plugin
All notable changes to the Decl'HDF5 plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
* Support for reading and writing attributes of datasets & datagroups
* Getting dataset size before reading it

### Changed

### Deprecated

### Removed

### Fixed

### Security


## [1.0.1] - 2021-02-27

### Fixed
* Fixed a bug where a null value for a read/write dataset yaml subtree would
  lead to an error.


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
