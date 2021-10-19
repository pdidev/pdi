# Changelog for PDI Test plugin
All notable changes to the Test plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed
* Unnecessary linear increase in heap size. Plugin now uses internal PDI callback
  to know when data is no longer shared.
  [#380](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/380)

### Security


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
