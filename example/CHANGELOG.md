# Changelog for PDI example project

All notable changes to the example project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Updated dependencies, now require cmake-3.10, and python-3.6 if enabled.

### Deprecated

### Removed

### Fixed
* Fixed an issue where OpenGL and GLU would not be used correctly if installed
  in a non-standard path
  [#405](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/405)
* Fixed an issue where parallel NetCDF would not be detected correctly with
  NetCDF too old and the wrong tests would be run
  [#383](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/383)

### Security


## [1.3.1] - 2021-08-03

### Fixed
* Initial value of `fti_head` was not defined in Fortran example. This could lead to
  timeout of Fortran example runs.
  [#369](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/369)
