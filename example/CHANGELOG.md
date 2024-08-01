# Changelog for PDI example project

All notable changes to the example project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)

### Deprecated

### Removed

### Fixed

### Security


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10, and python-3.6 if enabled.

### Fixed
* Fixed an issue where OpenGL and GLU would not be used correctly if installed
  in a non-standard path
  [#405](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/405)
* Fixed an issue where parallel NetCDF would not be detected correctly with
  NetCDF too old and the wrong tests would be run
  [#383](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/383)


## [1.3.1] - 2021-08-03

### Fixed
* Initial value of `fti_head` was not defined in Fortran example. This could lead to
  timeout of Fortran example runs.
  [#369](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/369)
