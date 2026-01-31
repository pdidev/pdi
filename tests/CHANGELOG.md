# Changelog for PDI tests project
All notable changes to the tests project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security


## [1.10.0] - 2026-01-31

### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22 [#613](https://github.com/pdidev/pdi/issues/613)


## [1.8.0] - 2024-11-28

### Changed
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16
  - directly use vendored gtest
  [#465](https://github.com/pdidev/pdi/issues/465)


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10.

### Fixed
* Do not run `Test05_C` when compiled without Decl'NetCDF support
  [#401](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/401)


## [1.0.0] - 2021-01-28

### Added
* Added the tests inspired by Parflow that combine Serialize & Decl'HDF5
