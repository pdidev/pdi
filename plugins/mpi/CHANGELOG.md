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


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable

### Fixed
* Workaround issue #291: MPI transtype fails when sizeof(MPI_Comm) == sizeof(MPI_Fint)
