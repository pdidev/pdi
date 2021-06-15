# Changelog for PDI Set-value plugin
All notable changes to the Set-value plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [1.1.0] - 2021-03-02

### Added
* support for new operation types to the plugin:
  - `event` to trigger a named event,
  - `release` to release data
* support to trigger operations on finalization

### Changed
* Shared data are not released by default on finalize


## [1.0.1] - 2021-02-27

### Fixed
* Fixed a bug where the order of set/share/expose operations specified in YAML
  would not be respected.


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
