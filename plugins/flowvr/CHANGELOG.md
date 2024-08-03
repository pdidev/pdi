# Changelog for PDI FlowVR plugin
All notable changes to the FlowVR plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)

### Deprecated
* The whole FlowVR plugin is now deprecated, unmaintained and unsupported

### Removed

### Fixed

### Security


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.6.0] - 2023-03-08

### Changed
* Remove {fmt} as a direct dependency, use that provided by spdlog to fix
  [#431](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/431)
  (reverts
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413))


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10, and python-3.6.

### Fixed
* Fixed an issue where OpenGL and GLU would not be used correctly if installed
  in a non-standard path
  [#405](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/405)
* Add {fmt} as a direct dependancy to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)


## [1.4.0] - 2021-11-09

### Fixed
* A bug where `sprintf` function got a std::string instead of const char*.
  This led to error when compiling with Clang.
  [#350](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/350)


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
* FlowVR instance name support
* Module name can be an expression

### Changed
* Module is now a default component (module map is optional in yaml)
* Silent abort default value is true
* Logs messages are more clear

### Fixed
* Calling put if module already got abort signal
