# Changelog for PDI FlowVR plugin
All notable changes to the FlowVR plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


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
