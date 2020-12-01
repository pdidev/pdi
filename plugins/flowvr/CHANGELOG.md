# Changelog for PDI
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
* FlowVR instance name support
* Module name can be an expression

### Changed
* Module is now a default component (module map is optional in yaml)
* Silent abort default value is true
* Logs messages are more clear

### Deprecated

### Removed

### Fixed
* Calling put if module already got abort signal

### Security
