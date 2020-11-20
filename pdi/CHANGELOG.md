# Changelog for PDI
All notable changes to this project will be documented in this file.

Please note this is the list for the distribution mechanism of PDI. The list for
each sub-project (including PDI itself) is located in the dedicated sub-project
`CHANGELOG.md`.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### For users

#### Added
* Lookup for plugins in paths specified by the `PDI_PLUGIN_PATH` env. variable
* Install plugins as specified by the `INSTALL_PDIPLUGINDIR` cmake variable
* Improved error reporting when loading plugins

#### Changed
* Fortran support now requires version 1.0.6 of Zpp minimum
* Python support now depends on python 3
* Require the CONTIGUOUS attribute for all Fortran arrays (including 1D)

#### Deprecated

#### Removed

#### Fixed
* Added the CONTIGUOUS attribute to all Fortran arrays in tests

#### Security



### For plugin developers

#### Added
* Added expression arithmetic operators
* The `PDI_DEFAULT_PLUGINDIR` cmake variable provides the default plugin installation path
* Subaccess methods for array, record and pointer datatypes

#### Changed

#### Deprecated

#### Removed

#### Fixed

#### Security
