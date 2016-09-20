# Refactoring
- [x] Rename `params` to `metadata` & `variables` to `data` in config
- [x] Rename the error handling functions & macros in `status.h` to something more
  sensible
    - `handle_error` to `make_error`
    - `handle_err` to `throw_error`
- [ ] Unifiy error handling, define the status variable, use the 2 macros & gotos
everywhere
- [ ] __WIP__: Document this (Doxygen, ...)

# FTI Plugin
- [ ] Correct restart support
- [ ] Specify variables to protect in the config file (regex?)
- [ ] Specify the name of the snapshot event in the config files (regex?)
- [ ] Make information from FTI available for import (restart mode, etc...)
- [ ] Correctly support sparse data

# seq-HDF5 Plugin
- [ ] correctly handle dimensions (?) 
- [ ] error handling
- [ ] handle nested arrays

# Missing features
- [ ] Structs & arrays dimensions order (C vs. Fortran)
- [ ] Whole config specification from code
 - metadata, data & their types
 - plugins

# Design
- [ ] Support transactions (=multiple successive expose), trigger an event before transac close
- [ ] Support installation of filters for events & data by plugins (i.e. regexp)
- [ ] Define how error handling should be done in plugins
- [ ] Define how memory transfered to PDI should be handled, how can plugins
  request for the data to be kept alive? (if release how to use the data (efficiently))
- [ ] Determine if plugins should be notified about data not specified in the
  config file at all?
- [ ] Add support for situations where you want to continue even on error, but
  store them for later report (finalize, plugins, etc.)
- [ ] Make meta-data (a.k.a. params) a specific case of data (a.k.a. variables),
  let their exposure be notified to plugins, same as normal data

# New plugins ideas
- [ ] Parallel HDF5
- [ ] Debug (i.e., printf, cksum)
- [ ] AH5
- [ ] SIONlib (Jülich center) 
- [ ] XIOS 
- [ ] Post-processing (use fonction)
