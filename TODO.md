# API
- [ ] Ensure continuity for array (contiguous statement in fortran)

# Refactoring
- [ ] Unifiy error handling, define the status variable, use the 2 macros & gotos
      everywhere
- [ ] __WIP__: Document this (Doxygen, ...)

# FTI Plugin
- [ ] Correctly support sparse data

# seq-HDF5 Plugin
- [ ] correctly handle dimensions (?) 
- [ ] error handling
- [ ] handle nested arrays
- [ ] add checksum (global option)

# Missing features
- [ ] Structs & arrays dimensions order (C vs. Fortran)
- [ ] Whole config specification from code
 - metadata, data & their types
 - plugins
* automatic declaration of scalars and n-Dim arrays (configuration file should be simpler)
* handle multiple views on one array
- [ ] select plug-in for importing data (ie: handle concurrent plug-ins)
- [ ] add a verbose debugging mode

# Design
- [ ] Support installation of filters for events & data by plugins (i.e. regexp)
- [ ] Define how error handling should be done in plugins
- [ ] WIP: Define how memory transfered to PDI should be handled, how can plugins
  request for the data to be kept alive? (if release how to use the data (efficiently))
- [ ] Determine if plugins should be notified about data not specified in the
  config file at all?
- [ ] Add support for situations where you want to continue even on error, but
  store them for later report (finalize, plugins, etc.)
  let their exposure be notified to plugins, same as normal data


# New plugins ideas
- [ ] Parallel HDF5
- [ ] Debug (i.e., printf, cksum)
- [ ] AH5
- [ ] SIONlib (Jülich center) 
- [ ] XIOS 
- [ ] Post-processing (use fonction)


# Existing plugins (improvement)
* HDF5
   - [ ] better support various type (simple, double can be used but they are machine dependant -> real16, real32, real64, real128) ?
   - [ ] test import
    
