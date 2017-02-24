# Gitlab: new branch, merge request, issues

## General Rules
 * Names consists of three zones
    * 1st: type of improvement (bug, feature, update, support...)
    * 2nd: part of the software concerned (PDI, one plug-in, all plug-ins...)
    * last: brief description 
 * Each zone is delimited by two hyphens
 * Space are placed by underscore
 
### issues:
 * 1st keyword: bug, miss_feature, outdated, ...
 * 2nd keyword: PDI or <Plug-in name> 
 * 3rd: content
 * Examples:
     *  bug--HDF5_per_process--Something_is_wrong_there
     *  miss_feature--PDI--Should_make_the_tea

### Branch and merge request
 * 1st keyword: bugfix, feature, update ...
 * 2nd keyword: PDI or <Plug-in name>
 * 3rd content
 * Examples:
     *  bugfix--HDF5_per_process--Something_is_working_better_there
     *  feature--PDI--Now_make_the_tea



# PDI Coding style

## API visibility levels

There are four level of visibilty defined for PDI:

| **API**                |  **Exposed**  | **Headers**         |
| :--------------------  |:-------------:| :--------------:|
|  public application    |  all content  | include/pdi.h   |
|  public plugin         |  all content  | include/pdi/*.h.|
|  library-wide          |  all content  | src/*.h         |
|  file-local APIs       |  any content  |  not exposed in any header  |


## Rules

### Naming

* any public content (symbol, type, macro, ...) *must* start with the `PDI_` prefix
* macro names *must* be all caps
* enum names *should* end with a `_e` suffix, this is mandatory for public enums
* struct names *should* end with a `_s` suffix, this is mandatory for public enums
* typedefs names  *should* end with a `_t` suffix, this is mandatory for public typedefs
* name elements *should* be separated by underscores

### Error handling

* any public or library-wide function *should* return a `PDI_status_t` error status
* error *should* be handled linux-style, i.e. a single return at the end of the function followed by tags to go to on error that free allocated resources
* errors *can* be created using the `handle_error()` function
* errors *should* be forwarded using the `handle_err` macro

### Misc.

* any public (app or plugin) symbol (i.e. function or global variable) *must* be exported using the `PDI_EXPORT` macro
* public typedefs & enums *must* be defined in a header ending with the `_fwd.h` suffix
* no struct *might* be defined in a header ending with the `_fwd.h` suffix

