# PDI Coding style

## API visibility levels

there are four level of visibilty defined for PDI:
* the public application API made up of all content exposed in the include/pdi.h header
* the public plugin API made up of all content exposed in the include/pdi/*.h headers
* the library-wide API made up of all content exposed in the src/*.h headers
* the file-local APIs made up of any content not exposed in any header

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
