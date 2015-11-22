# Some rules

* All header files in the include directory make up the PDI public API.
* The public API is made up of two parts
 - the application API in `pdi.h`
 - the plugin API in `pdi/plugin.h`
* any public symbol (i.e. function or global variable) should be exported using the PDI_EXPORT macro
* any public symbol (i.e. function or global variable), type or macro should start with the `PDI_` prefix
* all functions should return a `PDI_status_t` error status
