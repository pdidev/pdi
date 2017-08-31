# Gitlab: new branch, merge request, issues

## General Rules
 * Branch names, merge request and issues titles consist of three zones
    * 1st, type of improvement: 
        * bugfix : remove a bug
        * feature : add a new feature to PDI or an existing plug-in
        * update : update an existing feature
        * new : add a new plug-in
        * ... etc.
    * 2nd, part of the software concerned: PDI, one plug-in, all plug-ins...
    * Last, a brief description 
 * Each zone is delimited by two hyphens
 * Spaces are replaced by underscores


### Branch and merge request
 * 1st keyword: bugfix, feature, update, new ...
 * 2nd keyword: PDI or \<Plug-in name\>
 * 3rd content
 * Examples:
     *  bugfix--HDF5_per_process--Something_is_working_better_there
     *  feature--PDI--Now_make_the_tea



# PDI Coding style

## API visibility levels

There are four levels of visibility:

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
* struct names *should* end with a `_s` suffix, this is mandatory for public structs
* typedefs names  *should* end with a `_t` suffix, this is mandatory for public typedefs
* name elements *should* be separated by underscores

### Functions

* The code *should* be mostly object-oriented
* Function names *should* be composed of an identifier common to all functions manipulating the same type followed by a descriptor of the function role
* The main data manipulated by the function *must* come first
* output parameters *should* come just after the main data

### Error handling

* any public or library-wide function *should* return a `PDI_status_t` error status
* error *should* be handled linux-style, i.e. a single return at the end of the function followed by tags to go to on error that free allocated resources
* errors *can* be created using the `PDI_make_err` function
* errors *should* be forwarded using the `PDI_handle_err` macro


### C++, future rules (code is being rewritten)
#### Class
```
// Header
class My_class {
public: 
  void example_function();
private:
  int m_an_int;
}

// Code
int my_int = 0;
My_class an_instance;
an_instance.example_function();

```
* Class begins with an upper case and use underscore to separate words
* Member variables begin with an `m_` followed by lower case and underscores
* Private member fonctions begin with a lower score followed by underscores to separate words
* Data members of classes, both static and non-static, are named like ordinary nonmember variables, but with a trailing underscore. 

#### Variables 
* Begins with a lower case

#### Macro
* All caracters are upper case and use underscores to separate words


### Misc.

* any public (app or plugin) symbol (i.e. function or global variable) *must* be exported using the `PDI_EXPORT` macro
* public typedefs & enums *must* be defined in a header ending with the `_fwd.h` suffix
* no struct *might* be defined in a header ending with the `_fwd.h` suffix

## Indent & Format.

A astyle configuration file is provided with a Makefile in `/tools/Format_and_Indent/`


 *  `make indent_all`: copy and create indented copies into the subfolder workdir
 *  `make replace_all`: replace the original sources and headers with the indented versions from workdir. __  _If the original sources files were modified, this will replace your files!_ __
 *  `make all` / `make` : execute the two previous commands.
 *  `make clean`: remove the temporary content of workdir




