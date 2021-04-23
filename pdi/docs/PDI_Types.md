\page PDI_Types Type system

%PDI type system is used to describe the type of each data manipulated by %PDI.
This includes:
* the data layout in memory and how to copy or deallocate it,
* the data semantic interpretation (is it an integer or a floating point value).

Types of data exposed to PDI in C/C++/Fortran is provided from the specification
tree.

# Supported types

The %PDI type system support four kind of types: **arrays**, **record**,
**scalars** and **references**.

## Predefined C types

%PDI C provides the following predefined scalars:
* `char`,
* `double`,
* `float`,
* `int`,
* `int16`,
* `int32`,
* `int64`,
* `int8`.

`double`, `float`, `char` and `int` represent the types with the same names in
C.

`int8`, `int16`, `int32` and `int64` represent integers of the specified size in
bits, as provided by in the `<stdtypes.h>` header as `int8_t`, `int16_t`,
`int32_t` and `int64` respectively.

Each of these can be provided in the YAML specification tree either directly by
its name: `<NAME>` or using a key-value pair: `type: <NAME>`.
Hence, the following are equivalent:
```
data:
  my_intval: int
```
and
```
data:
  my_intval:
    type: int
```

## Predefined Fortan types

PDI Fortran provides the following predefined scalar templates:
* `character`,
* `integer`,
* `logical`,
* `real`.

Each of these represent the type with the same name in Fortran and accepts a
parameter named `kind` with the same default value and semantic as in the
Fortran compiler used.

These types can be provided in the YAML specification tree in the following way:
```
type: <NAME>
kind: <KIND>
```
for example:
```
type: integer
kind: 4
```

In case no `kind` is specified, the default kind is used and the YAML can be
simplified by replacing the `type: <NAME>` key-value by the name only.
Hence, the following are equivalent:
```
data:
  my_intval:
    type: integer
```
and
```
data:
  my_intval: integer
```
