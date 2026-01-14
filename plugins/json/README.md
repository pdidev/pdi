\page JSON_plugin JSON plugin
# JSON plugin {#JSON_plugin}

**WARNING** This documentation is a work in progress and does not 
reflect the full potential of the JSON plugin.

The JSON plugin enables users to export data in valid JSON format and allows for custom writing formats (\ref json_custom_format).

This plugin allows writing multiple variables in the same file as soon as they are available.

It supports not only basic data types such as scalar, array, record, tuple, and pointer, but also complex and nested combinations of these types. This includes multi-level arrays, multi-level records, and any mixture of the supported basic types at arbitrary levels of nesting. However, not all combinations are fully tested.

It ensures a JSON valid format at all times, even if the simulation crashes and PDI is not finalized, due to its incremental writing. **Warning:** this may result in increased disk bandwidth and may not be ideal for a performance-sensitive context. 
<!-- You might then want to consider using the `xxxxx` option to use the open file once behavior, instead of rewriting at each call. This would cause invalid JSON if simulation crashes or PDI's finalization is not called. But then you are using JSON anyways ... -->

Conditionnal writing is supported.

The write to file is only triggered when a variable is shared to PDI with read permission given.

Note that is written to this file the variable whichever variables listed in the "write" section when PDI is granted read permission.

The JSON plugin doesn't yet support the function of waiting for multiple variables to be readable before writing them.

## Configuration grammar {#json_configuration_grammar}

There are two syntax for the JSON configuration file:
First one is `var_name: file_path.json`, as for example :

```yaml
plugins:
  json:
    data1: file_path.json 
```

The second one starts with `- file: file_path.json` with arguments below

|key|value|
|:--|:----|
|`"file"` (*mandatory*)| Name of the output file|
|`"write"` (*mandatory*)| The variables to be writen|
|`"when"` (*optional*)| Indicate some kind of condition|

For example,

```yaml
plugins:
  json:
    - file: file_path.json
      when: iteration % 10 = 0 # This is optional
      write: [data1, data2, ...]
```

Note : By default, if the key "when" is not specified, a true condition is set so that the variable is written every time PDI is granted read permission.
Examples of those two syntax are given in `example/json.yml`.

## Install and test the JSON plugin ! {#json_test_plugin}
The JSON plugin relies on the [nlohmann/json](https://github.com/nlohmann/json) library to output data to JSON format.
The json library is included in the PDI repo as a submodule. To get the source code of this external library, one needs to do, from the root of PDI:
```bash
git submodule init
git submodule update
```
Next, users can activate the json_plugin by adding `BUILD_JSON_PLUGIN=ON` option to the PDI configuration commande.

Once the plugin installed, the easiest way to test the plugin is referring to the example section, which provides a full somewhat realistic C simulation code, where PDI is enabled and the JSON plugin is used. This one can be found in `example`.

To use it, first compile using CMake with `BUILD_TESTING` set to `ON`, then execute : 
```bash
build/PDI_EXAMPLE/PDI_example_C example/json.yml
```

<!-- ### Example output -->

## Full configuration file example {#json_config_example}

 Please refer to `example/json.yml`
## On going work / Limitations {#json_on_going_work}

- Adding support for reading variables. 
    Currently, the JSON plugin doesn't support reading variables, only writing them. To cover more use cases, the JSON plugin should be able to read variables as well as write them.
- The ability to create an on-the-go structures for several variables to be produced in a single JSON object

## The JSON plugin for custom formating {#json_custom_format}

For the sake of the example, let's imagine the following code, creating which performs a certain heat transfer simulation.

### The PDI exposes

<!-- \image html json_simulation_code_light.png width=800px -->

```C
    PDI_multi_expose("init",
        "simulation_name", &simulation_name, PDI_OUT,
        "max_steps", &max_steps, PDI_OUT,
        "mesh_config", &mesh_config, PDI_OUT,
        "rank", &rank, PDI_OUT,
        NULL) ;

    // main loop
    for (int step = 0; step < nb_steps; ++step) {
        do_compute(temp, MPI_COMM_WORLD) ;

        // share data at every iteration
        PDI_multi_expose("iter",
            "step", &step, PDI_OUT,
            "temp", temp, PDI_OUT,
            NULL) ;
        MPI_Barrier(MPI_COMM_WORLD);
        ...
    }
```

### The related YAML configuration
<!-- \image html simulation_yaml_light.png width=800px -->

```yaml
types: # [...] including config_t description
    metadata: {rank: int, step: int}
    data:
        simulation_name: { type: array, subtype: char, size: 512 }
        max_steps: int
        mesh_config:
            type: struct
            members:
            - dimensions: { type: array, subtype: int, size: 3}
            - spacings: { type: array, subtype: int, size: 3}

        temp: # the main temperature field
        - type: array
        - subtype: double
        - size: '$mesh_config.dimensions'

plugins:
    json:
    - file: data-$rank.json
      write: [step, temp]
      when: '$step > 0
```

### JSON output
If we run this code as is, the following JSON file would get generated.

<!-- \image html json_json_light.png width=800px -->

```json
[{
    "simulation_name": "Heat_transfer_simulation",
    "max_steps": 10,
    "mesh_config": {
        "dimensions": [100, 100, 50],
        "spacing": [0.1, 0.1, 0.1]
    },
    "rank": 7,
},
{
    "step" : 1,
    "temp": [75.3, 74.7, 76.1, ..., 54.2, 55.6]
},
{
    "step" :2,
    "temp" :[01.8, 89.6, 92.7, ..., 12.5, 13.7]
},
...
]
```

We can use this output json file to generate our custom format. For example, we can rely on [mustache](https://mustache.github.io/) to do so.

### Mustache code
<!-- \image html json_mustache_light.png width=800px -->

```mstch
Simulation {{simulation_name}} of {{max_steps}} iterations.
Mesh configuration was of {{mesh_config.dimensions}} dimensions and {{mesh_config.spacing}} spacings
{{#temperature_data}}
    At step {{step}}:
    temp was [{{#temp}}{{.}}{{#last}}{{^last}},{{/last}}{{/temp}}]
{{/temperature_datal}}
```

Finaly, let's print it ! We execute the json to mustache converter ...

```bash
vendor/mustach/mustach data.json format.mstch
```

... so we can get the following result !

<!-- \image html json_mustache_output_light.png width=800px -->

```text
Simulation Heat_transfer_simulation of 10 iterations.
Mesh configuration was of [100, 100, 50] dimensions and [0.1, 0.1, 0.1] spacings
At step 1, temp was [75.3, 74.7, 76.1, ..., 54.2, 55.6]
At step 2, temp was [91.8, 89.6, 92.7, ..., 12.5, 13.7]
...
```