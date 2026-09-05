# The user-code plugin {#user_code_plugin}

The `user-code` plugin enables one to call a user-defined function when a specified event occurs or
certain data becomes available.

## Important notes {#important_notes_node}

* Make sure to compile your program with `Wl,--export-dynamic` or `-rdynamic`
  flag (in CMake set `ENABLE_EXPORTS` to `TRUE` using `set_target_properties`
  command) in order to generate necessary symbols.
* Make sure you use the proper access rights in your function in \ref PDI_access
  (PDI_IN for reading, PDI_OUT for writing).
* Descriptor aliases enable one to use different descriptors without the need
  to recompile the code.

## Dependencies between the code and the specification tree {#dependencies_node}

To ensure the `user-code` plugin works properly, there are several conventions to follow in the
application code and in the specification tree.

First, each function name in the specification tree must be a valid function name (watch out for
[name mangling](https://en.wikipedia.org/wiki/Name_mangling)).
Please do not use any %PDI YAML key (`when`, `file`, etc.) as a function name.

Second, these functions **can not** take any argument or return any value, that is to say their
type must be `void(void)`.
Use descriptors to pass input and output variables instead.

Third, the symbols of the functions **must** be exported to make them accessible to the
`user-code` plugin. To do this, compile your program as described in \ref important_notes_node.

## Use examples {#use_examples_node}

This section shows simple examples of the use of the `user-code` plugin.

### Hello world! {#hello_world_node}

First, we will call a simple function, without using any descriptor, to print "Hello world!" on the
"print" event.

hello_world.c:
\snippet user_code/docs/hello_world.c example

hello_world.yml:
\snippet user_code/docs/hello_world.yml example

output:
\snippet user_code/docs/hello_world.output output

### Handling input {#handling_input_node}

Now we will pass some input data to the function.

print_number.c:
\snippet user_code/docs/print_number.c example

print_number.yml:
\snippet user_code/docs/print_number.yml example

output:
\snippet user_code/docs/print_number.output output

We can simplify this example by using `on_data` to print the value of `number` when it is shared
with %PDI.

print_number_on_data.c:
\snippet user_code/docs/print_number_on_data.c example

print_number_on_data.yml:
\snippet user_code/docs/print_number_on_data.yml example


the output does not change:
\snippet user_code/docs/print_number_on_data.output output

### Handling output {#handling_output_node}

Output handling is very similar to input handling, the only difference being the access rights.
In this example we call the `add_ten` function when `number` is shared with %PDI.

adding_to_number.c:
\snippet user_code/docs/adding_to_number.c example

adding_to_number.yml:
\snippet user_code/docs/adding_to_number.yml example

output:
\snippet user_code/docs/adding_to_number.output output

### Multiple input/output data {#multiple_inout_data_node}

In this example we use several data in a function.
We add and multiply two given numbers and return the results on the "calculate" event.

calculate.c:
\snippet user_code/docs/calculate.c example

calculate.yml:
\snippet user_code/docs/calculate.yml example

output:
\snippet user_code/docs/calculate.output output

## Configuration grammar {#conf_grammar_node}

The root of `user-code` plugin is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"logging"`  (*optional*)|a \ref logging_node|
|`"on_data"`  (*optional*)|a \ref on_data_node    |
|`"on_event"` (*optional*)|a \ref on_event_node   |
|`".*"`       (*optional*)| *anything*            |

* the `on_data` key specifies the list of descriptors that, when they become available, cause the
  specified functions to be called,
* the `on_event` key specifies the list of events on which to call the specified functions,
* additional keys are ignored.

### on_data {#on_data_node}

A \ref on_data_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"when"` (*optional*)|a $-expression defines a condition for executing the function|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of a descriptor, which triggers the specified functions when it
  becomes available.

If a data is used to trigger several functions and some of them should only be executed when the
`when` condition is satisfied, we can use the list-styled YAML syntax:
\snippet user_code/docs/user_code_examples.cxx on_data_list


In this example, `fun2` and `fun3` are executed when `my_data` is shared with %PDI.
`fun1`, however, is only executed when `my_data` is shared and `$cond>1`.

### on_event {#on_event_node}

A \ref on_event_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"when"` (*optional*)|a $-expression defines a condition for executing the function|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of an event, which triggers the specified functions when it
  occurs.

If an event is used to trigger several functions and some of them should only be executed when the
`when` condition is satisfied, we can use the list-styled YAML syntax:
\snippet user_code/docs/user_code_examples.cxx on_event_list


In this example, `fun2` and `fun3` are executed when `my_event` is issued.
`fun1`, however, is only executed when `my_event` is issued and `$cond>1`.

### function_list {#function_list_node}

A \ref function_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_param_list_node|

* each key identifies the name of a function, which is called on the specified event or data,
* **NOTE**: these functions **can not** take any argument or return any value, that is to say
  their type must be `void(void)`.

### function_param_list {#function_param_list_node}

A \ref function_param_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a $-expression referencing a data|

* each key identifies the name of a descriptor alias, which is available during the execution of
  the function.

## Specification tree example {#full_spec_tree_example_node}

\snippet user_code/docs/user_code_examples.cxx full_tree

