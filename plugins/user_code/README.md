# The user-code plugin {#user_code_plugin}

The `user-code` plugin enables one to call a user-defined function 
when a specified event occur or certain data becomes available.

## Important notes {#important_notes_node}

* Make sure to compile your program with `Wl,--export-dynamic` or `-rdynamic`
  flag (in CMake set `ENABLE_EXPORTS` to `TRUE` using `set_target_properties`
  command) in order to generate necessary symbols.
* Make sure you use the proper access rights in your function in \ref PDI_access
  (PDI_IN for reading, PDI_OUT for writing).
* Descriptor aliases enables one to use different descriptors without the need
  to recompile the code.

## Dependencies between the code and the specification tree {#dependencies_node}

To ensure proper work of the `user-code` plugin, there are several conventions to follow in the application 
code and the specification tree.

First, each function name in specification tree must be a valid function name (watch out for [name mangling](https://en.wikipedia.org/wiki/Name_mangling)).

Second, these functions **can not** take any arguments or return any value (i.e. their type must be `void(void)`). Use descriptors for passing input/output variables instead.

Third, function's symbols **must** be exported to make them accessible by `user-code` plugin. To do this, compile your program as described in \ref important_notes_node.

## Use examples {#use_examples_node}

This section shows a simple examples with the use of a `user-code` plugin.

### Hello world! {#hello_world_node}

First, we will call simple function without the use of descriptors to print "Hello world!"
on "print" event.

hello_world.c:
```C
#include <stdio.h>
#include <pdi.h>

void hello_world(void)
{
    printf("Hello world!\n");
}

int main(int argc, char* argv[])
{
    PC_tree_t conf = PC_parse_path("hello_world.yml");
    PDI_init(conf);
    PDI_event("print");
    PDI_finalize();
    return 0;
}
```

hello_world.yml:
```yaml
plugins:
    user_code:
        on_event:
            print:
                hello_world: {}
```

output:
```console
[PDI][User-code][13:40:55] *** info: Plugin loaded successfully
[PDI][13:40:55] *** info: Initialization successful
Hello world!
[PDI][13:40:55] *** info: Finalization
[PDI][User-code][13:40:55] *** info: Closing plugin
```

### Handling input {#handling_input_node}

Now we will pass some input data to the function.

print_number.c:
```C
#include <stdio.h>
#include <pdi.h>

void print_number(void)
{
    void* input; PDI_access("input", &input, PDI_IN);
    printf("I've got number %d.\n", *((int*)input));
    PDI_release("input");
}

int main(int argc, char* argv[])
{
    PC_tree_t conf = PC_parse_path("print_number.yml");
    PDI_init(conf);
    int number = 42;
    PDI_share("number", &number, PDI_OUT);
    PDI_event("print");
    PDI_reclaim("number");
    PDI_finalize();
    return 0;
}
```

print_number.yml:
```yaml
data:
    number: int
plugins:
    user_code:
        on_event:
            print:
                print_number: {input: $number}
```

output:
```console
[PDI][User-code][13:49:30] *** info: Plugin loaded successfully
[PDI][13:49:30] *** info: Initialization successful
I've got number 42.
[PDI][13:49:30] *** info: Finalization
[PDI][User-code][13:49:30] *** info: Closing plugin

```

We can simplify this example by using `on_data` to print value of number
when it is shared to %PDI.

print_number.c:
```C
#include <stdio.h>
#include <pdi.h>

void print_number(void)
{
    void* input; PDI_access("input", &input, PDI_IN);
    printf("I've got number %d.\n", *((int*)input));
    PDI_release("input");
}

int main(int argc, char* argv[])
{
    PC_tree_t conf = PC_parse_path("print_number.yml");
    PDI_init(conf);
    int number = 42;
    PDI_expose("number", &number, PDI_OUT);
    //PDI_event is no longer necessary and PDI_share/PDI_reclaim can be simplified to PDI_expose
    PDI_finalize();
    return 0;
}
```

print_number.yml:
```yaml
data:
    number: int
plugins:
    user_code:
        on_data:
            number:
                print_number: {input: $number}
```

output does not change:
```console
[PDI][User-code][13:52:15] *** info: Plugin loaded successfully
[PDI][13:52:15] *** info: Initialization successful
I've got number 42.
[PDI][13:52:15] *** info: Finalization
[PDI][User-code][13:52:15] *** info: Closing plugin
```

### Handling output {#handling_output_node}

Output handling is very similar to the input handling, the only
difference are the access rights. In this example we will call
`add_ten` function when `number` is shared to %PDI.

adding_to_number.c:
```C
#include <stdio.h>
#include <pdi.h>

void add_ten(void)
{
    void* input; PDI_access("input", &input, PDI_OUT);
    *((int*)input) += 10;
    PDI_release("input");
}

int main(int argc, char* argv[])
{
    PC_tree_t conf = PC_parse_path("adding_to_number.yml");
    PDI_init(conf);
    int number = 42;
    printf("Before expose, number = %d.\n", number);
    PDI_expose("number", &number, PDI_IN);
    printf("After expose, number = %d.\n", number);
    PDI_finalize();
    return 0;
}
```

adding_to_number.yml:
```yaml
data:
    number: int
plugins:
    user_code:
        on_data:
            number:
                add_ten: {input: $number}
```

output:
```console
[PDI][User-code][13:53:51] *** info: Plugin loaded successfully
[PDI][13:53:51] *** info: Initialization successful
Before expose, number = 42.
After expose, number = 52.
[PDI][13:53:51] *** info: Finalization
[PDI][User-code][13:53:51] *** info: Closing plugin

```

### Multiple input/output data {#multiple_inout_data_node}

In this example we will use multiple data in function. We will add 
and multiply two given numbers and return the results on event "calculate".

calculate.c:
```C
#include <stdio.h>
#include <pdi.h>

void sum_and_multiply(void)
{
    void* number1; PDI_access("number1", &number1, PDI_IN);
    void* number2; PDI_access("number2", &number2, PDI_IN);
    void* sum;     PDI_access("sum", &sum, PDI_OUT);
    void* product; PDI_access("product", &product, PDI_OUT);
    *((int*)sum) = *((int*)number1) + *((int*)number2);
    *((int*)product) = *((int*)number1) * *((int*)number2);
    PDI_release("number1");
    PDI_release("number2");
    PDI_release("sum");
    PDI_release("product");
}

int main(int argc, char* argv[])
{
    PC_tree_t conf = PC_parse_path("calculate.yml");
    PDI_init(conf);
    int foo = 4, bar = 5, res1 = 0, res2 = 0;
    printf("Before calculation, foo = %d, bar = %d, res1 = %d, res2 = %d.\n", foo, bar, res1, res2);
    PDI_multi_expose("calculate", 
        "foo", &foo, PDI_OUT,
        "bar", &bar, PDI_OUT,
        "res1", &res1, PDI_IN,
        "res2", &res2, PDI_IN, NULL);
    printf("After calculation, foo = %d, bar = %d, res1 = %d, res2 = %d.\n", foo, bar, res1, res2);
    PDI_finalize();
    return 0;
}
```

calculate.yml:
```yaml
data:
    foo: int
    bar: int
    res1: int
    res2: int
plugins:
    user_code:
        on_event:
            calculate:
                sum_and_multiply: {number1: $foo, number2: $bar, sum: $res1, product: $res2}
```

output:
```console
[PDI][User-code][13:58:20] *** info: Plugin loaded successfully
[PDI][13:58:20] *** info: Initialization successful
Before calculation, foo = 4, bar = 5, res1 = 0, res2 = 0.
After calculation, foo = 4, bar = 5, res1 = 9, res2 = 20.
[PDI][13:58:20] *** info: Finalization
[PDI][User-code][13:58:20] *** info: Closing plugin
```

## Configuration grammar {#conf_grammar_node}

The root of `user-code` plugin is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"logging"`  (*optional*)|a \ref logging_node|
|`"on_data"`  (*optional*)|a \ref on_data_node    |
|`"on_event"` (*optional*)|a \ref on_event_node   |
|`".*"`       (*optional*)| *anything*            |

* the `on_data` key specifies the list of descriptors that, when they become available, 
will cause the specified functions to be called,
* the `on_event` key specifies the list of events on which to call the specified functions,
* additional keys are ignored.

### on_data {#on_data_node}

A \ref on_data_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of a descriptor, which will trigger specified functions when it becomes available.

### on_event {#on_event_node}

A \ref on_event_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of an event, which will trigger specified functions when it occurs.

### function_list {#function_list_node}

A \ref function_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_param_list_node|

* each key identifies the name of a function, which will be called on specified event or data,
* **NOTE**: these functions **can not** take any arguments or return any value (i.e. their type must be `void(void)`).

### function_param_list {#function_param_list_node}

A \ref function_param_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a $-expression referencing a data|

* each key identifies the name of a descriptor alias, which will be available during function execution.

## Specification tree example {#full_spec_tree_example_node}

```yaml                   
data:
    desc1: int                          
    desc2: float                   
    desc3: double                  
plugins:                       
    user_code:                   
        on_data:                   
            desc1:                 
                fun1: {in: $desc2, out: $desc3}
            desc2:
                fun2: {}
                fun3: {out: $desc2}
        on_event:                  
            event1:                 
                fun2: {}
            event2:
                fun4: {param1: $desc2, param2: $desc1, param3: $desc3}
```
