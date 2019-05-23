\page user_code_plugin The user-code plugin

The user-code plugin enables one to call a user-defined function 
when a specified event occur or certain data becomes available.

\section conf_grammar_node Configuration grammar

The root of `user-code` plugin is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"on_data"`  (*optional*)|a \ref on_data_node |
|`"on_event"` (*optional*)|a \ref on_event_node|
|`".*"`       (*optional*)| *anything*         |

* the `on_data` key specifies the list of descriptors that, when they become available, 
will cause the specified functions to be called,
* the `on_event` key specifies the list of events on which to call the specified functions,
* additional keys are ignored.

\subsection on_data_node on_data

A \ref on_data_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of a descriptor, which will trigger specified functions when it becomes available.

\subsection on_event_node on_event

A \ref on_event_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of an event, which will trigger specified functions when it occurs.

\subsection function_list_node function_list

A \ref function_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_param_list_node|

* each key identifies the name of a function, which will be called on specified event or data,
* **NOTE**: these functions **can not** take any arguments or return any value (i.e. their type must be `void(void)`).

\subsection function_param_list_node function_param_list

A \ref function_param_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a $-expression referencing a data|

* each key identifies the name of a descriptor alias, which will be available during function execution.

\section full_spec_tree_example_node Specification tree example

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

\section use_examples_node Use examples

This section shows a simple examples with the use of a `user-code` plugin.

\subsection hello_world_node Hello world!

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
Hello world!
```

\subsection handling_input_node Handling input

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
I've got number 42.
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
I've got number 42.
```

\subsection handling_output_node Handling output

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
Before expose, number = 42.
After expose, number = 52.
```

\subsection multiple_inout_data_node Multiple input/output data

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
    PDI_multi_expose("calculate", 
        "foo", &number, PDI_OUT,
        "bar", &number, PDI_OUT,
        "res1", &number, PDI_IN
        "res2", &number, PDI_IN);
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
After calculation, foo = 4, bar = 5, res1 = 9, res2 = 20.\n
```
\section important_notes_node Important notes
 * Make sure you use the proper access rights in your function in \ref PDI_access (PDI_IN for reading, PDI_OUT for writing).
 * Descriptor aliases enables one to use different descriptors without the need to recompile the code.