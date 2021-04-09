\page First_steps First steps with %PDI

\section fs_hello_event Hello Event

As mentioned in \ref Specification_tree we have to provide specification tree to
instruct %PDI what data we will share and what to do with it.
We want to show what happens on each %PDI API call. We will use
\ref trace_plugin, which is very simple plugin that just prints every
information it gets.
Let's create a specification tree named `hello_event.yml` that will load trace
plugin:
```yaml
plugins:
  trace: ~
```

Yes, that is whole specification tree.
Trace plugin prints everything, so there is no need to specify what we want it
to do.

C source code:
```C
#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_event.yml"));

    PDI_event("Hello World Event");

    PDI_finalize();
    return 0;
}
```

Fortran source code:
```Fortran
program event
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t), target :: conf

    call PC_parse_path("hello_event.yml", conf)
    call PDI_init(conf)

    call PDI_event("Hello World Event")

    call PDI_finalize()

end program event
```

Let's analyze what happens in each line. Firstly we have PDI_init() function which take parameter of type `PC_tree_t`. It's a tree structure parsed from some YAML file, in our case we parse it with `paraconf` library build in %PDI. To parse a file we need to call `PC_parse_path` function passing file path as argument. The next step is to call an event in %PDI named "Hello World Event". At the end we have to call PDI_finalize(). The output from this program is presented below:
```
[PDI][Trace-plugin][10:25:28] *** info: Welcome!
[PDI][10:25:28] *** info: Initialization successful
[PDI][Trace-plugin][10:25:28] *** info: !!! named event: Hello World Event
[PDI][10:25:28] *** info: Finalization
[PDI][Trace-plugin][10:25:28] *** info: Goodbye!
```
The first line indicates that plugin has loaded successfully. The second is %PDI message, that tells it managed to create all descriptors and load all defined plugins. Then we have message from loaded trace plugin which printed the event name it has received. The next information is from %PDI and indicates that finalization has started and now it will deallocate resources. Last message is from trace plugin destructor. 

\section fs_hello_data Hello Data
In \ref fs_hello_event we learned how to call an event. In this chapter we will see how to share and reclaim data.

Firstly we have to create a specification tree named `hello_data.yml` with `data` and trace plugin tree declared:

```yaml
data:
  world: int
plugins:
  trace: ~
```

We have declared trace plugin and one descriptor named `world` of integer type.

C source code:
```C
#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_data.yml"));
    int my_world = 42;
    PDI_share("world", &my_world, PDI_OUT);
    //variable my_world is shared with PDI

    PDI_reclaim("world");
    //variable my_world is no longer shared with PDI

    PDI_finalize();
    return 0;
}
```

Fortran source code:
```Fortran
program data
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t),target :: conf
    integer, target        :: my_world
    integer, pointer       :: p_my_world
    
    call PC_parse_path("hello_data.yml", conf)
    call PDI_init(conf)

    my_world = 42
    p_my_world => my_world
    call PDI_share("world", p_my_world, PDI_OUT)
    //variable my_world is shared with PDI

    call PDI_reclaim("world");
    //variable my_world is no longer shared with PDI

    call PDI_finalize()

end program data
```

Let's analyze new functions:
- `PDI_share` shares access to the variable with %PDI. The first argument is a descriptor name and indicates what data we are sharing. The second one is pointer to our variable and the last one is access direction. `PDI_OUT` means data direction from application to %PDI, `PDI_IN` is a direction from %PDI to the program, `PDI_INOUT` includes both directions. 
- `PDI_reclaim` reclaims the share which means that %PDI will no longer have access to shared variable. As an argument it takes name of the descriptor.

The output from our application:
```
[PDI][Trace-plugin][10:59:35] *** info: Welcome!
[PDI][10:59:35] *** info: Initialization successful
[PDI][Trace-plugin][10:59:35] *** info: =>>   data becoming available in the store: world
[PDI][Trace-plugin][10:59:35] *** info: <<= data stop being available in the store: world
[PDI][10:59:35] *** info: Finalization
[PDI][Trace-plugin][10:59:35] *** info: Goodbye!
```

As we can see from the logs above, when we called `PDI_share` plugin gained access to the shared variable and after `PDI_reclaim` the variable has become no longer available for the plugin. The share notification gives plugin possibility to operate on data dependently what has been declared in specification tree.

The same exact result we can achieve with `PDI_expose` which is just `PDI_share` call and right after `PDI_reclaim` is called.

C source code:
```C
    PDI_share("world", &my_world, PDI_OUT);
    PDI_reclaim("world");
```

is the same as:
```C
    PDI_expose("world", &my_world, PDI_OUT);
```

Fortran source code:
```Fortran
    call PDI_share("world", my_world, PDI_OUT)
    call PDI_reclaim("world");
```
is the same as:
```Fortran
    call PDI_expose("world", my_world, PDI_OUT)
```

\section fs_access Hello Access
Now we will try to access a descriptor we share with %PDI. In this case we won't need any plugin. We want to define int and a string in our `world_access.yml`:
```yaml
data:
  my_value: int
  my_message: {type: array, subtype: char, size: 32}
```

C source code:
```C
#include <pdi.h>

void print_secret_msg() {
    int* value;
    PDI_access("my_value", (void**)&value, PDI_IN);
    printf("%d\n", *value);
    PDI_release("my_value");

    char* message;
    PDI_access("my_message", (void**)&message, PDI_IN);
    printf("%s\n", message);
    PDI_release("my_message");
}

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("world_access.yml"));
    int my_value = 42;
    PDI_share("my_value", &my_value, PDI_OUT);

    char* secret_msg = "Watermelon is the tastiest fruit";
    PDI_share("my_message", secret_msg, PDI_OUT);

    print_secret_msg();

    PDI_reclaim("my_message");
    PDI_reclaim("my_value");

    PDI_finalize();
    return 0;
}
```

Fortran source code:
```Fortran
subroutine print_secret_msg
  use pdi
  implicit none

  integer, pointer               :: p_val
  character, pointer, contiguous :: p_msg(:)
  integer                        :: msg_ranks(1)

  call pdi_access("value", p_val, pdi_in)
  print *, "PDI value: ", p_val
  call pdi_release("value")

  ! In case of accessing arrays, PDI_access takes additional array argument with dimensions sizes
  msg_ranks(1) = 32
  call PDI_access("message", p_msg, pdi_in, msg_ranks)
  print *, "PDI message: ", p_msg
  call pdi_release("message")
  return
end subroutine print_secret_msg

program access
  use paraconf
  use pdi
  implicit none
  type(pc_tree_t),target          :: conf
  integer, target                 :: my_value
  integer, pointer                :: p_my_value
  character, target               :: secret_message(28)
  character, pointer, contiguous  :: p_secret_message(:)
  call pc_parse_path("hello_access.yml", conf)
  call pdi_init(conf)
  my_value = 42
  p_my_value => my_value
  call pdi_share("value", p_my_value, pdi_out)
  secret_message = (/ "O", "r", "a", "n", "g", "e", " ", "i", "s",&
        " ", "t", "h", "e", " ", "t", "a", "s", "t", "i", "e", "s",&
        "t", " ", "f", "r", "u", "i", "t" /)
  p_secret_message => secret_message
  call pdi_share("message", p_secret_message, pdi_out)
  print *, "My value: ", my_value
  print *, "My message: ", secret_message
  call print_secret_msg()
  call pdi_reclaim("message")
  call pdi_reclaim("value")
  call pdi_finalize()
end program access

```

We will focus on `print_secret_msg` function. If you don't understand what happens in `main` function, please see \ref fs_hello_data example. `PDI_access` sets our pointer to the data location. We need to pass `PDI_IN` because data flows from %PDI to our application. We also want to use `PDI_release`, because `PDI_reclaim` would end the sharing status of this descriptor and we reclaim this data later in `main` function.
Output from the program:

```
[PDI][13:42:31] *** info: Initialization successful
42
Watermelon is the tastiest fruit
[PDI][13:42:31] *** info: Finalization
```

As you can see, we manage to access data descriptor from function only by passing its name and correct direction access.

\section fs_multiexpose Hello multi expose
In some cases we would want to expose many descriptors at once. For this we have multi expose which shares all the given descriptors, then call given event and then reclaim all passed data. Let's look at the example.

```yaml
data:
  my_int: int
  my_float: float
  my_string: {type: array, subtype: char, size: 32}

plugins:
  trace: ~
```

We have defined 3 descriptors and trace plugin.

C source code:
```C
#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_multi_expose.yml"));
    int x = 0;
    float y = 0;
    char* z = "RGB = Really Gawky Biscuit";

    PDI_multi_expose("event_between", 
                     "my_int", &x, PDI_OUT,
                     "my_float", &y, PDI_OUT,
                     "my_string", &z, PDI_OUT,
                     NULL);

    PDI_finalize();
    return 0;
}
```

First argument of the `PDI_multi_expose` is the event name we want to call when all the descriptors are shared. After this we pass in loop:

- name of the descriptor
- pointer to the data
- direction access
As the last argument we have to pass `NULL`.


Fortran source code (have to use transaction):
```Fortran
program transaction
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t)   :: conf
    integer           :: my_int
    real              :: my_float
    character(len=32) :: my_string

    call PC_parse_path("hello_transaction.yml", conf)
    call PDI_init(conf)

    my_int = 0
    my_float = 0
    my_string = "RGB = Really Gawky Biscuit";
    
    call PDI_transaction_begin("event_between")
    
    call PDI_expose("my_int", my_int, PDI_OUT)
    call PDI_expose("my_float", my_float, PDI_OUT)
    call PDI_expose("my_string", my_string, PDI_OUT)
    
    call PDI_transaction_end()

    call PDI_finalize()

end program transaction
```


The output of the execution:

```
[PDI][Trace-plugin][14:14:51] *** info: Welcome!
[PDI][14:14:51] *** info: Initialization successful
[PDI][Trace-plugin][14:14:51] *** info: =>>   data becoming available in the store: my_int
[PDI][Trace-plugin][14:14:51] *** info: =>>   data becoming available in the store: my_float
[PDI][Trace-plugin][14:14:51] *** info: =>>   data becoming available in the store: my_string
[PDI][Trace-plugin][14:14:51] *** info: !!!                            named event: event_between
[PDI][Trace-plugin][14:14:51] *** info: <<= data stop being available in the store: my_string
[PDI][Trace-plugin][14:14:51] *** info: <<= data stop being available in the store: my_float
[PDI][Trace-plugin][14:14:51] *** info: <<= data stop being available in the store: my_int
[PDI][14:14:51] *** info: Finalization
[PDI][Trace-plugin][14:14:51] *** info: Goodbye!
```

The logs from trace plugin confirm the execution order we were expecting.
