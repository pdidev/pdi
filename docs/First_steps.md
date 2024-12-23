# First steps with %PDI {#First_steps}


## Hello Event {#fs_hello_event}

As mentioned in \ref Specification_tree we have to provide specification tree to
instruct %PDI what data we will share and what to do with it.
We want to show what happens on each %PDI API call. We will use
\ref trace_plugin, which is very simple plugin that just prints every
information it gets.
Let's create a specification tree named `hello_event.yml` that will load trace
plugin:
\include First_steps/hello_event.yml

Yes, that is whole specification tree.
Trace plugin prints everything, so there is no need to specify what we want it
to do.

C source code:
\include First_steps/hello_event.c

Fortran source code:
\include First_steps/hello_event.f90

Let's analyze what happens in each line. Firstly we have PDI_init() function which take parameter of type `PC_tree_t`. It's a tree structure parsed from some YAML file, in our case we parse it with `paraconf` library build in %PDI. To parse a file we need to call `PC_parse_path` function passing file path as argument. The next step is to call an event in %PDI named "Hello World Event". At the end we have to call PDI_finalize(). The output from this program is presented below:
```
[PDI][Trace-plugin][10:25:28] *** info: Welcome!
[PDI][10:25:28] *** info: Initialization successful
[PDI][Trace-plugin][10:25:28] *** info: !!! named event: Hello World Event
[PDI][10:25:28] *** info: Finalization
[PDI][Trace-plugin][10:25:28] *** info: Goodbye!
```
The first line indicates that plugin has loaded successfully. The second is %PDI message, that tells it managed to create all descriptors and load all defined plugins. Then we have message from loaded trace plugin which printed the event name it has received. The next information is from %PDI and indicates that finalization has started and now it will deallocate resources. Last message is from trace plugin destructor. 



## Hello Data {#fs_hello_data}

In \ref fs_hello_event we learned how to call an event. In this chapter we will see how to share and reclaim data.

Firstly we have to create a specification tree named `hello_data.yml` with `data` and trace plugin tree declared:
\include First_steps/hello_data.yml

We have declared trace plugin and one descriptor named `world` of integer type.

C source code:
\include First_steps/hello_data.c

Fortran source code:
\include First_steps/hello_data.f90

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
\snippet First_steps/hello_expose.c share_reclaim

is the same as:
\snippet First_steps/hello_expose.c expose

Fortran source code:
\snippet First_steps/hello_expose.f90 share_reclaim

is the same as:
\snippet First_steps/hello_expose.f90 expose



## Hello Access {#fs_hello_access}

Now we will try to access a descriptor we share with %PDI. In this case we won't need any plugin. We want to define int and a string in our `hello_access.yml`:
\include First_steps/hello_access.yml

C source code:
\include First_steps/hello_access.c

Fortran source code:
\include First_steps/hello_access.f90


We will focus on `print_secret_msg` function. If you don't understand what happens in `main` function, please see \ref fs_hello_data example. `PDI_access` sets our pointer to the data location. We need to pass `PDI_IN` because data flows from %PDI to our application. We also want to use `PDI_release`, because `PDI_reclaim` would end the sharing status of this descriptor and we reclaim this data later in `main` function.
Output from the program:

```
[PDI][13:42:31] *** info: Initialization successful
42
Watermelon is the tastiest fruit
[PDI][13:42:31] *** info: Finalization
```

As you can see, we manage to access data descriptor from function only by passing its name and correct direction access.



## Hello multi expose {#fs_hello_multi_expose}

In some cases we would want to expose many descriptors at once. For this we have multi expose which shares all the given descriptors, then call given event and then reclaim all passed data. Let's look at the example.
\include First_steps/hello_multi_expose.yml

We have defined 3 descriptors and trace plugin.

C source code:
\include First_steps/hello_multi_expose.c

First argument of the `PDI_multi_expose` is the event name we want to call when all the descriptors are shared. After this we pass in loop:

- name of the descriptor
- pointer to the data
- direction access
As the last argument we have to pass `NULL`.

Fortran source code (have to use transaction):
\include First_steps/hello_multi_expose.f90

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
