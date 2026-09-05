# First steps with %PDI {#First_steps}


## Hello Event {#fs_hello_event}

As mentioned in \ref Specification_tree, we have to provide a specification tree to instruct %PDI
what data we will share and what to do with it.
We want to show what happens on each %PDI API call.
We will use \ref trace_plugin, which is a very simple plugin that just prints every piece of
information it gets.
Let's create a specification tree named `hello_event.yml` that loads the Trace plugin:
\snippet First_steps/hello_event.yml example

Yes, that is the whole specification tree.
The Trace plugin prints everything, so there is no need to specify what we want it to do.

C source code:
\snippet First_steps/hello_event.c example

Fortran source code:
\snippet First_steps/hello_event.f90 example

Let's analyze what happens in each line.
First we have the PDI_init() function, which takes a parameter of type `PC_tree_t`.
This is a tree structure parsed from a YAML file; here we parse it with the `paraconf` library
bundled in %PDI.
To parse a file, we call the `PC_parse_path` function, passing the file path as its argument.
The next step is to trigger an event in %PDI named "Hello World Event".
At the end we have to call PDI_finalize().
The output of this program is presented below:
\snippet First_steps/hello_event.output output
The first line indicates that the plugin has loaded successfully.
The next lines are %PDI messages telling us that it managed to create all descriptors and load all
the plugins that were defined.
Then we have the messages from the loaded Trace plugin, which prints the name of every event it
receives; `pdi_start_timer` and `pdi_stop_timer` are emitted by %PDI itself around the execution.
The following line is from %PDI and indicates that finalization has started and that it will now
deallocate resources.
The last message comes from the destructor of the Trace plugin.



## Hello Data {#fs_hello_data}

In \ref fs_hello_event we learned how to trigger an event.
In this chapter we will see how to share and reclaim data.

First we have to create a specification tree named `hello_data.yml`, declaring both a `data`
section and the Trace plugin:
\snippet First_steps/hello_data.yml example

We have declared the Trace plugin and one descriptor named `world` of integer type.

C source code:
\snippet First_steps/hello_data.c example

Fortran source code:
\snippet First_steps/hello_data.f90 example

Let's analyze new functions:
- `PDI_share` shares access to the variable with %PDI.
  The first argument is a descriptor name and indicates what data we are sharing.
  The second one is a pointer to our variable and the last one is the access direction.
  `PDI_OUT` means data flows from the application to %PDI, `PDI_IN` means it flows from %PDI to the
  program, and `PDI_INOUT` includes both directions.
- `PDI_reclaim` reclaims the share, which means that %PDI will no longer have access to the shared
  variable.
  As an argument it takes the name of the descriptor.

The output from our application:
\snippet First_steps/hello_data.output output

As we can see from the logs above, when we called `PDI_share` the plugin gained access to the shared
variable, and after `PDI_reclaim` the variable was no longer available to it.
The share notification is what lets a plugin operate on the data, according to what has been
declared in the specification tree.

We can achieve the exact same result with `PDI_expose`, which is just a `PDI_share` call
immediately followed by a `PDI_reclaim` one.

C source code:
\snippet First_steps/hello_expose.c share_reclaim

is the same as:
\snippet First_steps/hello_expose.c expose

Fortran source code:
\snippet First_steps/hello_expose.f90 share_reclaim

is the same as:
\snippet First_steps/hello_expose.f90 expose



## Hello Access {#fs_hello_access}

Now we will try to access a descriptor we shared with %PDI.
In this case we do not need any plugin.
We define an integer and a string in our `hello_access.yml`:
\snippet First_steps/hello_access.yml example

C source code:
\snippet First_steps/hello_access.c example

Fortran source code:
\snippet First_steps/hello_access.f90 example


We will focus on the `print_secret_msg` function.
If you do not understand what happens in the `main` function, please see the \ref fs_hello_data
example.
`PDI_access` sets our pointer to the data location.
We need to pass `PDI_IN` because the data flows from %PDI to our application.
We also use `PDI_release` rather than `PDI_reclaim`, because `PDI_reclaim` would end the sharing of
this descriptor, which we only want to do later, in the `main` function.
The output of the program:

\snippet First_steps/hello_access.output output

As you can see, we managed to access the data descriptor from the function only by passing its name
and the correct access direction.



## Hello multi expose {#fs_hello_multi_expose}

In some cases we want to expose several descriptors at once.
For this we have multi expose, which shares all the given descriptors, then triggers the given
event, then reclaims all the data that was passed.
Let's look at the example.
\snippet First_steps/hello_multi_expose.yml example

We have defined 3 descriptors and the Trace plugin.

C source code:
\snippet First_steps/hello_multi_expose.c example

The first argument of `PDI_multi_expose` is the name of the event we want to trigger once all the
descriptors are shared.
After that we pass, repeatedly:

- the name of the descriptor,
- a pointer to the data,
- the access direction.

As the last argument we have to pass `NULL`.

Fortran source code, which has to use a transaction:
\snippet First_steps/hello_multi_expose.f90 example

The output of the execution:

\snippet First_steps/hello_multi_expose.output output

The logs from the Trace plugin confirm the execution order we were expecting.
