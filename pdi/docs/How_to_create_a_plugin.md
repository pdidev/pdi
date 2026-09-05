# How to create a plugin {#how_to_create_a_plugin}

## Plugin class {#plugin_class}

PDI::Plugin is a class that handles shared data and triggered events to perform I/O operations.
It is dynamically linked to the user program by %PDI.
The behavior of each plugin is defined by the specification tree in its `plugins` subtree.

```yaml
plugins:
  example:
    # ...
```

The given example loads the `example` plugin and passes its subtree to the plugin's constructor.

A plugin has to inherit from PDI::Plugin and have a constructor taking a PDI::Context& and a
`PC_tree_t` as arguments.

### Example plugin {#example_plugin}

The simplest plugin, that does nothing:

\snippet How_to_create_a_plugin/minimal_plugin.cxx example

### Adding a callback {#adding_callback}

PDI::Context holds all the functions (callbacks) that are called when the user shares data, that is
to say calls `PDI_share`.
To add a new function, the plugin must call PDI::Context::on_data:

\snippet pdi/context.h on_data

The first argument is the function to call when the user shares data.
If the second parameter (`name`) is given, the function is only called for data of that name.
It returns a function that removes the callback from PDI::Context.

The includes every one of the following steps needs:

\snippet How_to_create_a_plugin/example_plugin.cxx includes

Example of adding a new callback, in the constructor of the plugin:

\snippet How_to_create_a_plugin/example_plugin.cxx on_data

If the user creates the following specification tree:

\snippet How_to_create_a_plugin/example_use.yml yaml

and the following program:

\snippet How_to_create_a_plugin/example_use.cxx example

the console displays:

\snippet How_to_create_a_plugin/example_use.output output

### Reading and writing data {#rw_data}

Example of reading and writing data:

\snippet How_to_create_a_plugin/example_plugin.cxx rw_data

### Handling events {#example_events}

\snippet How_to_create_a_plugin/example_plugin.cxx events

with the handlers themselves as members of the plugin:

\snippet How_to_create_a_plugin/example_plugin.cxx event_handlers

### Reading scalar and array from specification tree {#reading_pc_tree}

Specification tree:

\snippet How_to_create_a_plugin/example_plugin.cxx yaml

Reading a scalar and an array:

\snippet How_to_create_a_plugin/example_plugin.cxx conf_scalar_array

### Reading maps from specification tree {#reading_pc_tree_2}

Using the same specification tree as above:

\snippet How_to_create_a_plugin/example_plugin.cxx conf_map

## Creating a true plugin: POSIX plugin {#create_true_plugin}

The fastest way to learn is by example.
To show how to create a plugin, we will create a "posix plugin".
It does nothing special, but it gives the basic knowledge required to create one.

### Step 1: Think about what your plugin is for. {#step_1}

Simple checkpointing.
Each piece of data is saved in a separate file.
The user can check the status of all the files at once and then recover the data.

### Step 2: Prepare your specification tree schema. {#step_2}

\snippet How_to_create_a_plugin/posix.cxx yaml

`/file_path/` is the path where `some_data` is saved to and loaded from.
`can_recover_data` is a flag that indicates whether recovery is possible.

### Step 3: Write your plugin. {#step_3}

Members:

\snippet How_to_create_a_plugin/posix.cxx members

Read the recover tree:

\snippet How_to_create_a_plugin/posix.cxx read_recover_tree

Read the data tree:

\snippet How_to_create_a_plugin/posix.cxx read_data_tree

Create a function that writes the data to a temporary file, checks that the file was created with
the correct size, and then replaces the old file:

\snippet How_to_create_a_plugin/posix.cxx write_data

Create a function that reads the data from a file:

\snippet How_to_create_a_plugin/posix.cxx read_data

Handle the `can_recover_all` data:

\snippet How_to_create_a_plugin/posix.cxx can_recover

Add the functions we created to the callbacks, in the constructor:

\snippet How_to_create_a_plugin/posix.cxx constructor

### Next steps {#plugin_compile}

1. Compile it:
   `g++ posix.cxx -o libpdi_posix_plugin.so -lpdi -shared -fPIC -std=c++20`
2. Copy the file you created to a path where the dynamic linker can find it, for example:
   `sudo cp libpdi_posix_plugin.so /usr/local/lib/`
3. Configure the run-time bindings of the dynamic linker: `sudo ldconfig`
4. Create a program that uses the posix plugin.
5. Compile your test program: `gcc example_use.c -o example_use -lpdi -lparaconf`
6. Run your test program: `./example_use`

You can see an example of a program that uses this plugin on these
[slides](https://docs.google.com/presentation/d/1jT416oALDkquBBgq_XkVrU48o4qx72wGHUPb4emXJw4).
