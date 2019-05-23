\page First_steps First steps with %PDI

\section fs_hello_event Hello Event
As mentioned in \ref Specification_tree we have to provide specification tree to instuct %PDI what data we will share and what to do with it. We want to show what happens on each %PDI API call. We will use \ref test_plugin, which is very simple plugin that just print every information it gets. Let's create a specification tree named `hello_event.yml' that will load test plugin to %PDI:

```yaml
plugins:
  test: ~
```

Yes, that is whole specification tree. Test plugin prints everything, so there is no need to specify what we want it to do.

Now we need to write a source code of our application:
```C
#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_event.yml"));
    PDI_event("Hello World Event");
    PDI_finalize();
    return 0;
}
```

Let's analyse what happens in each line. Firstly we have `PDI_init` function which take parameter of type `PC_tree_t`. It's a structure parsed from some YAML file, in our case we parse it with `paraconf` library build in %PDI. To parse a file we need to call `PC_parse_path` function passing file path as argument. The next step is to call an event in %PDI named "Hello World Event". At the end we have to call `PDI_finalize()`. The output from this program is presented below:
```
[PDI][Test-plugin][10:25:28] *** info: Welcome to the test plugin!
[PDI][10:25:28] *** info: Initialization successful
[PDI][Test-plugin][10:25:28] *** info: The test plugin received an event: Hello World Event
[PDI][10:25:28] *** info: Finalization
[PDI][Test-plugin][10:25:28] *** info: Goodbye from the test plugin!
```
The first line indicates that plugin has loaded succesfully. The second is %PDI message, that it maneged to create all descriptors and load all defined plugins. Then we have message from loaded test plugin which printed the event name it has received. The next information is from %PDI and indicates that finalization has started and now it will dealocate resources. Last message is from test plugin destructor.

\subsection fs_hello_data Hello Data
In \ref fs_hello_event we learned how to call an event. In this chapter we will see how to share and reclaim data.

Firstly we have to create a specification tree named `hello_data.yml` with `data` and test plugin tree declared:

```yaml
data:
  world: int
plugins:
  test: ~
```

We have declared test plugin and one descriptor named `world` of integer type.
Now let's write our program:

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

Let's analyse new functions:
- PDI_share shares access to the variable with %PDI. The first argument is a descriptor name and indicates what data we are sharing. The second one is pointer to our variable and the last one is access direction. `PDI_OUT` lets plugin to read from the descriptor, `PDI_IN` to write to the descriptor, `PDI_INOUT` gives both rights. 
- PDI_reclaim reclaims the share which means that %PDI will no longer have access to shared variable. As an argument it takes name of the descriptor.

The output from our application:
```
[PDI][Test-plugin][10:59:35] *** info: Welcome to the test plugin!
[PDI][10:59:35] *** info: Initialization successful
[PDI][Test-plugin][10:59:35] *** info: =>> data becoming available to the test plugin: world
[PDI][Test-plugin][10:59:35] *** info: <<= data stop being available to the test plugin: world
[PDI][10:59:35] *** info: Finalization
[PDI][Test-plugin][10:59:35] *** info: Goodbye from the test plugin!
```

As we can see from the logs above, when we called `PDI_share` plugin gained access to the shared variable and after `PDI_reclaim` the variable has become no longer available for the plugin. The share notification gives plugin possibility to operate on data dependently what has been declared in specification tree.

The same exact result we can achive with `PDI_expose` which is just `PDI_share` call and right after `PDI_reclaim` is called.

```
    PDI_share("world", &my_world, PDI_OUT);
    PDI_reclaim("world");
```
is the same as:
```
    PDI_expose("world", &my_world, PDI_OUT);
```

\subsection fs_access Hello Access
Now we will try to access a descriptor we have shared earlier with %PDI. In this case we won't need any plugin. We want to define string in our `world_access.yml`:
```yaml
data:
  my_message: {type: array, subtype: char, size: 32}
```

Now let's write some simple program:
```c

void print_secret_msg() {
    char message[32];
    PDI_access("my_message", (void**)message, PDI_IN);
    printf("%s\n", message);
    PDI_release("my_message");
}

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_data.yml"));
    char* secret_msg = "Watermelon is the best fruit";
    PDI_share("my_message", secret_msg, PDI_OUT);

    print_secret_msg();

    PDI_reclaim("my_message");
    PDI_finalize();
    return 0;
}
```