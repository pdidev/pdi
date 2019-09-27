\page how_to_create_plugin How to create a plugin

\section plugin_class Plugin class

Plugin is a class, that handles shared data and triggered events to perform an I/O operations. It is dynamically linked to the user program by PDI. Behavior of each plugin is defined by specification tree in plugins subtree.

```yaml
plugins:
  example:
    ...
```
The given example will load example plugin and pass its subtree to the plugins constructor.

The plugin has to inherit from PDI::Plugin and have a constructor with arguments PDI::Context& and PC_tree_t.

\subsection example_plugin Example plugin
Example of the simplest plugin, that does nothing:

```cpp
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {}
}

PDI_PLUGIN(example)
```

\subsection adding_callback Adding a callback
PDI::Context has a container with all the functions (callbacks) that are called when user is sharing the data (calls PDI_share). To add a new function to this container the plugin must call add_data_callback.

```cpp
std::function<void()> add_data_callback(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {}))
```

The first argument is the function to be called when user shares the data.
If the second parameter given is the function, that will be called only on specified data name.
Returns a function that removes the callback from PDI::Context container.

Example of adding new callback:
```cpp
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        ctx.add_data_callback([](const std::string& data_name, PDI::Ref ref){
            std::cout << "User has shared a data named " << data_name << std::endl;
        });
    }
}

PDI_PLUGIN(example)
```
If user create specification tree:
```yaml
data:
    some_integer: int

plugins:
    example: ~
```
And a program:
```cpp
#include <pdi.h>

int main()
{
    PDI_init(PC_parse_path("spec_tree.yaml));
    int some_integer = 0;
    PDI_expose("some_integer", &some_integer, PDI_OUT);
    PDI_finalize();
    return 0;
}
```
The console will display:
```
[PDI][13:42:41] *** info: Initialization successful
User has shared a data named some_integer
[PDI][13:42:42] *** info: Finalization
```

\subsection rw_data Reading and writing data
Example of reading and writing data:
```cpp
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        ctx.add_data_callback([](const std::string& data_name, PDI::Ref ref){
            if(PDI::Ref_rw ref_rw{ref}) {
                //Plugin can read and write
                int* some_integer = ref_rw.get();
            } else if(PDI::Ref_r ref_r{ref}) {
                //Plugin can read
                const int* some_integer = ref_r.get();
            } else if(PDI::Ref_w ref_w{ref}) {
                //Plugin can write
                int* some_integer = ref_w.get();
            } else {
                //Plugin cannot read nor write
            }
        });
    }
}

PDI_PLUGIN(example)
```

\subsection example_events Handling events
```cpp
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        ctx.add_event_callback([this](const std::string& event_name){
            this->handle_event(event_name);
        });
        ctx.add_event_callback([this](const std::string& event_name){
            this->handle_special_event(event_name);
        }, "special_event");
    }
private:
    void handle_event(const std::string& event_name) {
        std::cout << "Event" << event_name << "called." << std::endl;
    }

    void handle_special_event(const std::string& event_name) {
        std::cout << "Special event `" << event_name << "'called." << std::endl;
    } 
}

PDI_PLUGIN(example)
```

\subsection reading_pc_tree Reading scalar and array from specification tree
Specification tree:
```yaml
plugins:
  example:
    scalar: some_string
    array: [0, 1, 2]
```
Reading a scalar and an array:
```cpp
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        //scalar
        PC_tree_t scalar_tree = PC_get(spec_tree, ".scalar");
        std::string string_scalar = PDI::to_string(scalar_tree);

        //array
        PC_tree_t array_tree = PC_get(spec_tree, ".array");
        int array_size = PDI::len(array_tree);
        std::vector<long> array;
        for (int i = 0; i < array_size; i++) {
            PC_tree_t array_element = PC_get(array_tree, "[%d]", i);
            array.emplace_back(PDI::to_long(array_element));
        }
    }
}

PDI_PLUGIN(example)
```
\subsection reading_pc_tree_2 Reading maps from specification tree
Specification tree:
```yaml
plugins:
  example:
    custom_subtree:
        here: 0
        can: 1
        be: 2
        any: 3
        subtree: 4
```
Reading a scalar and an array:
```cpp
#include <unordered_map>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct example_plugin : public PDI::Plugin
{
    example_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        PC_tree_t subtree = PC_get(spec_tree, ".custom_subtree");
        int subtree_size = PDI::len(subtree);
        std::unordered_map<std::string, long> custom_map;

        for (int i = 0; i < subtree_size; i++) {
            PC_tree_t key = PC_get(subtree, "{%d}", i);
            PC_tree_t value = PC_get(subtree, "<%d>", i);
            custom_map.emplace_back(PDI::to_string(key), PDI::to_long(value));
        }
    }
}

PDI_PLUGIN(example)
```

\section create_true_plugin Creating a true plugin: POSIX plugin

\subsection step_1 Step 1: Think what your plugin will be for.

Simple checkpointing. Each data will be saved in separate file. User can check the status of all files at once and then recover the data.

\subsection step_2 Step 2: Prepare your specification tree schema.
```yaml
data:
  some_data: {type: array, subtype: int, size: 64}
  can_recover_data: int

plugins:
  posix:
    data:
      some_data: /file_path/
    can_recover_all: can_recover_data
```
/file_path/ is a path where to save and load from some_data. can_recover_data is a flag that indicates if the recover is possible.

The fastest way to learn is by examples. To show how to create a plugin, we will create "posix plugin". It won't do anything special, but give you a basic knowledge how to create one.

\subsection step_3 Step 3: Write your plugin.
Members and constructor:
```cpp
class posix_plugin : public PDI::Plugin
{
    std::string m_can_recover_data;
    std::unordered_map<std::string, std::string> m_data_to_path_map;

public:
    posix_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {}
}

PDI_PLUGIN(posix)
```

Read recover tree:
```cpp
void read_recover_tree(PC_tree_t spec_tree) {
    PC_tree_t recover_tree = PC_get(spec_tree, ".can_recover_all");
    if(!PC_status(recover_tree)) {
        m_can_recover_data = PDI::to_string(recover_tree);
    }
}
```

Read data tree:
```cpp
void read_data_tree(PC_tree_t spec_tree) {
    PC_tree_t data_tree = PC_get(spec_tree, ".data");
    if (!PC_status(data_tree)) {
        int data_tree_size = PDI::len(data_tree);
        for (int i = 0; i < data_tree_size; i++) {
            PC_tree_t key = PC_get(data_tree, "{%d}", i);
            PC_tree_t value = PC_get(data_tree, "<%d>", i);
            m_data_to_path_map.emplace_back(PDI::to_string(key), PDI::to_string(value));
        }
    }
}
```

Create a function that writes data to temporary file and check if file was created, size is correct and then replace old file:
```cpp
void write_data(const std::string& data_name, PDI::Ref_r ref_r) {
    if(!ref_r) {
        return;
    }
    std::string tmp_path = m_data_to_path_map[data_name] + ".tmp";
    std::ofstream file{tmp_path, std::ios::binary};
    if (ref_r.type().buffersize() == ref_r.type().datasize()) {
        //dense data
        file.write((const char*)ref_r.get(), ref_r.type().buffersize());
    } else {
        //sparse data
        std::unique_ptr<char> data_copy {new char[ref_r.type().dataSize()]};
        ref_r.type().data_to_dense_copy(data_copy.get(), ref_r.get());
        file.write(data_copy.get(), ref_r.type().datasize());
    }
    file.close();

    //replace old file
    struct stat status;
    if (!stat(tmp_path.c_str(), &status) && status.st_size == ref_r.type().datasize()) {
        if (!stat(m_data_to_path_map[data_name].c_str()) && std::remove(m_data_to_path_map[data_name].c_str())) {
            throw PDI::Error {PDI_ERR_SYSTEM, "Cannot remove old file %s", m_data_to_path_map[data_name].c_str()};
        }
        if (std::rename(tmp_path.c_str(), m_data_to_path_map[data_name].c_str())) {
            throw PDI::Error {PDI_ERR_SYSTEM, "Cannot rename temporary file %s", tmp_path.c_str()};
        }
    } else {
        throw PDI::Error {PDI_ERR_SYSTEM, "Data write not complete"};
    }
}

```

Create a function that reads data from file:
```cpp
void read_data(const std::string& data_name, PDI::Ref_w ref_w) {
    if(!ref_w) {
        return;
    }
    std::ifstream file{m_data_to_path_map[data_name], std::ios::binary};
    if (ref_w.type().buffersize() == ref_w.type().datasize()) {
        //dense data
        file.read((char*)ref_w.get(), ref_w.type().buffersize());
    } else {
        //sparse data
        std::unique_ptr<char> data_copy {new char[ref_w.type().dataSize()]};
        file.read(data_copy.get(), ref_w.type().datasize());
        ref_w.type().data_from_dense_copy(ref_w.get(), data_copy.get());
    }
}
```

Handle can_recover_all data:
```cpp
void can_recover(const std::string& data_name, PDI::Ref_w ref_w) {
    if (!ref_w) {
        throw PDI::Error {PDI_ERR_RIGHT, "Cannot write to `can_recover_all' data"};
    }
    for (const auto& data_path_pair : m_data_to_path_map) {
        struct stat status;
        if (stat(data_path_pair.second.c_str(), &status)) {
            *static_cast<int*>(ref_w.get()) = 0;
            return;
        }
    }
    *static_cast<int*>(ref_w.get()) = 1;
}
```

Add created functions to callbacks:
```cpp
class posix_plugin : public PDI::Plugin
{
    std::string m_can_recover_data;
    std::unordered_map<std::string, std::string> m_data_to_path_map;

public:
    posix_plugin(PDI::Context& ctx, PC_tree_t spec_tree):
        Plugin{ctx}
    {
        read_recover_tree(spec_tree);
        read_data_tree(spec_tree);
        for (const auto& data_path_pair : m_data_to_path_map) {
            ctx.add_data_callback([this](const std::string& data_name, PDI::Ref ref) {
                this->write_data(data_name, ref);
            }, data_path_pair.first);
            ctx.add_data_callback([this](const std::string& data_name, PDI::Ref ref) {
                this->read_data(data_name, ref);
            }, data_path_pair.first);
        }
        if (!m_can_recover_data.empty()) {
            ctx.add_data_callback([this](const std::string& data_name, PDI::Ref ref) {
                this->can_recover(data_name, ref);
            }, m_can_recover_all_data);
        }
    }
}
```

\subsection plugin_compile Next steps
1. Compile: 
g++ posix.cxx -o libpdi_posix_plugin.so -lpdi -shared -fPIC -std=c++11

2. Copy created file to path where dynamic linker can find it. For example:
sudo cp libpdi_posix_plugin.so /usr/local/lib/

3. Configure dynamic linker run-time bindings:
sudo ldconfig

4. Create program that uses posix plugin.

5. Compile your test program:
gcc example_use.cxx -o example_use -lpdi -lparaconf

6. Run your test program:
./example_use

You can see example of the program that uses this plugin on this [slides](https://docs.google.com/presentation/d/1jT416oALDkquBBgq_XkVrU48o4qx72wGHUPb4emXJw4).