\page FlowVR_plugin The FlowVR plugin

The [FlowVR](http://flowvr.sourceforge.net) plugin lets you write FlowVR modules
 without knowing any specific FlowVR API calls. Properly created %PDI configuration
  file allows you to care only about proper input/output calls, but this plugin does
  not support the full FlowVR feature set.

\section configuration Configuration elements

\subsection flowvr_tree FlowVR plugin tree

The root of FlowVR plugin configuration (named `flowvr`), is a dictionary that contains the following keys:
- \ref component_node
- \ref wait_on_data_node (*recommended*)
- \ref wait_on_node (*optional*)
- \ref status_node (*optional*)
- \ref abort_on_node (*optional*)
- \ref init_on_node (*optional*)
- \ref input_ports (*optional*)
- \ref output_ports (*optional*)

\subsubsection component_node component

For now only available component is `module`, but this can be expanded to `filter` and `synchronizer` in the future.
Configuration example:

```yaml
plugins:
  flowvr:
    component: module
```

\subsubsection wait_on_data_node wait_on_data

 The plugin will call `wait` funciton every time given descriptors will be shared with `PDI_IN`
 access direction. This descriptor must be an integer type and the status returned from this call
 will be written as a response.
 Configuration example:

```yaml
data:
  wait_desc: int

plugins:
  flowvr:
    component: module
    wait_on_data: wait_desc
```

\subsubsection wait_on_node wait_on

Defines on which events the plugin calls the `wait` function. The value can be either single event
name or the array of events names (both examples presented below). This method of calling `wait`
should be avoided if possible, because it's not returning the wait status. Configuration examples:

```yaml
plugins:
  flowvr:
    component: module
    wait_on: "wait_event"
```

```yaml
plugins:
  flowvr:
    component: module
    wait_on: ["wait_event_1", "wait_event_2"]
```

\subsubsection status_node status

%PDI will copy the status of the module to the given descriptor. The same as the `wait_on` it can be a single
name or an array of names. Configuration example:

```yaml
data:
  wait_desc: int
  status_desc: int

plugins:
  flowvr:
    component: module
    wait_on_data: wait_desc
    status: status_desc
```

\subsubsection abort_on_node abort_on

Defines on which events the plugin calls the `abort` function which will stop the flowvr application.
The value can be either single event name or the array of events names. Configuration examples:

```yaml
data:
  wait_desc: int

plugins:
  flowvr:
   component: module
    wait_on_data: wait_desc
    abort_on: "abort_event"
```

\subsubsection init_on_node init_on

Defining this subtree tells %PDI that the plugin should be initialized not on `PDI_init`, but on given event.

```yaml
data:
  wait_desc: int

plugins:
  flowvr:
   component: module
    wait_on_data: wait_desc
    init_on: "init_event"
```

\subsubsection parallel_node parallel

This node can be for reading rank and size of the world, but also can be for setting this values.
`get_rank` will copy process's rank to given descriptor on share, `get_size` will save the 
world size the same way. Example:
```yaml
data:
  wait_desc: int
  rank_desc: int
  size_desc: int

plugins:
  flowvr:
    component: module
    wait_on_data: wait_desc
    parallel:
      get_rank: rank_desc
      get_size: size_desc
```

Values (use `$` expression) from descriptors defined in `set_rank` and `set_size` will be passed
to flowvr and set the environment. Setting this parameters must occur before plugin initialization. For this
you need to use `init_on` event and call this event after setting rank and size.

```yaml
data:
  wait_desc: int
  rank_desc: int
  size_desc: int

plugins:
  flowvr:
    component: module
    wait_on_data: wait_desc
    parallel:
      set_rank: $rank_desc
      set_size: $size_desc
```

\subsubsection several_modules Several modules

Single program can run several modules at once. To make it work just create array of modules in `flowvr` tree.
Configuration example:

```yaml
data:
  wait_1: int
  wait_2: int

flowvr:
  - component: module
    wait_on_data: wait_1
    ...
  - component: module
    wait_on_data: wait_2
    ...
```

\subsection ports Ports

\subsubsection input_ports input_ports

Input ports are defined in `input_ports` tree. Each port is defined by name and \ref message it will receive.
You can add `event_port: true` to define this as event port (non-blocking).

```yaml
flowvr:
  ...
  input_ports:
    port_name_1:
      ...
    port_name_2:
      event_port: true
      ...
```

\subsubsection output_ports output_ports

Output ports are defined in `output_ports` tree. Each port is defined by name and \ref message it will send.
```yaml
flowvr:
  ...
  output_ports:
    port_name_1: ...
    port_name_2: ...
```

\subsection message Message

Flowvr message consists of payload and stamps.
The `payload` is defined by user by a specific key in port definition:

|key|message type|
|:----|:----|
|"data"|\ref data_payload|
|"chunks"|\ref chunks_payload|
|"event_button"|\ref button_payload|
|"event_mouse"|\ref mouse_payload|

If none of the message type will be given, plugin will create a STAMP port (message has no payload).
`stamps` are described in \ref stamp section.

\subsubsection data_payload Data payload
Requires `data` key in a port tree. This configuration means that module will send simple buffer.
The plugin doesn't know the type of sending data. The value of `data` key is the name of
the descriptor where:

- for input port - FlowVR plugin will write to received data
- for output port - FlowVR plugin will read from to send data

Example of `data` in output port:

```yaml
data:
  data_desc: {type: array, subtype: int, size: 32}

flowvr:
  ...
  output_ports:
    some_port_name:
      data: data_desc
```

The user can not always predict how many data will receive. In this case
`size` node type must be a name of metadata where to write the size of received payload.
This metadata will hold the size of received array. The `size` property of descriptor
should be divided by size of single element. Example of receiving unknown payload size:

```yaml
metadata:
  received_size : int
data:
  data_descriptor_name: {type: array, subtype: int, size: $received_size/4}

plugins:
  flowvr:
    ...
    input_ports:
      some_port_name:
        data: data_descriptor_name
        size: received_size
```

In case that you want to send only a part of the data from descriptor you can specify it
in `copy_data_selection` tree defining datatype the same way as for descriptors. Simple example:

```yaml
data:
  data_descriptor_name:  {type: array, subtype: int, size: [32, 32]}

flowvr:
  ...
  output_ports:
    some_port_name:
      data: data_descriptor_name
      copy_data_selection:
        type: array
        subtype: int
        size: [32, 32]
        subsize: [30, 30]
        start: [1, 1]
```

\subsection chunks_payload Chunk payload

Requires `chunks` key in a port tree. This configuration means that module will send several buffers in
one payload. Value of the `chunks` key is the list of \ref data_payload.

---
WARNING

After each wait, the first access to the any descriptor (that belongs to the chunks)
will allocate the memory for the all descriptors. This means that all sizes
(if stored as metadata) should be set before first access to any of the descriptors.

---

```yaml
data:
  chunk_1_name: int
  chunk_2_name: {type: array, subtype: char, size: 255}
  chunk_3_name: float

flowvr:
  ...
  output_ports:
    some_port_name:
      chunks:
        - data: chunk_1_name
        - data: chunk_2_name
        - data: chunk_3_name
```

\subsection button_payload Event button payload

Requires `event_button` key in a port tree. The payload holds the values of the keyboard keys
pressed during iteration.

The full list of predefined keys:

|key in configuration|key on keyboard|
|:----|:----|
|KEY_F1|F1|
|KEY_F2|F2|
|KEY_F3|F3|
|KEY_F4|F4|
|KEY_F5|F5|
|KEY_F6|F6|
|KEY_F7|F7|
|KEY_F8|F8|
|KEY_F9|F9|
|KEY_F10|F10|
|KEY_F11|F11|
|KEY_F12|F12|
|KEY_UP|Up Arrow|
|KEY_DOWN|Down Arrow|
|KEY_LEFT|Left Arrow|
|KEY_RIGHT|Right Arrow|
|KEY_PAGE_UP|Page Up|
|KEY_PAGE_DOWN|Page Down|
|KEY_HOME|Home|
|KEY_END|End|
|KEY_INSERT|Insert|

Value for each key in configuration is a descriptor of integer type. You need to make sure that
you are setting this descriptors with correct values on each iteration. Example of configuration with
arrow buttons:

```yaml
data:
  up: int
  down: int
  left: int
  right: int

flowvr:
  ...
  output_ports:
    some_port_name:
      event_button:
        KEY_UP: up
        KEY_DOWN: down
        KEY_LEFT: left
        KEY_RIGHT: right
```

\subsection mouse_payload Event mouse payload

Requires `event_mouse` key in a port tree. The payload holds the values of the mouse keys
pressed and cursor position during iteration. The button state is saved in descriptors of integer type
and a cursor position in an array of 2 floats.

Available keys in `event_mouse` tree:
|key|description|
|:----|:----|
|POS_XY|cursor position|
|LEFT_BUTTON|left mouse button|
|MIDDLE_BUTTON|middle mouse button|
|RIGHT_BUTTON|right mouse button|

 Example of configuration with all keys defined:

```yaml
data:
  pos_xy: {type: array, subtype: float, size: 2}
  left_button: int
  right_button: int
  middle_button: int

flowvr:
  ...
  output_ports:
    some_port_name:
      event_mouse:
        POS_XY: pos_xy
        LEFT_BUTTON: left_button
        RIGHT_BUTTON: right_button
        MIDDLE_BUTTON: middle_button
```

\subsection stamp Stamp

The `stamps` key must be defined in a port tree. The value of `stamps` is simply a map with the stamp
name and descriptor name. The given descriptor must have a valid %PDI type, limited to:

- `int`
- `float`
- `array of ints`
- `array of floats`
- `array of chars`


Example of message with \ref data_payload and 2 stamps:

```yaml
data:
  data_desc: {type: array, subtype: int, size: 256}
  stamp_1_desc: int
  stamp_2_desc: {type: array, subtype: float, size: 2}

flowvr:
  ...
  output_ports:
    some_port_name:
      data: data_desc
      stamps:
        stamp_1_name: stamp_1_desc
        stamp_2_name: stamp_2_desc
```

For output port stamp can be also define as an expression, but stamp will need type definition in this case.
Example of expression stamp:

```yaml
metadata:
  value_desc: int

flowvr:
  ...
  output_ports:
    some_port_name:
      stamps:
        stamp_name:
          type: int
          expression: ($value_desc/2) * 3
```

For now only int and string stamps are supported as expression.

\section c_api Reading and writing data examples

FlowVR plugin uses 2 ways to handle data reading and writing:

1. Access the shared memory - the fastest way, user operates on flowvr shared memory.

2. Copy the data from shared memory to descriptor - needs the data copy (convenient for small
 messages or sparse data types).

\subsection read_shared Read data from FlowVR message by access the shared memory

```yaml
data:
  text_shr: {type: array, subtype: char, size: 4}

...
    input_ports:
      text:
        data: text_shr
```

```cpp
char* text_shr;
PDI_access("text_shr", (void**)&text_shr, PDI_IN);

// do something with text_shr or copy data to local buffer

PDI_release("text_shr"); // really important to release descriptors
```

\subsection write_shared Write data from FlowVR message by access the shared memory

```yaml
data:
  text_shr: {type: array, subtype: char, size: 4}

...
    output_ports:
      text:
        data: text_shr
```

```cpp
char* text_shr;
PDI_access("text_shr", (void**)&text_shr, PDI_OUT);

// do something with text_shr or copy data from local buffer

PDI_release("text_shr"); // really important to release descriptors
```

\subsection read_copy Read data from FlowVR message by copy from the shared memory

```yaml
data:
  text: {type: array, subtype: char, size: 4}

...
    input_ports:
      text:
        data: text
```

```cpp
char text[4];
PDI_expose("text", text, PDI_IN);
```

\subsection write_copy Write data to FlowVR message by copy to the shared memory

```yaml
data:
  text: {type: array, subtype: char, size: 4}

...
    output_ports:
      text:
        data: text
```

```cpp
char text[4];
PDI_expose("text", text, PDI_OUT);
```

\subsection wrtie_sparse Write data to FlowVR message by copy the subset of data to the shared memory

```yaml
data:
  my_array: {type: array, subtype: int, size: [20, 20]}

...
    output_ports:
      text:
        data: my_array
        copy_data_selection: {type: array, subtype: int, size: [20, 20], subsize: [10, 10], start: [0, 0]}
```

```cpp
int my_array[400];
PDI_expose("my_array", my_array, PDI_OUT); // copies only 100 elements
```

\section Reading and writing stamps examples

Stamps are always copied from descriptor to flowvr message.

\subsection read_stamp Read stamp from FlowVR message

```yaml
data:
  stamp_it: int
...
    input_ports:
      text:
        stamps:
          it: stamp_it
```

```cpp
int stamp_it = some_value;
PDI_expose("stamp_it", &stamp_it, PDI_IN);
```

\subsection write_stamp Write stamp from FlowVR message

```yaml
data:
  user_stamp: int

...
    output_ports:
      text:
        stamps:
          user_defined_stamp: user_stamp
```

```cpp
int user_stamp = some_value;
PDI_expose("user_stamp", &user_stamp, PDI_OUT);
```

\section mouse_button_event Reading and writing mouse and button event examples

\subsection write_mouse Write mouse event from FlowVR message

```yaml
data:
  pos_xy: {type: array, subtype: float, size: 2}
  left_button: int

...
    output_ports:
      keysOut:
        event_mouse:
          POS_XY: pos_xy
          LEFT_BUTTON: left_button
```

```cpp
float pos_xy[2] = {x_pos, y_pos};
PDI_expose("pos_xy", &pos_xy, PDI_OUT);

int left_button = 1;
PDI_expose("left_button", &left_button, PDI_OUT);
```

\subsection read_mouse Read mouse event from FlowVR message

```yaml
data:
  pos_xy: {type: array, subtype: float, size: 2}
  left_button: int

...
input_ports:
  keysIn:
    event_mouse:
      POS_XY: pos_xy
      LEFT_BUTTON: left_button
```

```cpp
float pos_xy;
PDI_expose("pos_xy", &pos_xy, PDI_IN);

int left_button;
PDI_expose("left_button", &left_button, PDI_IN);
```

\subsection write_button Write button event from FlowVR message

```yaml
data:
  up: int
  down: int

...
    output_ports:
      keysOut:
        event_button:
          KEY_UP: up
          KEY_DOWN: down
```

```cpp
int up_state = 1;
PDI_expose("up", &up_state, PDI_OUT);

int down_state = 1;
PDI_expose("down", &down_state, PDI_OUT);
```

\subsection read_button Read button event from FlowVR message

```yaml
data:
  up: int
  down: int

...
    input_ports:
      keysIn:
        event_button:
          KEY_UP: up
          KEY_DOWN: down
```

```cpp
int up_state;
PDI_expose("up", &up_state, PDI_IN);

int down_state;
PDI_expose("down", &down_state, PDI_IN);
```

\section flowvr_examples FlowVR examples reworked for %PDI

Path to the examples:

```path
  pdi/build/pdi_plugin-flowvr/src/FLOWVR_PLUGIN-build/examples/
```

Original flowvr source files are in directories `flowvr_original`.

\subsection run_app Running the application

1. Go to examples folder: `cd pdi_plugin-flowvr/src/FLOWVR_PLUGIN-build/examples`
2. Run `source flowvr-config.sh`. Now your environment is ready.
3. Run flowvr daemon on your system (best in new terminal, repeat 1. and 2.): `flowvrd --top`
4. Inside `$example_name` directory generate the flowvr configuration files by: `python $example_name.py` and run example by: `flowvr $example_name`

\subsection tictac Tictac example

Consists of 2 modules:

- `put`
  - source file: `put.cxx`
  - configuration file: `put.yml`
  - has `output port` named `text`
  - sends message with:
    - data: `type: {type: array, subtype: char, size: 4}  #"tic" or "tac"`
    - stamps:
      - it (predefined flowvr stamp): `type: int`
      - my_stamp (user define int array stamp): `type: {type: array, subtype: int, size: 2}`

- `get`
  - source file: `get.cxx`
  - configuration file: `get.yml`
  - has `input port` named `text`
  - receive message sent by `put` module
  - additionally has defined `stamp`:
    - source (predefined flowvr stamp): `type: {type: array, subtype: char, size: 256}`

**Network of the application:**

\image html tictac_net.jpg

\subsubsection bundle Bundle example

Consists of 3 modules:

- 2x `putMulitple`
  - source file: `putMulitple.cxx`
  - configuration file: `putMulitple.yml`
  - first one has `output port` named `text`, second `text2`
  - sends message with:
    - data: `type: {type: array, subtype: char, size: 4} # first "tic" or "tac", second "TIC" or "TAC"`
    - stamps:
      - `it` (predefined flowvr stamp): `type: int`
- `getMulitple`
  - source file: `getMulitple.cxx`
  - configuration file: `getMulitple.yml`
  - has `input ports` named `text` and `text2`
  - receive message sent by `putMulitple` modules
  - additionally has defined `stamp`:
    - source (predefined flowvr stamp): `type: {type: array, subtype: char, size: 256}`

**Network of the application:**

\image html bundle_net.jpg

\subsection primes Primes example

Consists of 3 modules:

- `capture`
  - source file: `capture.cxx`
  - configuration file: `capture.yml`
  - has `output port` named `keysOut`
  - sends message with:
    - `event_button` (payload):
          - KEY_UP: up  
          - KEY_DOWN: down
          - KEY_LEFT: left
          - KEY_RIGHT: right

- `compute`
  - source file: `compute.cxx`
  - configuration file: `compute.yml`
  - has `output port` named `primesOut`
  - sends message with:
    - data (payload): `type: {type: array, subtype: int, size: $tempPrimeNumbersMaxCount}`
    - stamps:
      - computationTimeIt: `type: int`

- `visu`
  - source file: `visu.cxx`
  - configuration file: `visu.yml`
  - has `input ports` named `primesIn` and `keysIn`
  - receive message sent by `capture` and `compute` modules

**Network of the application:**

\image html primes_net.jpg

\subsection fluid Fluid example

Consists of 2 modules:

- `fluid`
  - source file: `fluid.cxx`
  - configuration file: `fluid.yml`
  - has `input port` named `position`
  - received message sent by `gldens` module
  - has `output ports` named `velocity` and `density`
  - `velocity` and `density` both sends message with:
    - data (payload): `type: {type: array, subtype: char, size: [$NX * 2, $NY]} # where NX and NY is metadata`
    - stamps:
      - P: `type: {type: array, subtype: int, size: 2}`
      - N: `type: {type: array, subtype: int, size: 2}`
- `gldens`
  - source file: `gldens.cxx`
  - configuration file: `gldens.yml`
  - has `input ports` named `velocity` and  `density`
  - received message sent by `fluid` module
  - has `output port` named `position`
  - sends message with:
    - data (payload): `type: {type: array, subtype: float, size: 3}`
  - very important is how to get a number of elements received by `velocity` and  `density` ports:
    - `type: {type: array, subtype: char, size: $velocitySize} # velocitySize is defined as metadata`
    Here `velocitySize` must be preceded with `$` to let plugin to write the size there. The `velocitySize` descriptor will store a valid size **after** accessing the `velocity` descriptor, because only then plugin will write size.

**Network of the application:**

\image html fluid_net.jpg
