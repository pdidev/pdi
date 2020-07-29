\page FTI_plugin FTI plugin

**WARNING** This documentation is a work in progress and does not 
reflect the full FTI plugin potential.

The FTI plugin enables one to perform checkpoints using FTI.
FTI plugin does not support the full FTI feature set but offers a simple
declarative interface to access a large subset of it.

## Features and requirements

### Dependencies

FTI plugin uses a `MPI_Comm` datatype and `MPI_COMM_WORLD` descriptor, 
which are defined in `mpi` plugin.
For now it must be loaded alongside FTI plugin (as shown below).

```yaml
pdi:
   ...

   plugins:
      mpi:
      fti:
         ...
```

### Initialization

FTI initialization in FTI plugin is based on several rules:
* if `init_on` is specified, FTI plugin will initialize FTI
  on that exact event and expects the communicator and file name
  to be correctly available or fail otherwise,
* if `init_on` is not specified and the communicator is available
  on plugin initialization (e.g. when using `MPI_COMM_WORLD`
  predefine from `mpi` plugin), FTI plugin will try to initialize
  FTI (on plugin initialization) and expects file name
  to be correctly available or fail otherwise,
* if `init_on` is not specified and the communicator is not
  available on plugin initialization, FTI plugin will try 
  to initialize FTI on communicators descriptor exposure and expects
  file name to be correctly available or fail otherwise.

### Predefined descriptors

FTI plugin loads a predefined descriptor named `FTI_COMM_WORLD` 
(`FTI_COMM_WORLD_F` for Fortran) after a successfull initialization 
of FTI. This descriptor is treated as metadata. Its content can 
be accessed using `PDI_access`.

## Configuration grammar

At its root, the FTI configuration is made of several nodes:
`checkpoint`, `communicator`, `config_file`, `dataset`, `init_on`,
`recover_on`, `recover_var`, `send_file`, `snapshot_on` and `status`.

### checkpoint

A `checkpoint` specifies a list of checkpoints to execute.
It is specified by a key/value map that may contain following keys:
`L1_on`, `L2_on`, `L3_on`, `L4_on`.
Each of the keys specifies events names that trigger a specified checkpoint.
Value of this key can be either string or a list of strings:
* `L1_on`: executes FTI L1 checkpoint on specified PDI events,
* `L2_on`: executes FTI L2 checkpoint on specified PDI events,
* `L3_on`: executes FTI L3 checkpoint on specified PDI events,
* `L4_on`: executes FTI L4 checkpoint on specified PDI events.

### communicator

A `communicator` is a string, which specifies name of descriptor containing 
a MPI communicator to use for FTI initialization. Additionally, FTI plugin can
write `FTI_COMM_WORLD` into this descriptor, if it is shared with write access right.
It defaults to `MPI_COMM_WORLD`, which is a predefined descriptor from `mpi` plugin.

### config_file

A `config_file` is a string that can contain $-expressions. 
Specifies the name of the FTI configuration file.

### dataset

A `dataset` is a key/value map, which keys are integers that specify
datasets ids in FTI and values are names of protected descriptors.
Alternatively, the map may contain a map with keys `name` and `size`,
where `name` is a name of protected descriptor and `size` is a descriptor
where FTI plugin will write size of the protected descriptor when shared 
with write access right.

```yaml
dataset:
  0: {name: variable, size: variable_size}
  1: variable2
```

### init_on

A `init_on` is a string or a list of strings that specifies the names 
of the PDI events that executes recovery of FTI.

### recover_on

A `recover_on` is a string or a list of strings that specifies the names 
of the PDI events that executes recovery of FTI.

### recover_var

A `recover_var` is a list of maps with keys:
- `on_event`
- `var`

`on_event` defines on which events the variable recovery should be done (single event or list of events)

`var` defines IDs of variables to recover (single ID or list of IDs)

```yaml
plugins:
  fti:
    recover_var:
      - on_event: recover_test
        var: 2
      - on_event: [recover_arrays, dummy_event]
        var: [0, 1]
```

### send_file

A `send_file` is a key/value map or a list of key/value maps 
describing source and destination paths of files to send, 
event or list of events to trigger on and optionally name of descriptor, 
where will be stored status information when exposed.
Example:
```yaml
send_file:
  on_even: event
  file:
    src: source_path
    dest: destination_path
    status: status_descriptor
```

### snapshot_on

A `snapshot_on` is a string or a list of strings that specifies the names 
of the PDI events that executes FTI_Snapshot.

### status

A `status` is a string that specifies the name of a descriptor to which FTI
will write FTI_Status code when shared.

## full configuration example

```yaml
pdi:
  metadata: # type of small values for which PDI keeps a copy
    iter: int
    size:  { size: 2, type: int }
  data: # type of values for which PDI does not keep a copy
    fti_status: int
    array_size: int64
    file_name: {size: 64, type: array, subtype: char}
    file_status: int
    file1_status: int
    file2_status: int
    main_array: { size: ['$size[0]', '$size[1]'], type: array, subtype: double }
  
  plugins:
    mpi:
    fti:
      config_file: fti_config.ini
      communicator: MPI_COMM_WORLD
      init_on: fti_init_event
      status: fti_status
      dataset:
        0: {name: main_array, size: array_size}
        1: iter
      snapshot_on: snapshot_event
      recover_on: recover_event
      checkpoint:
        L1_on: ckpt_l1_event
        L2_on: [ckpt_l2_1_event, ckpt_l2_2_event]
        L3_on: [ckpt_l3_1_event, ckpt_l3_2_event]
        L4_on: ckpt_l4_event
      send_file:
        - on_event: send_one_file
          file:
            src: ./$file_name
            dest: ./somewhere_else/$file_name
            status: file_status
        - on_event: [send_two_files_first_event, send_two_files_second_event]
          file:
            - src: ./file1
              dest: ./somewhere_else/file1
              status: file1_status
            - src: ./file2
              dest: ./somewhere_else/file2
              status: file2_status
```
