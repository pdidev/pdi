# The VeloC plugin

The VeloC plugin enables  

* writing and reading generic-data checkpoint files using [VeloC memory-based API](https://veloc.readthedocs.io/en/latest/api.html#example)
* writing and reading applications' custom checkpoint files using [VeloC file-based API](https://veloc.readthedocs.io/en/latest/api.html#example#memory-based-api)
* handling the persistence, versioning, and transfer of checkpoint files using [VeloC](https://veloc.readthedocs.io/en/latest/) **in synchronous mode only**. 

Please note: <br> 
* for applications's customs checkpoints, VeloC does not automate the serialization or deserialization of data structure like it does for generic-data checkpoints; that remains manual and user‑defined. (See [VeloC documentation](https://veloc.readthedocs.io/en/latest/)) <br>
* VeloC's own configuration file requires the definition of a temporary storage directory `scratch` and a persistent one `persistent`. After the program's execution, users will find the checkpoint files in : `persistent`with the naming convention: `<label>-<rank>-<version>.dat`



The VeloC plugin does not currently support the full set of features of VeloC, but it offers a simple
declarative interface to access the core features, as explained in the following paragraphs. 

## External Dependencies   
In order to use the VeloC plugin, the user needs to install VeloC 1.8 or above as it is not vendored in PDI. This can be done following the instructions [here](https://veloc.readthedocs.io/en/latest/quick.html#download-and-install). <br>

The user also needs an MPI implementation, such as 
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.3.2 or above with --with-ch4-shmmods configure options to enable shared memory support. Please note the [debian mpich package](https://tracker.debian.org/pkg/mpich) for versions above 4.3.2 does not build mpich with shared memory support and is therefore not suitable for the use of the plugin. 
## Configuration grammar

### Mandatory Mappings

The VeloC plugin specification tree requires 3 mandatory mappings for successful initialization. 

* *config_file* : path to [VeloC's own configuration file](https://veloc.readthedocs.io/en/latest/userguide.html#setup). <br> Please note the plugin currently only works in synchronous mode. This means the parameter "mode" should be set to "sync" in VeloC's configuration file when using the plugin. 

```yml
config_file: ./veloc_config.cfg 
```
* *checkpoint_label*: label used by VeloC to name checkpoint files. When using the VeloC plugin to checkpoint an application, the user may set the *checkpoint_label* equal to "myapp". If a failure occurs and the user wishes to restart the simulation from a checkpoint, the specification tree used in the new simulation run must have the *checkpoint_label* also equal to "myapp". 

```yml
checkpoint_label: myapp
```

* *iteration*: name of the variable corresponding to the simulation loop's iterator in the PDI data store. This is also used by VeloC to name checkpoint files. 

```yml
iteration: ii
```

### Optional Mappings 
* *status* : name of an integer variable in the PDI data store that represents whether a recovery is needed (status = 0) or not (status = 1). 
```yml
status: checkpoint_status
```
The plugin by default defines the status equal to 1 but users can change the status by writing to PDI.
```cpp 
PDI_expose("status", &checkpoint_status, PDI_OUT);
``` 
If users wish to inspect the status they can read it from PDI. 
```cpp 
PDI_expose("status", &checkpoint_status, PDI_IN);
``` 

* *counter* : name of an integer variable in the PDI data store that represents the number of checkpoints written by the plugin.

```yml
counter: checkpoint_counter
```

If users wish to inspect this value they can read it from PDI. 
```cpp 
PDI_expose("counter", &checkpoint_counter, PDI_IN);
```

### Checkpointing Behaviors

The user can define a *managed_checkpointing* tree or a *custom_checkpointing* tree. 

#### Managed Checkpointing
*managed_checkpointing* requires the following mappings: 

* *protected_data* : list of data structures to be checkpointed during a checkpoint event or to be recovered during a recover event (specify the same names as defined in the PDI data store). This list must include the simulation's iteration counter.

* *checkpoint_on_event* : name of the PDI event where the user wants to checkpoint

* *recover_on_event* : name of the PDI event where the user wants to recover

* *synchronize_on_event* : name of the PDI event where the user wants to synchronize. If the *status* is equal to 0, the synchronization event will be a recovery event, if the *status* is equal to 1, the synchronization event will be a checkpoint event.

* *when* : PDI expression indicating when to execute a checkpoint operation. It is evaluated both in the case of a checkpoint event and of a synchronization event that performs a checkpoint. 

*managed_checkpointing* accepts the following optional mapping

  * *recover_from_iteration* : The iteration from which the user wants to restore. For example, if the user sets this key to 20, the plugin will restore the checkpoint written at iteration 20.

```yml
managed_checkpointing:
        protect_data: [iter, main_field,elapsed_offset] # data to be checkpointed/recovered, iter must be included
        synchronize_on_event: newiter # name of PDI_event where to check the status and consequently checkpoint/recover  
        when : '$iter % 1000 = 0' 
        recover_from_iteration : 3000
```

#### Custom Checkpointing
*custom_checkpointing* requires the following mappings:

* *veloc_file* : name of a char array variable in the PDI data store with a maximum size equal to 256. This variable is used by VeloC to map an input filename to the actual routed file path managed by VeloC.


* *custom_checkpoint* : sub-tree that requires the following mappings 

  * *filename* : user-defined name of the checkpoint file 
  * *start_on_event* : name of the PDI event where the user wants to start a checkpoint phase 
  * *route_file_on_event* : name of the PDI event where the user wants to map the *filename* to the routed file path managed by VeloC. 
  * *end_on_event* : name of the PDI event where the user wants to end a checkpoint phase 

  All checkpointing logic must be placed by the user after the "route_file_on_event" event and before the "end_on_event" event. 

*  *custom_recover* : sub-tree that requires the following mappings 

   * *filename* : user-defined name of the checkpoint file to be used for recovering 
   * *start_on_event* : name of the PDI event where the user wants to start a recovery phase 
   * *route_file_on_event* : name of the PDI event where the user wants to map the *filename* to the actual routed file path managed by VeloC. 
   * *end_on_event* : name of the PDI event where the user wants to end a recovery phase 

   *custom_recover* accepts the following optional mapping

   * *recover_from_iteration* : The iteration from which the user wants to restore. For example, if the user sets this key to 20, the plugin will restore the checkpoint written at iteration 20.


  All recovery logic must be placed by the user after the "route_file_on_event" event and before the "end_on_event" event. 

```yml
custom_checkpointing:
  veloc_file: veloc_file
  custom_checkpoint:
    original_file: file1.h5
    start_on: start
    route_file_on: route
    end_on: end
  custom_recover:
    original_file: file1.h5
    start_on: start
    route_file_on: route
    end_on: end
    checkpoint_nr : 20
```
    
## Use Case to Avoid 
When using [VeloC memory-based API](https://veloc.readthedocs.io/en/latest/api.html#example), therefore when using the *managed_checkpointing* behaviour of the plugin, VeloC binds data structures to the memory address provided during the first checkpoint/recovery event. If a pointer is later reassigned (e.g. through buffer swapping), subsequent checkpoints/recoveries will still operate on the originally registered memory region rather than the pointer’s current value after reassignation. 

In applications that perform pointer swaps inside a loop, this can lead to checkpointing irrelevant data. To mitigate this, consider triggering checkpoints only on selected iterations (e.g. every other iteration).


## Example
Check [veloc.yml](../../example/veloc.yml) and [veloc_recovery.yml](../../example/veloc_recovery.yml) in the pdi example folder for an example of how to configure the *managed_checkpointing* behaviour of the plugin. <br>
Check [veloc_test_custom.cxx](./tests/veloc_test_custom.cxx) for an example of how to configure the *custom_checkpointing* behaviour of the plugin. 

## Running a program
In order to run a program with the VeloC plugin, follow the following steps: <br>
  * install VeloC (see [above section](#external-dependencies))
  * install an MPI implementation (see [above section](#external-dependencies))
  * install PDI and its dependencies ([see documentation]( ../../pdi/docs/Source_installation.md))
  * source pdi environment 
  * append path/to/veloc/install/lib/ to LD_LIBRARY_PATH
  * compile your program and run 
