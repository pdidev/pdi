# The VeloC plugin

The VeloC plugin enables  

* writing and reading generic-data checkpoint files using [VeloC memory-based API](https://veloc.readthedocs.io/en/latest/api.html#example)
* writing and reading applications' custom checkpoint files using [VeloC file-based API](https://veloc.readthedocs.io/en/latest/api.html#example#memory-based-api)
* handling the persistence, versioning, and transfer of checkpoint files using [VeloC](https://veloc.readthedocs.io/en/latest/)

Please note for applications's customs checkpoints, VeloC does not automate the serialization or deserialization of data structure like it does for generic-data checkpoints; that remains manual and user‑defined. 

The VeloC plugin does not currently support the full set of features of VeloC, but it offers a simple
declarative interface to access the core features.

## Requirements 
In order to use the VeloC plugin, the user needs to install VeloC 1.8 or above as it is not vendored in PDI. This can be done following the instructions [here](https://veloc.readthedocs.io/en/latest/quick.html#download-and-install). <br>

The user also needs an MPI implementation, such as 
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.0 or above but not higher than 4.3.2 due to [issue 7814](https://github.com/pmodels/mpich/issues/7814)

## Configuration grammar

### Mandatory Mappings

The VeloC plugin specification tree requires 4 mandatory mappings. 

* *failure* : an integer that indicates whether a failure has occurred , therefore, whether the application should be restored from a checkpoint 

```yml
failure: 0 #restore not required
```
```yml
failure: 1 #restore required
```
* *config_file* : path to [VeloC's own configuration file](https://veloc.readthedocs.io/en/latest/userguide.html#setup)

```yml
config_file: veloc_config.cfg 
```
* *checkpoint_label*: label used by VeloC to name checkpoint files. 

```yml
checkpoint_label: myapp
```
When using the VeloC plugin to checkpoint an application, the user may set the *checkpoint_label* equal to "myapp". If a failure occurs and the user wishes to restart the simulation from a checkpoint, the specification tree used in the new simulation run must have the *checkpoint_label* also equal to "myapp". 

* *iteration*: name of the variable corresponding to the simulation loop's iterator (specify the same name as defined in the PDI data/metadata specification tree). This is also used by VeloC to name checkpoint files. 

```yml
iteration: ii
```

### Optional Mappings 
* *status* : name of an integer variable that, when exposed to PDI, is set by the plugin to 0 if a recovery is needed (based on the value of the *failure* key), and to 1 if no recovery is needed (based on the value of the *failure* key or if a recovery already occurred). This is provided for applications that wish to inspect this state.

```yml
status: checkpoint_status
```
* *counter* : name of an integer variable that, when exposed to PDI, is set by the plugin to the number of checkpoints written up to that point. This is provided for applications that wish to inspect this value.

```yml
status: checkpoint_counter
```

### Checkpointing Behaviors

The user can define a *managed_checkpointing* tree or a *custom_checkpointing* tree. 

#### Managed Checkpointing
*managed_checkpointing* requires the following mappings: 

* *protected_data* : list of names of data structures to be checkpointed during a checkpoint event or to be recovered during a recover event (specify the same names as defined in the PDI data/metadata specification tree). This list must include the simulation's iteration counter.

* *checkpoint_on* : name of the PDI event where the user wants to checkpoint

* *recover_on* : name of the PDI event where the user wants to recover

* *synchronize_on* : name of the PDI event where the user wants to synchronize. If the *failure* key is set to 1, the first synchronization event is a recovery event, and subsequent synchronization events are checkpoint events. If the *failure* key is set to 0, all synchronization events are checkpoint events.

* *when* : PDI expression indicating when to execute a checkpoint operation. It is evaluated both in the case of a checkpoint event or a synchronization event. 

*managed_checkpointing* accepts the following optional mapping

  * *checkpoint_nr* : The iteration number of the checkpoint to restore. For example, if the user specifies 20, the plugin will restore the checkpoint written at iteration 20.

```yml
managed_checkpointing:
        protect_data: [iter, main_field,elapsed_offset] # data to be checkpointed/recovered, iter must be included
        synchronize_on: newiter # name of PDI_event to check status and consequently checkpoint/recover  
        when : '$iter % 1000 = 0'
        checkpoint_nr : 3000
```

#### Custom Checkpointing
*custom_checkpointing* requires the following mappings:

* *veloc_file* : name of a char array variable with a maximum size equal to 256 (specify the same name as defined in the PDI data/metadata specification tree). This variable is used by VeloC to map an input filename to the actual routed file path managed by VeloC.


* *custom_checkpoint* : sub-tree that requires the following mappings 

  * *original_file* : name of the checkpoint file 
  * *start_on* : name of the PDI event where the user wants to start a checkpoint phase 
  * *route_file_on* : name of the PDI event where the user wants to map the logical filename to the actual routed file path managed by VeloC. 
  * *end_on* : name of the PDI event where the user wants to end a checkpoint phase 

  All checkpointing logic must be placed by the user after the "route_file_on" event and before the "end_on" event. 

*  *custom_recover* : sub-tree that requires the following mappings 

   * *original_file* : name of the checkpoint file to be used for recovering 
   * *start_on* : name of the PDI event where the user wants to start a recovery phase 
   * *route_file_on* : name of the PDI event where the user wants to map the logical filename to the actual routed file path managed by VeloC. 
   * *end_on* : name of the PDI event where the user wants to end a recovery phase 

   *custom_recover* accepts the following optional mapping

   * *checkpoint_nr* : The iteration number of the checkpoint to restore. For example, if the user specifies 20, the plugin will restore the checkpoint written at iteration 20.


  All recovery logic must be placed by the user after the "route_file_on" event and before the "end_on" event. 

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
The Veloc plugin binds data structures to the memory address provided during the first checkpoint/recovery event. If a pointer is later reassigned (e.g. through buffer swapping), subsequent checkpoints will still operate on the originally registered memory region rather than the pointer’s current value after reassignation. 

In applications that perform pointer swaps inside a loop, this can lead to checkpointing irrelevant data. To mitigate this, consider triggering checkpoints only on selected iterations (e.g. every other iteration).


## Example
Check [veloc.yml](../../example/veloc.yml) and [veloc_recovery.yml](../../example/veloc_recovery.yml) in the pdi example folder for an example of how to use the *managed_checkpointing* behaviour of the plugin. <br>
Check [veloc_test_03_1.cxx](./tests/veloc_test_03_1.cxx) and [veloc_test_03_2.cxx](./tests/veloc_test_03_2.cxx) for an example of how to use the *custom_checkpointing* behaviour of the plugin. 