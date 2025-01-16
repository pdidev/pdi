# Damaris with Python and Dask Support

## Running the Examples
There are 2 examples in the example/python directory that can be useful to understand what is happening.  
  
Please note: The examples are only installed if CMake was configured with -DDAMARIS_BUILD_EXAMPLES=ON.
             And, if they are installed in a read-only system directory, you will need to copy them
             to your local directory area. They do not write a lot of data.

### Example 1  
A very basic example that only uses Python functionality (no Dask).
From directory     : <INSTALL_DIR>/example/python
Executable         : 3dmesh_py 
YML config         : 3dmesh_py.yml
Python script      : 3dmesh_py.py

```
Usage: 3dmesh_py <3dmesh_py.yml> [-v] [-r] [-s X]
-v  <X>    X = 0 default, do not print arrays
           X = 1 Verbose mode, prints arrays
           X = 2 Verbose mode, prints summation of arrays
-r         Array values set as rank of process\n");
-s  <Y>    Y is integer time to sleep in seconds between iterations
-i  <I>    I is the number of iterations of simulation to run\

 mpirun --oversubscribe -np 5 ./3dmesh_py 3dmesh_py.yml -i 3 -v 2 -r
```
  
Expected result:  
For the same iteration, the sum output from C++ printed to screen should match the value computed by the 3dmesh_py.py script

N.B. This example will fail if run over multiple nodes, or with multiple Damaris server cores, as the Python script only has access to NumPy data of the clients that are working with the server core.

To see this in action, change <dedicated cores="1" nodes="0" /> to <dedicated cores="2" nodes="0" /> in file 3dmesh_py.yml and run:
```
 # Note the extra rank requested: -np 6, to account for the extra Damaris server core.
 mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_py.yml -i 3 -v 2 -r
```
  
You will notice 2 outputs from the Python script on each iteration, one output for each Damaris server, showing a sum, and still only a single output of the sum from C++. You will notice that the sum of the 2 Python outputs will be equal to the C++ total.
```
C++    iteration  0 , Sum () = 98304
...
Python iteration  0 , Sum() =  16384
Python iteration  0 , Sum() =  81920

```
This shows that distributed nature of the data, with each Damaris server looking after data from particular Damaris server ranks (2 clients per server in this case).

### Example 2
From directory     : <INSTALL_DIR>/example/python
Executable         : 3dmesh_py_domains 
YML config         : 3dmesh_dask.yml
Python script      : 3dmesh_dask.py
  
An example of using Python integration with Dask distributed. This version sets deals with the distributed data on the Python side by creating a dask.array, so it can sum of the data in the blocks over multiple Damaris server cores and even over distributed nodes.
    
To run this example, a Dask scheduler needs to be spun up:
```
   # N.B. best to run this in a separate xterm session as it is quite verbose
   dask-scheduler --scheduler-file "path/to/dask_file.json" &
```
  
The --scheduler-file argument must match what is specified in the Damaris YML file

```yaml
pdi:
  ...
  plugins:
    ...
    damaris: 
      ...
      pyscript: 
        name: MyPyAction
        file: path/to/3dmesh_dask.py # can be absolute or relative path
        scheduler_file: path/to/dask_file.json
        ...
```

To run the simulation:   
Assumes 4 Damaris clients and 2 Damaris server cores as per the xml file   
i.e. the YML input file (3dmesh_dask.yml) ] contains 

```yaml
pdi:
  ...
  plugins:
    ...
    damaris: 
      ...
      architecture:
        ...
        dedicated:
          core: 2
          node: 0
      ...
```

Usage: 3dmesh_py_domains  <3dmesh_dask.yml> [-i I] [-d D] [-r] [-s S];
-i  I    I is the number of iterations of simulation to run
-r         Array values set as rank of process
-d  D    D is the number of domains to split data into (must divide into WIDTH value in XML file perfectly)
-s  S    S is integer time to sleep in seconds between iterations

mpirun --oversubscribe -np 6 ./3dmesh_py_domains 3dmesh_dask.yml -i 10 -r -d 4
``` 
  
The simulation code (via Damaris pyscript class) will create the Dask workers (one per Damaris server core) and have them connect to the Dask scheduler. The simulation code will remove the workers at the end of the execution, unless we have ```keep_workers: yes``` specified in 3dmesh_dask.yml.
  
To stop the scheduler:
```
DASK_SCHEDULER_FILE=$HOME/dask_file.json  # must match what you are using above
DASK_SCHED_STR=\'$DASK_SCHEDULER_FILE\'
python3 -c "from dask.distributed import Client; client= Client(scheduler_file=$DASK_SCHED_STR, timeout='2s'); client.shutdown()"
```

