# Damaris plugin with Python and Dask support

These examples pass simulation data to Python through the Damaris `pyscript` support.
They require Damaris built with Python support (`-DENABLE_PYTHON=ON`), plus the `damaris4py`
and `dask[distributed]` Python packages for the Dask example.

Both use the same simulation code, `3dmesh_py.c`. A global 3D cube of size
`[depth, height, width]` (set by `cube:` at the top of the YAML file) is split along its depth
between the Damaris client processes. At each iteration, the code exposes `cube_i` (int),
`cube_f` (float), `last_iter` and `array_sum` (the sum of the whole cube, computed in C) to PDI.
The Damaris plugin writes them to Damaris and ends the Damaris iteration. The Damaris server
cores then run the Python script on the datasets that reference it with `script: MyPyAction`.

```
Usage: 3dmesh_py <config.yml> [-v X] [-r] [-s Y] [-i I]
-v  <X>    X = 0 default, do not print arrays
           X = 1 Verbose mode, prints arrays
           X = 2 Verbose mode, prints summation of arrays
-r         Array values set as rank of process
-s  <Y>    Y is integer time to sleep in seconds between iterations
-i  <I>    I is the number of iterations of simulation to run
```

The cube depth must be divisible by the number of client processes (the total number of MPI
processes minus the Damaris server cores).

## Example 1: Python only

YAML config: `3dmesh_py.yml`. Python script: `3dmesh_py.py`.

The datasets passed to Python reference the `pyscript` by its name with `script:`, and the
`pyscript` gives the Python file to run (no `scheduler_file`, as Dask is not used):
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
          core: 1   # one Damaris server core per node runs the Python script
          node: 0
      ...
      datasets:
        - dataset:
            name: cube_i
            layout: cells_whd_wf
            script: MyPyAction    # makes the dataset available to the pyscript below
        ...
      pyscript:
        name: MyPyAction
        file: 3dmesh_py.py        # can be absolute or relative path
        language: python
        frequency: 1              # run the script every N iterations
      write:
        cube_i:
          position: [ '$prank*$gsize[0]/$psize', 0, 0 ]
        ...
```

With 4 clients and 1 Damaris server core:
```
mpirun --oversubscribe -np 5 ./3dmesh_py 3dmesh_py.yml -i 3 -v 2 -r
```

Expected result: for the same iteration, the sum printed by C should match the sum computed by
the `3dmesh_py.py` script.

N.B. The Python script only has access to the NumPy data of the clients that are working with
its own Damaris server core. To see this in action, set `dedicated: { core: 2 }` in
`3dmesh_py.yml` and run with one more process:
```
mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_py.yml -i 3 -v 2 -r
```
There are now 2 outputs from the Python script on each iteration, one for each Damaris server,
and their sum is equal to the C total:
```
C      iteration 0 , Sum () = 98304
...
Python iteration  0 , Sum() =  16384
Python iteration  0 , Sum() =  81920
```

## Example 2: Python and Dask distributed

YAML config: `3dmesh_dask.yml`. Python script: `3dmesh_dask.py`.

This version creates a `dask.array` from the blocks held by every Damaris server core, so it
can sum the whole dataset over multiple server cores and even over distributed nodes.

A Dask scheduler must be started first (it is quite verbose, so a separate terminal helps):
```
dask-scheduler --scheduler-file dask_file.json &
```

The `--scheduler-file` argument must match `pyscript.scheduler_file` in `3dmesh_dask.yml`:
```yaml
      pyscript:
        name: MyPyAction
        file: 3dmesh_dask.py
        scheduler_file: dask_file.json
```

`3dmesh_dask.yml` uses 2 Damaris server cores (`dedicated: { core: 2 }`), so with 4 clients:
```
mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_dask.yml -i 3 -r
```

Damaris creates the Dask workers (one per Damaris server core) and connects them to the
scheduler. It removes them at the end of the execution, unless `keep_workers: yes` is set under
`pyscript`.

To stop the scheduler:
```
python3 -c "from dask.distributed import Client; Client(scheduler_file='dask_file.json', timeout='2s').shutdown()"
```
