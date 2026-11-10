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
mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_dask.yml -i 3 -v 2 -r
```

Damaris creates the Dask workers (one per Damaris server core) and connects them to the
scheduler. It removes them at the end of the execution, unless `keep_workers: yes` is set under
`pyscript`.

To stop the scheduler:
```
python3 -c "from dask.distributed import Client; Client(scheduler_file='dask_file.json', timeout='2s').shutdown()"
```

## Example 3: Dask with a persistent client and state between iterations

YAML config: `3dmesh_dask_client.yml`. Python script: `3dmesh_dask_client.py`.

Same setup and same check as Example 2 (2 Damaris server cores, a Dask scheduler started first),
with two changes meant for large data and long simulations:
- The Dask client is opened once, on the first iteration, and reused until the last one, instead
  of being opened and closed on every iteration.
- A result is kept on the Dask workers from one iteration to the next: the running sum of
  `cube_i`, from which the time-average of every cell is computed. It stays distributed on the
  workers (`persist()`); nothing is gathered on the Damaris servers.

This works because Damaris runs the script on every iteration with the same Python globals: a
variable set at the top level of the script keeps its value from one iteration to the next.

```
dask-scheduler --scheduler-file dask_file.json &
mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_dask_client.yml -i 3 -v 2 -r
```

On each iteration, in addition to the output of Example 2, the script prints the sum of the
time-average and checks it against the mean of the sums computed in C:
```
Python iteration  1  time-average over 2 iterations, sum := 1573888.0 PASS
```
The client is closed on the last iteration, given by `last_iter`, so the script expects
`frequency: 1`.

## Which example to start from

| Example | Advantages | Costs |
|---|---|---|
| 1. Python only | Simplest and fastest: NumPy directly on the Damaris shared memory, nothing else to install or start. | Each server only sees the blocks of its own clients: no global view of the dataset. |
| 2. Dask, new client each iteration | Global view: one `dask.array` over all the servers and nodes. Iterations are independent, so a failure does not carry over. | Needs a Dask scheduler and a consistent Python environment. Workers start at the first iteration, plus a scheduler round trip on every iteration. |
| 3. Dask, persistent client | For large data and long runs: no reconnection on every iteration (it matters with a remote scheduler), and results kept distributed on the workers across iterations (time statistics, accumulations, ...). | The script manages the client and the kept data itself, closing them on the last iteration; it must not keep more than the workers' memory can hold. |

Measured on a laptop (scheduler on the same machine, 4 clients and 2 server cores, 16×8×16
cube), Python script time on server 0:

| | iteration 0 | iterations 1 to 4 |
|---|---|---|
| Example 2, new client each iteration | 1.30 to 1.60 s | 0.070 to 0.080 s |
| Example 3, client reuse only (no time-average) | 1.29 s | 0.065 to 0.076 s |
| Example 3, with the time-average | 1.55 s | 0.11 to 0.13 s |

Iteration 0 is mostly the start of the Dask workers. The gain from reusing the client is small
here because the scheduler is local and the data tiny; it grows with a remote scheduler and many
iterations. The time-average costs one more distributed computation per iteration, but its result
stays on the workers, which is the point for data too large to gather.
