#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Institut national de recherche en informatique et en automatique (Inria)
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

# Python code: 3dmesh_dask_client.py
#
# Same computation as 3dmesh_dask.py (sum of cube_i with a dask.array built over all the Damaris
# server cores, compared with the sum computed in C), plus two things meant for large data and
# long simulations:
# - The Dask client is opened once, on the first iteration, and reused on the next ones instead
#   of being opened and closed on every iteration.
# - A result is kept on the Dask workers from one iteration to the next: here the running sum of
#   cube_i over the iterations, from which the time-average of every cell is computed. The data
#   stays distributed on the workers (persist()), nothing is gathered on the Damaris servers.
#
# This works because Damaris runs this file on every iteration with the same Python globals and
# locals: a variable set at the top level of the script (here `_dask_state') keeps its value from
# one iteration to the next, on each Damaris server core.
#
# Run (see README.md):
#   dask-scheduler --scheduler-file dask_file.json &
#   mpirun --oversubscribe -np 6 ./3dmesh_py 3dmesh_dask_client.yml -i 3 -v 2 -r
#
# N.B. The client is closed on the last iteration (`last_iter' sent by the simulation), so this
#      script assumes `frequency: 1`.


def main(DD, state):
    import time
    from dask.distributed import Client, TimeoutError, wait
    from damaris4py.server import getservercomm, listknownclients
    from damaris4py.dask import damaris_dask

    t_start = time.time()
    damaris_comm = getservercomm()
    rank = damaris_comm.Get_rank()

    iteration = damaris_dask.return_iteration(DD)
    last_iter = damaris_dask.return_numpy_array(DD, 'last_iter', client_rank=listknownclients()[0])[0]

    scheduler_file = damaris_dask.return_scheduler_filename(DD)
    if scheduler_file == "":
        print('Python INFO: Scheduler file not found')
        return

    try:
        # Opened on the first iteration only, then reused
        if state['client'] is None:
            state['client'] = Client(scheduler_file=scheduler_file, timeout='2s')
            print('Python server', rank, 'iteration', iteration, ': Dask client opened')
        client = state['client']

        # The sum computed by the simulation is only on the server that holds client 0's data
        sum_from_sim = damaris_dask.return_scalar_from_position(DD, 'array_sum', pos=(0))

        # Collective over the Damaris servers: only rank 0 gets the dask.array, the others None
        x = damaris_dask.return_dask_array(DD, client, 'cube_i', damaris_comm, print_array_component=False)
        if x is not None:
            array_sum_dask = x.sum().compute()
            print('Python iteration ', iteration, ' dask x.sum() := ', array_sum_dask)
            # sum_from_sim is on rank 0 too when client 0 writes to server 0 (default placement)
            if sum_from_sim is not None:
                print('PASS' if sum_from_sim == array_sum_dask else 'FAIL')

            # Running sum of cube_i over the iterations, kept on the Dask workers. persist() starts
            # the computation on the workers and keeps the result there; wait() makes sure it is
            # done before the blocks of this iteration are unpublished below.
            x64 = x.astype('float64')
            running_sum = x64 if state['running_sum'] is None else state['running_sum'] + x64
            running_sum = running_sum.persist()
            wait(running_sum)
            state['running_sum'] = running_sum
            state['nb_iter'] += 1
            # Time-average of every cell, still a distributed dask.array (e.g. to save or plot)
            time_average = running_sum / state['nb_iter']

            # Check: the sum of the time-average is the mean of the sums computed in C
            if sum_from_sim is not None:
                state['sim_sums'].append(sum_from_sim)
                expected = sum(state['sim_sums']) / len(state['sim_sums'])
                average_sum = time_average.sum().compute()
                print('Python iteration ', iteration, ' time-average over', state['nb_iter'], 'iterations, sum :=',
                      average_sum, 'PASS' if abs(average_sum - expected) < 1e-6 * abs(expected) else 'FAIL')
            # See 3dmesh_dask.py: published blocks must be removed, or the Dask workers cannot be
            # retired at the end of the simulation
            for dataset_name in client.list_datasets():
                if dataset_name.startswith('cube_i_'):
                    client.unpublish_dataset(dataset_name)
            print('Python iteration ', iteration, ' script time: {:.3f} s'.format(time.time() - t_start))

        # Last iteration: close the client before Damaris retires the Dask workers
        if iteration == last_iter - 1:
            damaris_comm.Barrier()
            state['running_sum'] = None   # releases the data kept on the workers
            client.close()
            state['client'] = None
            print('Python server', rank, 'iteration', iteration, ': Dask client closed')

    except TimeoutError as err:
        print('Python ERROR: TimeoutError!: ', err)
    except OSError as err:
        print('Python ERROR: OSError!: ', err)


# Kept from one iteration to the next (Damaris reuses the same Python globals and locals)
try:
    _dask_state
except NameError:
    _dask_state = {'client': None, 'running_sum': None, 'nb_iter': 0, 'sim_sums': []}

if __name__ == '__main__':
    main(DamarisData, _dask_state)
