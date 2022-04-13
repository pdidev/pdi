import os, sys


import mpi4py
mpi4py.rc(initialize=False, finalize=False)
from mpi4py import MPI


import numpy as np

def callback(cycle, ensemble_list_background, ensemble_list_analysis,
        ensemble_list_hidden_inout, assimilated_index, assimilated_varid):

    rank = MPI.COMM_WORLD.rank
    print('my rank:', rank)

    print("in the callback function")
    print("now doing DA update for cycle=%d..." % cycle)
    print("lens:", len(ensemble_list_background), len(ensemble_list_analysis))

    ii = ensemble_list_background[0]
    oo = ensemble_list_analysis[0]
    # load observation corresponding to time of cycle
    # somehow compare them with ensemble_list_background to generate ensemble_list_analysis
    print(np.array(ii).shape)
    print('input:', ii)
    oo = ii + 1
    print('output:', oo)
    print('refcount:', sys.getrefcount(ii), sys.getrefcount(oo))


    print('hidden:', ensemble_list_hidden_inout)

    return 1


if __name__ == '__main__':
    from melissa_da_study import *
    clean_old_stats()
    run_melissa_da_study(
        runner_cmd='simulation1',
        total_cycles=3,  # Total amount of cycles
        ensemble_size=3,
        assimilator_type=ASSIMILATOR_PYTHON,
        # cluster is now auto selected
        procs_server=2,
        procs_runner=3,
        n_runners=1,
        show_server_log=False,
        show_simulation_log=False,
        additional_server_env={
            'PYTHONPATH': os.getcwd() + ':' + os.getenv('PYTHONPATH'),
            'MELISSA_DA_PYTHON_ASSIMILATOR_MODULE': 'script_assimilate_python'
            },
        #precommand_server='xterm_gdb',
        server_timeout=10000,
        runner_timeout=10000,
        walltime='00:05:00'
        )
