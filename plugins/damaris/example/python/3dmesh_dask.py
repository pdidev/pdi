# Python code: 3dmesh_dask.py.template
# Author: Josh Bowden, Inria
# Description: Template file for running tests on Grid5000
# Part of the Damaris examples of using Python integration with Dask distributed
# To run this example, a Dask scheduler needs to be spun up:
# 
#   dask-scheduler --scheduler-file "/home/user/dask_file.json" &
#
# The --scheduler-file argument must match what is int the Damaris XML file 
# <pyscript> tag
# 
# Then run the simulation: (assumes 4 Damaris clients and 2 Damaris server cores as per the xml file)
#
#   mpirun --oversubscribe --host ubu20-hvvm-c -np 6 ./3dmesh_py 3dmesh_dask.xml -i 3 -v 2 -r
#
# This will create the Dask workers (one per Damaris server core) and have them connect to 
# the Dask scheduler. The simulation code will remove the workers at the end of the execution, 
# unless  keep-workers="yes" is specified in 3dmesh_dask.xml <pyscript> tag
# 

import numpy as np
np.set_printoptions(threshold=np.inf)


# N.B. This file is read by each Damaris server process on each iteration that is 
#      specified by the frequency="" in the <pyscript> XML sttribute.
# 
# DD (AKA Damaris Data) is a dictionary that has been filled by the 
# Damaris server process with NumPy arrays that point to the Damaris data variables 
# that are exposed in the simulation. The Damaris class that implements 
# this is PyAction, found in the src/scripts/ and include/damaris/scripts 
# directories. Damaris <variables> must be exposed to the Python <pyscript>
# XML element by including its name i.e. "MyPyAction"" in the following example:
# 
#    <variable name="cube_i" type="scalar" layout="cells_whd_wf" mesh="mesh" 
#                        centering="nodal" script="MyPyAction" />
#
#    <scripts>
#       <pyscript name="MyPyAction" file="3dmesh_py.py" language="python" frequency="1"
#            scheduler-file="/home/user/dask_file.json" nthreads="1" keep-workers="no" 
#            timeout="4" />
#    </scripts>
#    N.B. Setting nthreads="openmp" will set the dask worker threads
#         to the value obtained from opm_get_num_threads()
# 
#
# The Damaris server processes also present three dictionaries, damaris_env, dask_env and iteration_data 
# containing various data about the simulation as well as the variable data itself, packaged as Numpy arrays.
# DD['damaris_env'].keys()     - The global Damaris environment data
#       (['is_dedicated_node',    # 
#         'is_dedicated_core',    # 
#         'servers_per_node',     # Number of Damaris server ranks per node
#         'clients_per_node',     # Number of Damaris client ranks per node
#         'ranks_per_node',       # Total number of ranks per node
#         'cores_per_node',       # Total number of ranks per node (yes, the same as above)
#         'number_of_nodes',      # The total number of nodes used in the simulation
#         'simulation_name',      # The name of the simulation (specified in Damaris XML file)
#         'simulation_magic_number' # Unique number for a simulation run (used in constructing name of Dask workers.)
#       ])
#
# DD['dask_env'].keys()  - The Dask environment data, 
#       (['dask_scheduler_file',    # if an empty string then no Dask scheduler was found
#         'dask_workers_name',      # Each simulation has a uniquely named set of workers
#         'dask_nworkers',          # The total number of dask workers (== 'servers_per_node' x 'number_of_nodes')
#         'dask_threads_per_worker' # Dask workers can have their own threads. Specify as nthreads="1" in Damris XML file
#       ]) 
#
# DD['iteration_data'].keys() - A single simulation iteration. 
#                        Contains the iteration number and a list of sub-dictionaries, 
#                        one for each *Damaris variable* that has been exposed to the Python 
#                        interface. i.e. specified with the script="MyAction" as in the example above
#       (['iteration',              # The iteration number as an integer.
#          'cube_i',                # A Damaris variable dictionary - the name relates to the variable name used in the Damaris XML file
#          '...',                   # A Damaris variable dictionary
#          '...'                    # A Damaris variable dictionary
#       ])
#
# A Damaris variable dictionary has the following structure
# DD['iteration_data']['cube_i'].keys()
#       (['numpy_data', 
#         'sort_data',             
#         'type_string'           # possibly to be removed as this information can be obtained from the NumPy array itself
#        ])
# 
# DD['iteration_data']['cube_i']['sort_data']
# sort_data is a list, that can be sorted on (possibly required to be transformed to tuple) which
# when sorted, the list values can be used to reconstruct the whole array using Dask:
#    ['string', 'string', [ <block_offset values> ]]
#   A specific example:
#     ['S0_I1_<simulation_magic_number>', 'P0_B0', [ 0, 9, 12 ]]
#   The string 'S0_I1_<simulation_magic_number>' indicates 'S' for server and 'I' for iteration. 
#                                                The magic number is needed as the data is published to a Dask server
#   The string 'P0_B0' indciates the dictionary key of Numpy data (see next description for explanation of 'P' and 'B')
#   The list [ 0, 9, 12 ] indicates the offestes into the global array from where the NumPy data is mapped 
#                         (The size of the NumPy array inicates the block size of the data)
#                         
#
# And, finally, the NumPy data is present in blocks, given by keys constructed as described below
# DD['iteration_data']['cube_i']['numpy_data'].keys()
#        (['P0_B0',  
#          'P1_B0'
#        ])

#  Damaris NumPy data keys: 'P' + damaris client number + '_B' + domain number
#  The client number is the source of the data (i.e. it is the Damaris client number that wrote the data)
#  The domain number is the result of multiple calls to damaris_write_block()
#  or 0 if only damaris_write() API is used or a single block only was written.
# 
# N.B. Only the data for the current iteration is available - and it is Read Only. 
#      If it is needed later it needs to be saved somehow and re-read on the 
#      next iteration. When connected to a Dask scheduler then the data can be saved on 
#      the distributed workers.



def main(DD):
    import numpy as np
    import time
    from os import path
    from dask.distributed import Client, TimeoutError, Lock, Variable
    from damaris4py.server import getservercomm
    from damaris4py.server import listknownclients
    from damaris4py.dask import damaris_dask
    
   

    # Using to sum up elements in each block
    # see: https://stackoverflow.com/questions/40092808/compute-sum-of-the-elements-in-a-chunk-of-a-dask-array
    def compute_block_sum(block):
        return np.array([np.sum(block)])[:, None, None]

    try:
        # pass
        damaris_comm = getservercomm()
        rank = damaris_comm.Get_rank()
        size = damaris_comm.Get_size() 
          
        # This is the iteration value the from Damaris perspective
        # i.e. It is the number of times damaris_end_iteration() has been called
        iter_dict        = DD['iteration_data']
        iteration        = iter_dict['iteration']
        iteration_dam    = damaris_dask.return_iteration(DD)         
        assert iteration == iteration_dam
        
        damaris_comm.Barrier()

        # The value passed through by Damaris clients as array_sum is to be compared with the 
        # value computed by Dask. It is an array of a single value, which changes each iteration 
        # and is the sum of all values in the whole cube_i dataset in distributed memory.
        # The sum is only available on client rank 0
        # Uses defaults for:  client_rank=0, block_number=0 
        sum_from_sim = damaris_dask.return_scalar_from_position(DD, 'array_sum', pos=(0))
        print('Info from Python: Iteration ', iteration, ' Data found: iter_dict[array_sum][numpy_data][P0_B0] = ', sum_from_sim )

        
        # lists the known clients of the current Damaris server rank
        client_list = listknownclients()
        last_iter_numpy = damaris_dask.return_numpy_array(DD, 'last_iter', client_rank=client_list[0] )
        if (iteration == 0):
            if (last_iter_numpy is not None):
                print('Python iteration ', iteration, ' Data found: last_iter = ', last_iter_numpy[0] )
            else:
                print('Python iteration ', iteration, ' Data not found: last_iter ')

        # Do some Dask stuff
        # If this scheduler_file exists (is not an empty string) then a Dask scheduler was 
        # found (Access has been tested on the Damaris server C++ Python).
        # scheduler_file  = dask_dict['dask_scheduler_file'] 
        scheduler_file  = damaris_dask.return_scheduler_filename(DD)        
        if iteration == 0:
            print('Python INFO: Scheduler file '+ scheduler_file) 
        if (scheduler_file != ""):            
            
            try:      
                client =  Client(scheduler_file=scheduler_file, timeout='2s')
                # We now create the Dask dask.array
                x =  damaris_dask.return_dask_array(DD, client, 'cube_i', damaris_comm, print_array_component=False )
                # Only rank 0 returns a dask.array, the others return None
                if (x is not None):
                    array_sum_dask = x.sum().compute()
                    print('Python iteration ', iteration,' dask x.sum() := ', array_sum_dask)
                    
                    # if iteration == 2 :
                    #    y = x.map_blocks(compute_block_sum, chunks=(1, 1, 1)).compute()
                    #    print('Python iteration ', iteration, 'The sum of values within the dask.array blocks is:')
                    #    print(y)
                    
                    if sum_from_sim == array_sum_dask:
                        print('PASS')  # I should push this to the Dask memory to compare at end of iterations?
                    else:
                        print('FAIL')
                    print('')
                    # Use .persist() to not bring datasets back to the curent client
                    # Use .compute() with small datasets (summaries / reductions) to use in displaying results

                damaris_comm.Barrier()
  
                # close the client only:
                client.close()
                
            except TimeoutError as err:
                print('Python ERROR: TimeoutError!: ', err) 
            except OSError as err:
                print('Python ERROR: OSError!: ', err)
        else:
             print('Python INFO: Scheduler file not found:', scheduler_file)
    except KeyError as err: 
        print('Python ERROR: KeyError: No damaris data of name: ', err)
    except PermissionError as err:
        print('Python ERROR: PermissionError!: ', err)
    except ValueError as err:
        print('Python ERROR: Damaris Data problem!: ', err)
    except UnboundLocalError as err:
        print('Python ERROR: Damaris data not assigned!: ', err)
    except NameError as err:
        print('Python ERROR: NameError: ', err)
    # finally: is always called.    
    finally:
        pass



if __name__ == '__main__':
    main(DamarisData)
