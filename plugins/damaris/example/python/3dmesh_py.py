# Python code: 3dmesh_py.py
# Damaris example of obtaining data from a simulation via Python
#
# mpirun --oversubscribe --host ubu20-hvvm-c -np 5 ./3dmesh_py 3dmesh_py.xml -i 3 -v 2 -r
# 
# Example output:
# ScriptManager has found a script which has a file field named: 3dmesh_py.py
# Input paramaters found: v=2 r=1 (0 is not found)
# Input paramaters found: v=2 r=1 (0 is not found)
# Input paramaters found: v=2 r=1 (0 is not found)
# Input paramaters found: v=2 r=1 (0 is not found)
# Iteration 0 Rank 0 Sum =        0
# Iteration 0 Rank 1 Sum =    16384
# Iteration 0 Rank 2 Sum =    32768
# Iteration 0 Rank 3 Sum =    49152
# Iteration 0 done in 1.001990 seconds
# cube_i
# cube_f
# Info from Python: Iteration  0  Data found: cube_i[ P3_B0 ].sum() =  49152
# Info from Python: Iteration  0  Data found: cube_i[ P1_B0 ].sum() =  16384
# Info from Python: Iteration  0  Data found: cube_i[ P0_B0 ].sum() =  0
# Info from Python: Iteration  0  Data found: cube_i[ P2_B0 ].sum() =  32768
# for variable cube_i the number of domains (== clients x blocks_per_client) for variable last_iter:  4
# for variable cube_i block sources list:  [3, 1, 0, 2]

import numpy as np
np.set_printoptions(threshold=np.inf)


# DD (AKA Damaris Data) is a dictionary that has been filled by the 
# Damaris server process with NumPy arraysthat point to the data variables 
# that is exposed in the simulation. The Damaris source file that implements 
# thisis PyAction, found in the src/scripts/ and include/damaris/scripts 
# directories. Damaris <variables> must be exposed to the Python <pyscript>
# XML element by including its name i.e. MyPyAction in the following example:
# 
#    <variable name="cube_i" type="scalar" layout="cells_whd_wf" mesh="mesh" 
#                                                  centering="nodal" script="MyPyAction" />
#
#    <scripts>
#       <pyscript name="MyPyAction" file="3dmesh_py.py" language="python" frequency="1"
#            scheduler-file="/home/user/dask_file.json" nthreads="1" keep-workers="no" 
#            timeout="4" />
#    </scripts>
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
    try:
        # These two dictionaries are set up in the PyAction constructor 
        # and are static.
        damaris_dict = DD['damaris_env']
        dask_dict    = DD['dask_env']
        # This third dictionary is set up in PyAction::PassDataToPython() and is 
        # typically different each iteration.
        iter_dict    = DD['iteration_data']   
        # This is the iteration value the from Damaris perspective
        # i.e. It is the number of times damaris_end_iteration() has been called
        it       = iter_dict['iteration']

        if it == 0:
            print('The DamarisData dictionaries are:')
            keys = list(DD.keys())
            print(keys)
            
            # These are the variables that have been published to Python by the Damaris server process:
            print('The DamarisData variables available are :')
            keys = list(iter_dict.keys())
            for data_key in keys :
                if (data_key != 'iteration'):
                    print(data_key)
                   
        # We know the variable names as they match what is in the Damaris XML file
        cube_i =  iter_dict['cube_i']
        # There will be one key and corresponding NumPy array for each block of the variable
        total_sum = 0
        for key in cube_i['numpy_data'].keys() :
            cube_i_numpy = cube_i['numpy_data'][key]  # This is our NumPy array
            print('Python iteration ', it, ', Data found: cube_i[',key,'].sum() = ', cube_i_numpy.sum() )
            total_sum += cube_i_numpy.sum()
        
        print('Python iteration ', it, ', Sum() = ', total_sum )


        
    except KeyError as err: 
        print('KeyError: No damaris data of name: ', err)
    except PermissionError as err:
        print('PermissionError!: ', err)
    except ValueError as err:
        print('Damaris Data is read only!: ', err)
    except UnboundLocalError as err:
        print('Damaris data not assigned!: ', err)   
    finally:
        pass


if __name__ == '__main__':
    main(DamarisData)
