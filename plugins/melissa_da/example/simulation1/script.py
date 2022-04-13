import os
 
print("PDI_DIR = " + os.environ["PDI_DIR"])

from melissa_da_study import *

run_melissa_da_study(
        runner_cmd='/home/kacper/Workspace/pdi/build/MELISSA_DA_PLUGIN/src/MELISSA_DA_PLUGIN_pkg-build/example/simulation1/sim',
        total_steps=10,
        ensemble_size=5,
        assimilator_type=ASSIMILATOR_DUMMY,
        # not necessary to add cluster. By default an automatic selection for the cluster
        # is done. See the cluster_selector() method.
        #cluster=LocalCluster(),
        procs_server=2,
        procs_runner=4,
        n_runners=1,
        show_server_log=True,
        show_simulation_log=True)