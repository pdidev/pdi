import repex


import time
import random
import os
import sys
#from matplotlib import pyplot as plt
import numpy as np

from melissa_da_study import *

random.seed(43)
def free_resources():
    MAX_MINUTES = 20
    MAX_RES = 16
    ts = [0, MAX_MINUTES]
    ys = [2, 0]

    for _ in range(12):
        t = random.randint(0, MAX_MINUTES*2) / 2.
        while t in ts:
            t = random.randint(0, MAX_MINUTES*2) / 2.
        y = random.randint(3, MAX_RES)
        ts.append(t)
        ys.append(y)
    data = sorted(zip(ts,ys))
    ts = np.array(data).T[0]
    ys = np.array(data).T[1]
    #plt.step(ts, ys)
    #plt.show()
    return ts, ys

ts, ys = free_resources()

print(ts, ys)

start_time = time.time()

NODES_SERVER = 2
NODES_RUNNER = 1

def runners_now():
    global ts, ys
    mins = (time.time() - start_time) / 60
    i = 0
    while ts[i] < mins:
        i += 1
    return (ys[i] - NODES_SERVER) // NODES_RUNNER



def run():
    clean_old_stats()
    run_melissa_da_study(
            runner_cmd='simulation1',
            total_steps=100000,  # 10e3 are about 5 minuts be longer for sure ;)
            ensemble_size=30,
            assimilator_type=ASSIMILATOR_DUMMY,
            # not necessary to add cluster. By default an automatic selection for the cluster
            # is done. See the cluster_selector() method.
            cluster=SlurmCluster('igf@cpu'),
            procs_server=2,
            nodes_server=NODES_SERVER,
            procs_runner=40,
            nodes_runner=NODES_RUNNER,
            server_timeout=120,
            n_runners=runners_now,
            show_server_log=False,
            show_simulation_log=False,
            additional_server_env={  # necessary for jean-zay
                'LD_LIBRARY_PATH': os.getenv('LD_LIBRARY_PATH') + ':/gpfsscratch/rech/moy/rkop006/conda_envs/lib'
                },
            walltime='00:45:00')

if len(sys.argv) == 1:
    HOME = os.getenv("HOME")
    en = 'test-elasticity'
    repex.run(
            EXPERIMENT_NAME=en,
            INPUT_FILES=[HOME+'/workspace/melissa-da/build/CMakeCache.txt'],
            GIT_REPOS=[HOME+'/workspace/melissa-da'],
            experiment_function=run)

else:
    # make a nice plot:
    dt = 10  # seconds
    start_t = None
    updates_per_dt = {}
    with open(sys.argv[1], 'r') as f:
        for line in f.readlines():
            # if 'Finished up' in line:
            if 'Finished sta' in line:
                second = int(line.split(" ")[4])
                if not start_t:
                    start_t = second

                i = (second - start_t)//dt

                if not i in updates_per_dt:
                    updates_per_dt[i] = 0
                updates_per_dt[i] += 1

    # for now assume they start in the same...
    import numpy as np
    from matplotlib import pyplot as plt
    a = np.array(list(updates_per_dt.items()))
    xs = a.T[0] * dt
    updates = a.T[1]
    updates = updates / np.max(updates) * np.max(ys)  # normalize!
    # transform xs into minutes:
    xs -= xs.min()
    xs = xs / 60
    print(xs.shape)
    print(updates.shape)
    fig, (ax1, ax2) = plt.subplots(1, 2)
    ax1.plot(xs, updates, label='state updates per dt (normalized by max)')
    ax1.plot(ts, ys, label='active runners')
    ax1.set_xlabel('t in minutes')
    ax1.legend()

    #ax2.plot(ys, updates)
    ax2.set_xlabel('state updates per dt')
    ax2.set_ylabel('active runners')
    plt.show()
    print(updates_per_dt)

