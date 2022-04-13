# Python3

from melissa_da_study import *
import shutil
import os
import sys
import pandas as pd
from io import StringIO

import time

import subprocess
import signal

import random

clean_old_stats()


executable='simulation1'
total_steps=3
ensemble_size=3
assimilator_type=ASSIMILATOR_DUMMY
cluster=LocalCluster()
procs_server=1
procs_runner=1
n_runners=1

def compare(reference_file, output_file="STATS/output.txt"):
    print('Compare %s with %s...' % (output_file, reference_file))
    ret = subprocess.call(['diff', '-s', '--side-by-side', output_file, reference_file])
    if ret != 0:
        print("failed! Wrong %s generated!" % output_file)
        exit(ret)


def get_run_information():
    return pd.read_csv('STATS/server.run-information.csv')


def get_timing_information():
    return pd.read_csv('STATS/server.timing-information.csv')


def run(server_slowdown_factor_=1):
    start = time.time()
    LocalCluster.clean_up_test()
    run_melissa_da_study(
            executable,
            total_steps,
            ensemble_size,
            assimilator_type,
            LocalCluster(),
            procs_server,
            procs_runner,
            n_runners,
            False,
            False,
            server_slowdown_factor=server_slowdown_factor_,
            runner_timeout=10,
            precommand_server='')
    LocalCluster.clean_up_test()
    diff = time.time() - start
    print("This took %.3f seconds" % diff)

def test_index_map(executable_):
    global executable, procs_server, procs_runner, n_runners, total_steps
    global assimilator_type

    ref_file = './reference-index-map.csv'
    if os.path.isfile(ref_file):
        os.remove(ref_file)

    executable = executable_

    total_steps = 1
    assimilator_type = ASSIMILATOR_PRINT_INDEX_MAP


    procs_server = 2
    procs_runner = 3
    n_runners = 2
    clean_old_stats()
    run()

    os.system('cat STATS/index-map-hidden.csv >> STATS/index-map.csv')
    shutil.copyfile('STATS/index-map.csv', ref_file)
    n_runners = 1
    procs_server = 3
    procs_runner = 2
    run()
    os.system('cat STATS/index-map-hidden.csv >> STATS/index-map.csv')


    compare("STATS/index-map.csv", './reference-index-map.csv')




testcase = sys.argv[1]
if testcase == 'test-crashing-launcher':
    assert False # unimplemented

elif testcase == 'test-check-stateless':
    assert check_stateless('simulation1')

    assert check_stateless('simulation1-stateful') == False

    assert check_stateless('simulation1-hidden')

elif testcase == 'test-index-map':
    test_index_map('simulation1-index-map')
elif testcase == 'test-hidden-index-map':
    executable = "simulation1-hidden-index-map"

    clean_old_stats()
    total_steps = 1
    assimilator_type = ASSIMILATOR_PRINT_INDEX_MAP


    procs_server = 2
    procs_runner = 3
    n_runners = 2
    run()
    os.system('cat STATS/index-map-hidden.csv >> STATS/index-map.csv')
    compare("STATS/index-map.csv", './reference-hidden-index-map.csv')
elif testcase == 'test-empty-index-map':
    test_index_map('simulation1')
    compare("STATS/index-map.csv", './reference-empty-index-map.csv')
elif testcase == 'test-empty-hidden-index-map':
    executable = "simulation1-hidden"

    total_steps = 1
    assimilator_type = ASSIMILATOR_PRINT_INDEX_MAP

    procs_server = 3
    procs_runner = 2
    n_runners = 1
    clean_old_stats()
    run()
    os.system('cat STATS/index-map-hidden.csv >> STATS/index-map.csv')
    compare("STATS/index-map.csv", './reference-empty-hidden-index-map.csv')
else:
    raise NotImplementedError("{:s} not implemented".format(testcase))

print("passed!")
exit(0)

