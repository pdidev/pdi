# Python script run by the Damaris server cores for test_damaris_pyscript_dask.yml.
# Builds a dask.array from the `values` blocks held by every Damaris server core and, on server
# rank 0, writes `pyscript_dask_It<iteration>.txt` with the number of server cores and the sum.

def main(DD):
    from dask.distributed import Client
    from damaris4py.server import getservercomm
    from damaris4py.dask import damaris_dask

    damaris_comm = getservercomm()
    client = None
    try:
        iteration = DD['iteration_data']['iteration']
        client = Client(scheduler_file=damaris_dask.return_scheduler_filename(DD), timeout='10s')
        # Only server rank 0 gets the dask.array, the others return None
        x = damaris_dask.return_dask_array(DD, client, 'values', damaris_comm)
        if x is not None:
            total = int(x.sum().compute())
            with open('pyscript_dask_It' + str(iteration) + '.txt', 'w') as result:
                result.write(str(damaris_comm.Get_size()) + ' ' + str(total) + '\n')
            # Release the blocks published on the workers, so they can be retired at the end
            for dataset_name in client.list_datasets():
                if dataset_name.startswith('values_'):
                    client.unpublish_dataset(dataset_name)
    finally:
        damaris_comm.Barrier()
        if client is not None:
            client.close()


if __name__ == '__main__':
    main(DamarisData)
