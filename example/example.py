from mpi4py import MPI
from sys import argv, exit
import numpy as np
import pdi, yaml

def init(dsize, pcoord):
    assert len(dsize) == 2 and len(pcoord) == 2
    dat = np.zeros(dsize)
    if (pcoord[1] == 0):
        dat[:,0] = 1000000
    return dat

def iter(cur, nxt):
    assert len(cur.shape) == len(nxt.shape) == 2
    assert cur.shape == nxt.shape

    nxt[0,:] = cur[0,:]
    nxt[:,0] = cur[:,0]
    nxt[-1,:] = cur[-1,:]
    nxt[:,-1] = cur[:,-1]
    for yy in range(1, nxt.shape[0]-1):
        for xx in range(1, nxt.shape[1]-1):
            nxt[yy,xx] = cur[yy,xx] * .5 \
                        + cur[yy,xx-1] * .125 \
                        + cur[yy,xx+1] * .125 \
                        + cur[yy-1,xx] * .125 \
                        + cur[yy+1,xx] * .125

def exchange(cart_comm, cur):
    assert len(cur.shape) == 2
    dsize = cur.shape

    if (not exchange.initialized):
        exchange.column = MPI.DOUBLE.Create_vector(dsize[0]-2, 1, dsize[1])
        exchange.column.Commit()
        exchange.row = MPI.DOUBLE.Create_contiguous(dsize[1]-2)
        exchange.row.Commit()
        exchange.initialized = True

    # send down 
    source, dest = cart_comm.Shift(0, 1)
    cart_comm.Sendrecv([cur[dsize[0]-2,1:-1], 1, exchange.row], dest, 100,
                       [cur[0,1:-1], 1, exchange.row], source, 100)

    # send up 
    source, dest = cart_comm.Shift(0, -1)
    cart_comm.Sendrecv([cur[1,1:-1], 1, exchange.row], dest, 100,
                       [cur[dsize[0]-1,1:-1], 1, exchange.row], source, 100)

    # send to the right 
    source, dest = cart_comm.Shift(1, 1)
    cart_comm.Sendrecv([cur[1:-1,dsize[1]-2], 1, exchange.column], dest, 100,
                       [cur[1:-1,0], 1, exchange.column], source, 100)

    # send to the left 
    source, dest = cart_comm.Shift(1, -1)
    cart_comm.Sendrecv([cur[1:-1,1], 1, exchange.column], dest, 100,
                       [cur[1:-1,dsize[1]-1], 1, exchange.column], source, 100)
exchange.initialized = False


if __name__ == '__main__':
    if (len(argv) != 2):
        exit('Usage: {} <config_file>'.format(argv[0]))
    config_path = argv[1]

    with open(config_path, 'r') as config_file:
        try:    
            config = yaml.load(config_file)
        except yaml.YAMLError as exc:
            exit(exc)

    comm = pdi.init(MPI.COMM_WORLD, yaml.dump(config['pdi']))

    comm_rank = comm.Get_rank()
    comm_size = comm.Get_size()

    dsize = np.array(config['datasize'])
    psize = np.array([config['parallelism']['height'], config['parallelism']['width']])
    duration = np.array(config['duration'])

    assert dsize[0] % psize[0] == dsize[1] % psize[1] == 0
    assert psize[0] * psize[1] == comm_size
    dsize[0] = dsize[0] // psize[0] + 2
    dsize[1] = dsize[1] // psize[1] + 2

    cart_comm = comm.Create_cart(dims=psize, reorder=True)
    pcoord = np.array(cart_comm.Get_coords(comm_rank))
    
    pdi.share('dsize', dsize, pdi.OUT)
    pdi.reclaim('dsize')
    pdi.share('psize', psize, pdi.OUT)
    pdi.reclaim('psize')
    pdi.share('pcoord', pcoord, pdi.OUT)
    pdi.reclaim('pcoord')

    cur = init(dsize, pcoord)
    nxt = np.zeros(cur.shape)
    pdi.event('main_loop')
    start = MPI.Wtime()

    ii, next_reduce = np.array(0), np.array(0)
    while(True):
        pdi.share('iter', ii, pdi.INOUT)
        pdi.share('main_field', cur, pdi.INOUT)
        pdi.event('newiter')
        pdi.reclaim('main_field')
        pdi.reclaim('iter')

        iter(cur, nxt)
        exchange(cart_comm, nxt)
        nxt, cur = cur, nxt
        if (ii >= next_reduce):
            local_time, global_time = np.array(MPI.Wtime() - start), np.array(0.0)
            comm.Allreduce([local_time, MPI.DOUBLE], [global_time, MPI.DOUBLE], op=MPI.MAX)
            if (global_time >= duration):
                if (comm_rank == 0):
                    print("iter={:7d}; time={:7.3f}; STOP!!!".format(int(ii), float(global_time)))
                break
            rem_iter = int(.9 * (duration - global_time) * (ii+1) / (global_time + 0.1))
            rem_iter = 1 if rem_iter < 1 else rem_iter
            next_reduce = ii + rem_iter
            if (comm_rank == 0):
                print("iter={:7d}; time={:7.3f}; next_reduce={:7d}".format(int(ii), float(global_time), int(next_reduce)))
        ii+=1

    pdi.event('finalization')
    pdi.share('iter', ii, pdi.OUT)
    pdi.reclaim('iter')
    pdi.share('main_field', cur, pdi.OUT)
    pdi.reclaim('main_field')

    MPI.Finalize()
