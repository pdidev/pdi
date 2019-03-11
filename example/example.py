#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

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

def iter(cur, next):
    assert len(cur.shape) == len(next.shape) == 2
    assert cur.shape == next.shape

    next[0,:] = cur[0,:]
    next[:,0] = cur[:,0]
    next[-1,:] = cur[-1,:]
    next[:,-1] = cur[:,-1]
    for yy in range(1, next.shape[0]-1):
        for xx in range(1, next.shape[1]-1):
            next[yy,xx] = cur[yy,xx] * .5 \
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

    main_comm = MPI.COMM_WORLD
    pdi.init(yaml.dump(config['pdi']))

    psize_1d = main_comm.Get_size()
    pcoord_1d = main_comm.Get_rank()

    dsize = np.array(config['datasize'])
    psize = np.array([config['parallelism']['height'], config['parallelism']['width']])
    duration = np.array(config['duration'])

    assert dsize[0] % psize[0] == dsize[1] % psize[1] == 0
    assert psize[0] * psize[1] == psize_1d
    dsize[0] = dsize[0] // psize[0] + 2
    dsize[1] = dsize[1] // psize[1] + 2

    cart_comm = main_comm.Create_cart(dims=psize, reorder=True)
    pcoord = np.array(cart_comm.Get_coords(pcoord_1d))
    
    pdi.expose('dsize', dsize, pdi.OUT)
    pdi.expose('psize', psize, pdi.OUT)
    pdi.expose('pcoord', pcoord, pdi.OUT)

    cur = init(dsize, pcoord)
    next = np.zeros(cur.shape)

    pdi.event('main_loop')
    start = MPI.Wtime()
    ii, next_reduce = np.array(0), np.array(0)
    while(True):
        pdi.multi_expose('newiter', [
                         ('iter', ii, pdi.INOUT),
                         ('main_field', cur, pdi.INOUT)
                         ])

        iter(cur, next)
        exchange(cart_comm, next)
        next, cur = cur, next
        
        if (ii >= next_reduce):
            local_time, global_time = np.array(MPI.Wtime() - start), np.array(0.0)
            main_comm.Allreduce([local_time, MPI.DOUBLE], [global_time, MPI.DOUBLE], op=MPI.MAX)
            if (global_time >= duration):
                if (pcoord_1d == 0):
                    print("iter={:7d}; time={:7.3f}; STOP!!!".format(int(ii), float(global_time)))
                break
            rem_iter = int(.9 * (duration - global_time) * (ii+1) / (global_time + 0.1))
            rem_iter = 1 if rem_iter < 1 else rem_iter
            next_reduce = ii + rem_iter
            if (pcoord_1d == 0):
                print("iter={:7d}; time={:7.3f}; next_reduce={:7d}".format(int(ii), float(global_time), int(next_reduce)))
        ii+=1

    pdi.event('finalization')
    pdi.expose('iter', ii, pdi.OUT)
    pdi.expose('main_field', cur, pdi.OUT)
