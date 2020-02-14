#!/usr/bin/env python

from __future__ import (print_function,
                        division)

import functools
from mpi4py import MPI
import sioninter

TESTLEN = 2 ** 10


def main():
    greet()
    test_data()
    keyval_test()


def print_start_stop(func):
    @functools.wraps(func)
    def wrapper(*args, **kwds):
        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        name = func.__name__
        print("on rank {}: {} start".format(rank, name))
        ret = func(*args, **kwds)
        print("on rank {}: {} finished".format(rank, name))
        return ret
    return wrapper


@print_start_stop
def greet():
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    print("on rank {}: greet".format(rank))
    text = b"hello hello"

    for end_idx in range(5):
        tmp = text[:len(text) - end_idx]
        read_write_test(tmp, "testfile.sion", rank)


@print_start_stop
def test_data():
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    bb = chr(ord("A") + (rank % 26)).encode() * TESTLEN

    read_write_test(bb, "testfile.sion", rank)


def read_write_test(text, filename, rank):
    sionfile = sioninter.open("testfile.sion", "wb", par_mode="mpi")
    sionfile.write(text)
    sionfile.close()

    sionfile = sioninter.open("testfile.sion", "rb", par_mode="mpi")
    tmp = sionfile.read(1)
    tmp += sionfile.read()
    sionfile.close()

    assert text == tmp, "on rank {}: {} != {}".format(rank, repr(text),
                                                      repr(tmp))


@print_start_stop
def keyval_test():
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    key = rank
    val = b"hello"

    sionfile = sioninter.open("testfile.sion", "wb,keyval=inline",
                              par_mode="mpi")
    sionfile.write_key(val, key)
    sionfile.close()

    sionfile = sioninter.open("testfile.sion", "rb,keyval=inline",
                              par_mode="mpi")
    tmp = sionfile.read_key(key, len(val))
    sionfile.close()

    assert tmp == val


if __name__ == "__main__":
    main()
