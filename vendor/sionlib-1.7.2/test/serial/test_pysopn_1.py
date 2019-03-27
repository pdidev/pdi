#!/usr/bin/env python

from __future__ import (print_function,
                        division)

import functools
import sioninter


def main():
    greet()
    keyval_test()


def print_start_stop(func):
    @functools.wraps(func)
    def wrapper(*args, **kwds):
        name = func.__name__
        print("{} start".format(name))
        ret = func(*args, **kwds)
        print("{} finished".format(name))
        return ret
    return wrapper


@print_start_stop
def greet():
    text = b"hello hello"
    for end_idx in range(5):
        tmp = text[:len(text) - end_idx]
        read_write_test(tmp, "testfile.sion")


def read_write_test(text, filename):
    sionfile = sioninter.open(filename, "wb")
    sionfile.write(text)
    sionfile.close()

    sionfile = sioninter.open(filename, "rb")
    tmp = sionfile.read(1)
    tmp += sionfile.read()
    sionfile.close()

    assert text == tmp, "{} != {}".format(repr(text), repr(tmp))


@print_start_stop
def keyval_test():
    key = 123
    val = b"hello"

    sionfile = sioninter.open("testfile.sion", "wb,keyval=inline")
    sionfile.write_key(val, key)
    sionfile.close()

    sionfile = sioninter.open("testfile.sion", "rb,keyval=inline")
    tmp = sionfile.read_key(key, len(val))
    sionfile.close()

    assert tmp == val


if __name__ == "__main__":
    main()
