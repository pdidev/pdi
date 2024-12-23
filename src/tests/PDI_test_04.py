#!/usr/bin/env python3

import pdi
import numpy as np

pdi.init("logging: trace")

a = 42
pdi.share("a", a, pdi.OUT)

a_returned = pdi.access("a", pdi.IN)
assert a_returned == a
pdi.release("a")

pdi.reclaim("a")


b = 42.1
pdi.share("b", b, pdi.OUT)

b_returned = pdi.access("b", pdi.IN)
assert b_returned == b
pdi.release("b")

pdi.reclaim("b")


c = False
pdi.share("c", c, pdi.OUT)

c_returned = pdi.access("c", pdi.IN)
assert c_returned == c
pdi.release("c")

pdi.reclaim("c")


d = [1, 2, 3]
pdi.share("d", d, pdi.OUT)

d_returned = pdi.access("d", pdi.IN)
assert d_returned[0] == d[0]
assert d_returned[1] == d[1]
assert d_returned[2] == d[2]
pdi.release("d")

pdi.reclaim("d")

e = [1.2, 2.3, 3.4]
pdi.share("e", e, pdi.OUT)

e_returned = pdi.access("e", pdi.IN)
assert e_returned[0] == e[0]
assert e_returned[1] == e[1]
assert e_returned[2] == e[2]
pdi.release("e")

pdi.reclaim("e")


f = np.array([1, 2, 3])
pdi.share("f", f, pdi.IN)

f_returned = pdi.access("f", pdi.OUT)
f_returned[0] = -1
f_returned[1] = -2
f_returned[2] = -3
assert f_returned[0] == f[0]
assert f_returned[1] == f[1]
assert f_returned[2] == f[2]
pdi.release("f")

pdi.reclaim("f")

class TestClass():
    def __init__(self, int_value, string_value):
        self.int_value = int_value
        self.string_value = string_value

g = TestClass(42, "some text")
try :
    pdi.share("g", g, pdi.OUT)
    raise Error("`" + name + "' share: Class object type should not be supported by PDI")
except pdi.Error:
    pass


pdi.finalize()
