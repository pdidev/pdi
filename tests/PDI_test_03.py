#!/usr/bin/env python

import pdi
import numpy as np

pdi.init("")

x = np.zeros([2,3,5])
for i in range(2):
	for j in range(3):
		for k in range(5):
			x[i][j][k] = k + 10*j + 100*i

pdi.share("sh", x, pdi.INOUT)
y = pdi.access("sh", pdi.INOUT)

print("y:")
print(y, "\n")

print(" --- change y ---\n")
y[0][1][3] = 999

print("y:")
print(y, "\n")

pdi.release("sh") # y

del y

pdi.reclaim("sh") # x

print("x:")
print(x, "\n")

assert x[0][1][3] == 999
