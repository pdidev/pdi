#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

from sys import stdout, argv, exit
import pdi, yaml
import numpy as np

config_path = argv[1]

with open(config_path, 'r') as config_file:
	try:    
		config = yaml.safe_load(config_file)
	except yaml.YAMLError as exc:
		exit(exc)

pdi.init(yaml.dump(config))

# read values
outer_struct = pdi.access("outer_struct", pdi.IN)
print("outer_struct.scalar = " + str(outer_struct.scalar))
assert outer_struct.scalar == 4
print("outer_struct.array = " + str(outer_struct.array))
assert outer_struct.array[0] == 4
assert outer_struct.array[1] == 5
assert outer_struct.array[2] == 6
assert outer_struct.array[3] == 7

print("outer_struct.inner_struct.scalar = " + str(outer_struct.inner_struct.scalar))
assert outer_struct.inner_struct.scalar == 8
print("outer_struct.inner_struct.array = " + str(outer_struct.inner_struct.array))
assert outer_struct.inner_struct.array[0] == 0
assert outer_struct.inner_struct.array[1] == 1
assert outer_struct.inner_struct.array[2] == 2
assert outer_struct.inner_struct.array[3] == 3
pdi.release("outer_struct")

# write values
outer_struct = pdi.access("outer_struct", pdi.OUT)
outer_struct.scalar = 5
outer_struct.array = np.array([5, 6, 7, 8])

outer_struct.inner_struct.scalar = 9
try :
    outer_struct.inner_struct  = np.array([10, 11, 12, 13])
    raise pdi.Error("Can change only int64 array datatype")
except pdi.Error:
    pass
pdi.release("outer_struct")

# read new values
outer_struct = pdi.access("outer_struct", pdi.IN)
print("outer_struct.scalar = " + str(outer_struct.scalar))
assert outer_struct.scalar == 5
print("outer_struct.array = " + str(outer_struct.array))
assert outer_struct.array[0] == 5
assert outer_struct.array[1] == 6
assert outer_struct.array[2] == 7
assert outer_struct.array[3] == 8

print("outer_struct.inner_struct.scalar = " + str(outer_struct.inner_struct.scalar))
assert outer_struct.inner_struct.scalar == 9
print("outer_struct.inner_struct.array = " + str(outer_struct.inner_struct.array))
assert outer_struct.inner_struct.array[0] == 0
assert outer_struct.inner_struct.array[1] == 1
assert outer_struct.inner_struct.array[2] == 2
assert outer_struct.inner_struct.array[3] == 3
pdi.release("outer_struct")
