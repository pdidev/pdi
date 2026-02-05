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
outer_array = pdi.access("outer_array", pdi.IN)
for i in range(4):
	print("outer_array[" + str(i) + "].scalar == " + str(outer_array[i].scalar))
	assert outer_array[i].scalar == i * 10
	for j in range(4):
		print("outer_array[" + str(i) + "].inner_array[" + str(j) + "] == " + str(outer_array[i].inner_array[j]))
		assert outer_array[i].inner_array[j] ==  i * 10 + j + 1
pdi.release("outer_array")

# write values
outer_array = pdi.access("outer_array", pdi.OUT)
for i in range(4):
	print("outer_array[" + str(i) + "].scalar = " + str(i * 100))
	outer_array[i].scalar = i * 100
	for j in range(4):
		print("outer_array[" + str(i) + "].inner_array[" + str(j) + "] = " + str(i * 100 + j + 10))
		outer_array[i].inner_array[j] =  i * 100 + j + 10
pdi.release("outer_array")

# read new values
outer_array = pdi.access("outer_array", pdi.IN)
for i in range(4):
	print("outer_array[" + str(i) + "].scalar == " + str(outer_array[i].scalar))
	assert outer_array[i].scalar == i * 100
	for j in range(4):
		print("outer_array[" + str(i) + "].inner_array[" + str(j) + "] == " + str(outer_array[i].inner_array[j]))
		assert outer_array[i].inner_array[j] ==  i * 100 + j + 10
pdi.release("outer_array")
