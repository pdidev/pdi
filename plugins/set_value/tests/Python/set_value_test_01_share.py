#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

from sys import argv, exit
import pdi, yaml
import numpy as np

config_path = argv[1]

with open(config_path, 'r') as config_file:
	try:    
		config = yaml.safe_load(config_file)
	except yaml.YAMLError as exc:
		exit(exc)

pdi.init(yaml.dump(config))

struct = pdi.access("record_data", pdi.IN)
print(struct.scalar_data)
assert struct.scalar_data == 4
print(struct.array_data)
assert struct.array_data[0] == 2
assert struct.array_data[1] == 3
assert struct.array_data[2] == 4
assert struct.array_data[3] == 5
pdi.release("record_data")
