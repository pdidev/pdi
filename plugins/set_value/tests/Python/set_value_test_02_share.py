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

#scalars
value_char = pdi.access("value_char", pdi.IN)
print("value_char = " + str(value_char))
assert value_char == 42
pdi.release("value_char")

value_short = pdi.access("value_short", pdi.IN)
print("value_short = " + str(value_short))
assert value_short == 4242
pdi.release("value_short")

value_int = pdi.access("value_int", pdi.IN)
print("value_int = " + str(value_int))
assert value_int == 424242
pdi.release("value_int")

value_long = pdi.access("value_long", pdi.IN)
print("value_long = " + str(value_long))
assert value_long == 424242424242
pdi.release("value_long")

value_float = pdi.access("value_float", pdi.IN)
print("value_float = " + str(value_float))
assert abs(value_float - 3.141592) < 0.00001
pdi.release("value_float")

value_double = pdi.access("value_double", pdi.IN)
print("value_double = " + str(value_double))
assert abs(value_double - 3.14159265) < 0.00000001
pdi.release("value_double")

# arrays
array_char = pdi.access("char_array", pdi.IN)
print("char_array = " + str(array_char))
assert array_char[0] == 0
assert array_char[1] == 1
assert array_char[2] == 2
pdi.release("char_array")

array_short = pdi.access("short_array", pdi.IN)
print("short_array = " + str(array_short))
assert array_short[0] == 3
assert array_short[1] == 4
assert array_short[2] == 5
pdi.release("short_array")

array_int = pdi.access("int_array", pdi.IN)
print("int_array = " + str(array_int))
assert array_int[0] == 6
assert array_int[1] == 7
assert array_int[2] == 8
pdi.release("int_array")

array_long = pdi.access("long_array", pdi.IN)
print("long_array = " + str(array_long))
assert array_long[0] == 9
assert array_long[1] == 10
assert array_long[2] == 11
pdi.release("long_array")

array_float = pdi.access("float_array", pdi.IN)
print("float_array = " + str(array_float))
assert abs(array_float[0] - 1.234567) < 0.00001
assert abs(array_float[1] - 12.34567) < 0.00001
assert abs(array_float[2] - 123.4567) < 0.00001
pdi.release("float_array")

array_double = pdi.access("double_array", pdi.IN)
print("double_array = " + str(array_double))
assert abs(array_double[0] - 1.23456789) < 0.00000001
assert abs(array_double[1] - 12.3456789) < 0.00000001
assert abs(array_double[2] - 123.456789) < 0.00000001
pdi.release("double_array")
