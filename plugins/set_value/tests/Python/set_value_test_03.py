#!/usr/bin/env python3
#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
