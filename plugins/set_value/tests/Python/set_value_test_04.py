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
