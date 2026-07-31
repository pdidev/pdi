#!/usr/bin/env python3
# ******************************************************************************
# Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# ****************************************************************************/

import pdi
import numpy as np

pdi.init("logging: trace")

x = np.zeros([2, 3, 5])
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

pdi.release("sh")  # y

del y

pdi.reclaim("sh")  # x

print("x:")
print(x, "\n")

assert x[0][1][3] == 999

