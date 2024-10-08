#!/usr/bin/env python3
#*******************************************************************************
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
#*****************************************************************************/

import pdi
import numpy as np

pdi.init("""logging: trace""")

# share
string = "string example"
string_np_array = np.copy(np.frombuffer(bytes(string.encode("ascii", "replace")), dtype='uint8')) # needs a copy to be mutable
pdi.share("string", string_np_array, pdi.OUT)

# access
returned_string_in_ascii = pdi.access("string", pdi.IN)

# check
string_as_ascii = np.frombuffer(bytes(string.encode("ascii", "replace")), dtype='uint8')
assert np.array_equal(string_as_ascii, returned_string_in_ascii)

pdi.release("string")

pdi.reclaim("string")

pdi.finalize()
