#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

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
