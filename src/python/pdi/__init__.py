#*******************************************************************************
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

from ._pdi import access, Error, event, finalize, init, reclaim, release, version, OUT, IN, INOUT, NONE
import pdi._pdi
import numpy as np
import inspect

def share(name, data, access):
    if (isinstance(data, np.ndarray)):
        data_np_array = data
    elif (access == OUT or access == NONE):
        # data is not numpy array
        try:
            data_np_array = np.array(data)
        except:
            raise Error("`" + name + "' share: Type is not supported by PDI, cannot insert it into numpy array")
    else:
        raise Error("`" + name + "' share: IN and INOUT can be only done with numpy array data type")
    pdi._pdi.share(name, data_np_array, access)

def expose(name, data, access):
    share(name, data, access)
    reclaim(name)

def multi_expose(event_name, expose_list):
    exposed = []
    try:
        for (name, data, access) in expose_list:
            share(name, data, access)
            exposed.append(name)
        event(event_name)
    except:
        pass
    final_error = ()
    for name in exposed:
        try:
            reclaim(name)
        except Exception as e:
            final_error += (e)
    if (final_error != ()):
        raise final_error
