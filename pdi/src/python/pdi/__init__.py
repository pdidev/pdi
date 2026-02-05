# SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

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
