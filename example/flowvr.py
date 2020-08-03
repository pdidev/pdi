#=============================================================================
# Copyright (C) 2019-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2019-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#!/usr/bin/env python3
from flowvrapp import *
from filters import *
from pdi_flowvr import Module_PDI
import os
import sys

class FilterMerge2D(Filter):
   def __init__(self, name, host = ''):
     Filter.__init__(self, name, run = 'flowvr.plugins.Merge2D', host = host)
     self.addPort('in0', direction = 'in')
     self.addPort('in1', direction = 'in')
     self.addPort("out", direction = 'out')

#compute
hosts = 'localhost,localhost,localhost,localhost'
prefix = 'PDI_example_flowvr'
compute_yaml = str(sys.argv[1]) + "/flowvr.yml"
visu_yaml = str(sys.argv[1]) + "/flowvr_visu.yml"
compute_exec = str(sys.argv[2])
visu_exec = str(sys.argv[3])

if os.environ.get('MPI_LIB') == "mpich":
  computerun = FlowvrRunMPICH(compute_exec + " " + compute_yaml, hosts = hosts, prefix = prefix)
elif os.environ.get('MPI_LIB') == "mvapich":
  computerun = FlowvrRunMVAPICH(compute_exec + " " + compute_yaml, hosts = hosts, prefix = prefix)
else: # assume openmpi
  computerun = FlowvrRunOpenMPI(compute_exec + " " + compute_yaml, hosts = hosts, prefix = prefix)

compute_modules = []
compute_output_ports = []
for i in range(4):
	compute_modules.append(Module_PDI(prefix + "/" + str(i), run = computerun, host = 'localhost', pdi_conf = compute_yaml))
	compute_output_ports.append(compute_modules[i].getPort("heat"))

compute_tree = generateNto1(prefix="comNto1Merge", in_ports = compute_output_ports, arity = 2, node_class = FilterMerge2D)

#visu
visu_module = Module_PDI("flowvr_visu", cmdline = visu_exec + " " + visu_yaml, pdi_conf = visu_yaml)

#presignal
presignal_filter = FilterPreSignal("preSignal")

#link compute -> visu
compute_tree.link(visu_module.getPort("heat"))

#link visu -> presignal
visu_module.getPort("endIt").link(presignal_filter.getPort("in"))

#presignal -> compute_modules
for i in range(4):
  presignal_filter.getPort("out").link(compute_modules[i].getPort("beginIt"))

app.generate_xml("flowvr")
