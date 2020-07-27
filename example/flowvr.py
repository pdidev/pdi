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

if os.environ.get('MPI_LIB') == "mpich":
  computerun = FlowvrRunMPICH(str(sys.argv[2]) + " " + compute_yaml, hosts = hosts, prefix = prefix)
elif os.environ.get('MPI_LIB') == "mvapich":
  computerun = FlowvrRunMVAPICH(str(sys.argv[2]) + " " + compute_yaml, hosts = hosts, prefix = prefix)
else: # assume openmpi
  computerun = FlowvrRunOpenMPI(str(sys.argv[2]) + " " + compute_yaml, hosts = hosts, prefix = prefix)

compute_modules = []
compute_output_ports = []
for i in range(4):
	compute_modules.append(Module_PDI(prefix + "/" + str(i), run = computerun, host = 'localhost', pdi_conf = compute_yaml))
	compute_output_ports.append(compute_modules[i].getPort("heat"))

compute_tree = generateNto1(prefix="comNto1Merge", in_ports = compute_output_ports, arity = 2, node_class = FilterMerge2D)

#visu
visu_module = Module_PDI("flowvr_visu", cmdline = str(sys.argv[3]) + " " + visu_yaml, pdi_conf = visu_yaml)

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
