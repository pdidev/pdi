import sys

from flowvrapp import *
from filters import *

from pdi_flowvr import Module_PDI

nx, ny = 256, 256

if len(sys.argv) >= 3:
  nx, ny = int(sys.argv[1]), int(sys.argv[2])

fluid = Module_PDI("fluid", cmdline = "bin/fluid %d %d" % (nx, ny), pdi_conf = 'config/fluid.yml')
gldens = Module_PDI("gldens", cmdline = "bin/gldens 0 0 1", pdi_conf = 'config/gldens.yml')

# Connection from gldens to fluid simulation
pres = FilterPreSignal( "Pos_PreSignal", messagetype = 'full' )
gldens.getPort("positions").link(pres.getPort("in"))
pres.getPort("out").link(fluid.getPort("positions"))

# DENSITY DATA TRANSFER
# link fluid (port density)
fluid.getPort("density").link(gldens.getPort("density"))

# VELOCITY DATA TRANSFER
# link fluid (port velocity)
fluid.getPort("velocity").link(gldens.getPort("velocity"))

app.generate_xml("fluid")
