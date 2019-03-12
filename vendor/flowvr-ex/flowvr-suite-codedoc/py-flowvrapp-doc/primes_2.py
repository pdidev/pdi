from flowvrapp import *
from filters import *

computerun = FlowvrRunSSHMultiple('bin/compute', hosts = 'localhost', prefix = 'compute')
compute = Module("compute/0", run = computerun)
compute.add_port("primesOut", direction = "out")

visu = Module("visu", cmdline = "bin/visu")
visu.add_port("primesIn", direction = "in")

compute.getPort("primesOut").link(visu.getPort("primesIn"))

### presignal that synchronizes compute with visu

# nb is the number of messages to send before the first wait
presignal = FilterPreSignal("presignal", nb = 1)

visu.getPort("endIt").link(presignal.getPort('in'))
presignal.getPort('out').link(compute.getPort("beginIt"))


app.generate_xml("primes")



