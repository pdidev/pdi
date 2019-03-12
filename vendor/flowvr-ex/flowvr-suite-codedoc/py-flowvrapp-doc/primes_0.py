from flowvrapp import *

computerun = FlowvrRunSSHMultiple('bin/compute', hosts = 'localhost', prefix = 'compute')
compute = Module("compute/0", run = computerun)
compute.add_port("primesOut", direction = "out")

visu = Module("visu", cmdline = "bin/visu")
visu.add_port("primesIn", direction = "in")

compute.getPort("primesOut").link(visu.getPort("primesIn"))

app.generate_xml("primes")
