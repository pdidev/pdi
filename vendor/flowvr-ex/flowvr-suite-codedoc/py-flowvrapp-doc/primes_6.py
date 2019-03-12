from flowvrapp import *
from filters import *

computerun = FlowvrRunSSHMultiple('bin/compute', hosts = 'localhost', prefix = 'compute')
compute = Module("compute/0", run = computerun)
compute.add_port("primesOut", direction = "out")


class Display(Component):  
  """ Displays primes in a window and another that the user can control with another window. """

  def __init__(self, prefix):
    Component.__init__(self)
    
    visu = Module(prefix + "/visu", cmdline = "bin/visu")
    visu.add_port("primesIn", direction = "in")
    visu.add_port("keysIn", direction = "in")

    ### capture module to control the visu

    capture = Module(prefix + "/capture", cmdline = "bin/capture")
    capture.add_port("keysOut")

    # greedy that samples captures's keysOut at the speed of visu's endIt
    greedy = Greedy(prefix + "/greedy")

    visu.getPort("endIt").link(greedy.getPort("sync"))
    capture.getPort("keysOut").link(greedy.getPort("in"))
    greedy.getPort("out").link(visu.getPort("keysIn"))
        
    ### expose input and output ports

    self.ports["endIt"] = visu.getPort("endIt")
    self.ports["primesIn"] = visu.getPort("primesIn")
    

display = Display("display")
compute.getPort("primesOut").link(display.getPort("primesIn"))

### presignal that synchronizes compute with visu

# nb is the number of messages to send before the first wait
presignal = FilterPreSignal("presignal", nb = 1)

display.getPort("endIt").link(presignal.getPort('in'))
presignal.getPort('out').link(compute.getPort("beginIt"))


app.generate_xml("primes")



