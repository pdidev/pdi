from flowvrapp import *
from filters import *


class Compute(Component):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port""" 
  
  def __init__(self, prefix, hosts):
    Component.__init__(self)
    
    # hosts: string with host names, separated by spaces
    computerun = FlowvrRunSSHMultiple('bin/compute', hosts = hosts, prefix = prefix)

    # hosts_list: convert hosts to a list
    hosts_list = hosts.split()
    
    # nb of instances
    ninstance = len(hosts_list) 

    merge = FilterMerge(prefix + '/merge')
    # collect beginIt's 
    all_beginIts = []

    for i in range(ninstance): 
      compute = Module(prefix + "/" + str(i), run = computerun, host = hosts_list[i])
      compute.add_port("primesOut", direction = "out")

      compute.getPort("primesOut").link(merge.new_port_in())
      all_beginIts.append(compute.getPort("beginIt"))
      
    self.ports["primesOut"] = merge.getPort("out")
    self.ports["beginIt"] = tuple(all_beginIts)
    

class Display(Component):  
  """ Displays primes in a window and another that the user can control with another window. """

  def __init__(self, prefix):
    Component.__init__(self)
    
    visu = Module(prefix + "/visu", cmdline = "bin/visu")
    visu.run.options += "-x DISPLAY "
    visu.add_port("primesIn", direction = "in")
    visu.add_port("keysIn", direction = "in")

    ### capture module to control the visu

    capture = Module(prefix + "/capture", cmdline = "bin/capture")
    capture.run.options += "-x DISPLAY "
    capture.add_port("keysOut")

    # greedy that samples captures's keysOut at the speed of visu's endIt
    greedy = Greedy(prefix + "/greedy")

    visu.getPort("endIt").link(greedy.getPort("sync"))
    capture.getPort("keysOut").link(greedy.getPort("in"))
    greedy.getPort("out").link(visu.getPort("keysIn"))
        
    ### expose input and output ports

    self.ports["endIt"] = visu.getPort("endIt")
    self.ports["primesIn"] = visu.getPort("primesIn")
    
app.default_host = "mohawk"

compute = Compute("compute", hosts = "mohawk " * 4 + "opata " * 4)
display = Display("display")
compute.getPort("primesOut").link(display.getPort("primesIn"))

### presignal that synchronizes compute with visu

presignal = FilterPreSignal("presignal", nb = 1)

display.getPort("endIt").link(presignal.getPort('in'))
presignal.getPort('out').link(compute.getPort("beginIt"))




app.generate_xml("primes")



