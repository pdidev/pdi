import sys

from flowvrapp import *
from filters import *

"""
  Change to CPU core numbers of your choosing.
  One entry in compute_cores corresponds to one entry in compute_hosts.
  You can specify multiple cores for a single module (taskset syntax).
"""

compute_cores = '0 0,1 2,3 2,3' 
compute_hosts = 'localhost ' * 4

#####################################################################
# With composites, to improve readablility

class Display(Composite):  
  """ Displays primes in a window. The user can control the view with
  another window."""

  def __init__(self, prefix):
    Component.__init__(self)
    
    visu = Module(prefix + "/visu", cmdline = "bin/visu")
    visu.addPort("primesIn", direction = "in")
    visu.addPort("keysIn", direction = "in")

    ### capture module to control the visu

    capture = Module(prefix + "/capture", cmdline = "bin/capture")
    capture.addPort("keysOut")

    ### make sure the DISPLAY environment variable is available 
    
    visu.run.options += "-x DISPLAY "
    capture.run.options += "-x DISPLAY "        

    ### the presignal has to be duplicated 

    presignal = FilterPreSignal(prefix + "/presignal", nb = 1)
    visu.getPort("endIt").link(presignal.getPort('in'))

    # greedy that samples captures's keysOut at the speed of visu's endIt
    greedy = Greedy(prefix + "/greedy")

    visu.getPort("endIt").link(greedy.getPort("sync"))
    capture.getPort("keysOut").link(greedy.getPort("in"))
    greedy.getPort("out").link(visu.getPort("keysIn"))

    ### expose input and output ports

    self.ports["endIt"] = visu.getPort("endIt")
    self.ports["primesIn"] = visu.getPort("primesIn")
    
    

class ComputeWithTree(Composite):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port. This version
  merges the computer's results with a tree. """ 
  
  def __init__(self, prefix, hosts, out_port):
    Composite.__init__(self)
    
    # hosts: string with host names, separated by spaces
    computerun = FlowvrRunSSHMultiple('bin/compute', hosts = hosts, prefix = prefix, cores = compute_cores)

    # hosts_list: convert hosts to a list
    hosts_list = hosts.split()
    
    # nb of instances
    ninstance = len(hosts_list) 

    all_beginIts = []
    all_primesOut = []

    for i in range(ninstance): 
      compute = Module(prefix + "/" + str(i), run = computerun, host = hosts_list[i])
      compute.addPort("primesOut", direction = "out")

      all_beginIts.append(compute.getPort("beginIt"))
      all_primesOut.append(compute.getPort("primesOut"))
      
    treeOut = generateNto1(prefix + '/tree', all_primesOut)
    treeOut.link(out_port)

    self.ports["beginIt"] = list(all_beginIts)



# take first host as the one on which we display...
app.default_host = compute_hosts.split()[0]

display = Display("display")

compute = ComputeWithTree("compute", hosts = compute_hosts, out_port = display.getPort("primesIn"))

### presignal that synchronizes compute with visu

presignal = FilterPreSignal("presignal", nb = 1)

display.getPort("endIt").link(presignal.getPort('in'))
presignal.getPort('out').link(compute.getPort("beginIt"))


app.generate_xml('primes')
