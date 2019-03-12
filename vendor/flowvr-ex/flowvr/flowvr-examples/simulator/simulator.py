import sys

from flowvrapp import *
from filters import *
"""
Simulator
"""
# parameters
nbEngine = 4
nbVisu = 1

# module definition
class Engine(Composite):
  def __init__(self, name, host = 'localhost', prefix = 'engine'):
    Composite.__init__(self)
    
    # use a paralell instance of the module
    computerun = FlowvrRunSSHMultiple('bin/engine', hosts = host, prefix = prefix)
    
    # hosts_list: convert hosts to a list
    hosts_list = host.split()
    
    # nb of instances
    ninstance = len(hosts_list) 

    all_simulatorOut = []
    
    for i in range(ninstance): 
      compute = Module(prefix + "/" + str(i), run = computerun, host = hosts_list[i])
      compute.addPort("simulatorOut", direction = "out")

      all_simulatorOut.append(compute.getPort("simulatorOut"))

    self.ports["simulatorOut"] = list(all_simulatorOut)
    
class Visualization(Module):
  def __init__(self, name, host = 'localhost'):
    cmdline = 'bin/visualization'
    Module.__init__(self, name, cmdline = cmdline, host = host)
    self.addPort("simulatorIn", direction = "in")
    

if __name__ == '__main__':

  # Collection of Visualization modules
  visu = []
  
  engine = Engine("Simulator","localhost " * nbEngine)
  
  
  ## FOR EACH VISUALIZATION ##
  for i in range(nbVisu):
    v = Visualization("visu-%d" % i)
    visu.append(v)
    
    # Link Greedy output to visualization
    g = Greedy("ToSample-%d" % i)
    g.getPort("out").link(v.getPort("simulatorIn"))
    v.getPort("endIt").link(g.getPort("sync"))
    
    #### VISU INPUT #### Merge Engine  ===> Visualization
    treeOut = generateNto1('simulator/tree' + str(i), engine.getPort("simulatorOut"))
    treeOut.link(g.getPort("in"))
     

  app.generate_xml("simulator")
