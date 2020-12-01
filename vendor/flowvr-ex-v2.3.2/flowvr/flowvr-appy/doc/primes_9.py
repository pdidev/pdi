from flowvrapp import *
from filters import *




def most_common_host(ports):
  " find most common host over the ports "
  
  # 'flatten' ports (replace tuples with components of tuple)
  ports_flat = []
  for port in ports: 
    if type(port)==type(()): ports_flat += port
    else:                    ports_flat.append(port)      
  hosts = [port.owner.host for port in ports_flat]
  hosts_with_count = [(hosts.count(h), h) for h in set(hosts)]
  return max(hosts_with_count)[1]
    
  

def make_filter_tree(prefix, in_ports, out_port, 
                     arity = 2, node_class = FilterMerge):
  """
  Make a tree where each node is a filter specified with node_class
  (should behave like a FilterWithManyInputs). Input and output ports
  need to be known so that the hosts of the nodes can be chosen
  efficently.

  prefix:    three nodes will be prefixed with this
  in_ports:  list of input ports
  out_port:  output port
  arity:     nb of children per node
  """

  cur_path = app.current_path
  app.current_path += prefix + '/'

  node_i = 0

  # loop over tree levels, from leaves to root
  while len(in_ports) > 1:
    new_ports = []
    
    # loop over nodes of current level    
    for i in range(0, len(in_ports), arity):

      # node takes these input ports
      ports = in_ports[i : i + arity]

      if len(in_ports) > arity: 
        host = most_common_host(ports)
      else:
        # also take into account output in decision
        host = most_common_host(ports + [out_port])

      # make new node
      node = node_class("node%d" % node_i, host = host)
      node_i += 1

      # link input ports to node
      for port in ports:
        port.link(node.new_port_in())
        
      new_ports.append(node.getPort('out'))
      
    in_ports = new_ports

  # link the root
  in_ports[0].link(out_port)

  app.current_path = cur_path





class Compute(Component):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port""" 
  
  def __init__(self, prefix, hosts, out_port):
    Component.__init__(self)
    
    # hosts: string with host names, separated by spaces
    computerun = FlowvrRunSSHMultiple('bin/compute', hosts = hosts, prefix = prefix)

    # hosts_list: convert hosts to a list
    hosts_list = hosts.split()
    
    # nb of instances
    ninstance = len(hosts_list) 

    all_beginIts = []
    all_primesOut = []

    for i in range(ninstance): 
      compute = Module(prefix + "/" + str(i), run = computerun, host = hosts_list[i])
      compute.add_port("primesOut", direction = "out")

      all_beginIts.append(compute.getPort("beginIt"))
      all_primesOut.append(compute.getPort("primesOut"))
      
    make_filter_tree(prefix + '/tree', all_primesOut, out_port)

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

display = Display("display")

hosts = "mohawk " * 4 + "opata " * 4

compute = Compute("compute", hosts = hosts, out_port = display.getPort("primesIn"))

### presignal that synchronizes compute with visu

presignal = FilterPreSignal("presignal", nb = 1)

display.getPort("endIt").link(presignal.getPort('in'))
presignal.getPort('out').link(compute.getPort("beginIt"))




app.generate_xml("primes")



