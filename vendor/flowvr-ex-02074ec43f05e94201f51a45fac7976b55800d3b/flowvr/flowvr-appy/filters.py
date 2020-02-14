"""
Definition of basic FlowVR filters & synchronizers + a few standard
modules.

"""

import flowvrapp
import sys
import inspect


class FilterPreSignal(flowvrapp.Filter):
  """Filter that adds nb (a parameter) initial messages, then forward
  incoming messages to output (used to boot cycles).
  parameter nb:  number of initial messages.
  """
    
  def __init__(self, name, host = '', nb = 1, messagetype = 'stamps'):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.PreSignal', host = host)
    self.addPort('in', direction = 'in', messagetype = messagetype)
    self.addPort('out', direction = 'out', messagetype = messagetype)

    self.parameters['nb'] = nb


class RoutingNode(FilterPreSignal):
  """ Filter used to broadcats or route messages. Similar to FilterPreSignal except that nb is set to 0 and it works with full ports also"""

  def __init__(self, name, host = '', messagetype = 'full'):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.PreSignal', host = host)
    self.addPort('in', direction = 'in', messagetype = messagetype)
    self.addPort('out', direction = 'out', messagetype = messagetype)
    
    self.parameters['nb'] = 0
    
class FilterIt(flowvrapp.Filter):
  """Forward on its output the messages received on its input having
  the same it numbers than the ones received on the order
  port. Discard other messages (filter usually used with a
  synchronizer).
  """
  
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.FilterIt', host = host)
    self.addPort('in', direction = 'in')
    self.addPort('out', direction = 'out')    
    self.addPort('order', direction = 'in', messagetype = 'stamps')

class SyncMaxFrequency(flowvrapp.Synchronizer):
  """ Send a first message and then send output messages at
  min(incoming message rate, freq rate). The frequency must be integer. """
  
  def __init__(self, name, host = ''):
    flowvrapp.Synchronizer.__init__(self, name, run = 'flowvr.plugins.MaxFrequencySynchronizor', host = host)
    self.addPort('endIt', direction = 'in', messagetype = 'stamps')
    self.addPort('out', direction = 'out', messagetype = 'stamps')
    
    self.parameters['freq'] = 1   
    

class FilterDiscard(flowvrapp.Filter):
  """Discard all messages from input if filter not open, forward them
  all on output otherwise. Open is true iff a data messages statrting
  with something else than byte 0 is receved"""

  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.Discard', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("out", direction = 'out')
    self.addPort("open", direction = 'in', messagetype = 'full')
    
    
class FilterStop(flowvrapp.Filter):
  """Buffer all messages from input if filter not open, forward them
  all on output otherwise. Open is true iff a data messages statrting
  with something else than byte 0 is receved"""

  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.Stop', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("out", direction = 'out')
    self.addPort("open", direction = 'in', messagetype = 'full')


class FilterFiniteQueue(flowvrapp.Filter):
  """ Maintains messages in a queue of size queue_length, and delivers
  a number of the recent messages on request.
  """
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.Discard', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("out", direction = 'out')
    self.addPort("trigger", direction = 'in', messagetype = 'stamps')
    self.parameters["queue_length"] = 1



class SyncVarFrequency(flowvrapp.Synchronizer):
  """ Send a first message and then send output messages at min (incoming message rate, freq rate)
  Besides, the frequency rate can be modified at runtime (via the "freq" input)."""
  
  def __init__(self, name, host = ''):
    flowvrapp.Synchronizer.__init__(self, name, run = 'flowvr.plugins.VariableFrequencySynchronizor', host = host)
    self.addPort('endIt', direction = 'in', messagetype = 'stamps')
    self.addPort('freq', direction = 'in', messagetype = 'full')
    self.addPort('out', direction = 'out', messagetype = 'stamps')
    
    self.parameters['freq'] = 1
    self.parameters["freqHz"] = 1
    self.parameters["freqMin"] = 1
    self.parameters["freqMax"]  = 500
    

class GreedySynchronizor(flowvrapp.Synchronizer):
  """Every time it receives a message on endIt port, it looks for the
  most recent message received on stamps port and sends to order port
  the stamp of this message. Used for implementing greedy
  (subsampling) filtering."""

  def __init__(self, name, host = ''):
    flowvrapp.Synchronizer.__init__(self, name, run = 'flowvr.plugins.GreedySynchronizor', host = host)
    self.addPort('endIt', direction = 'in', messagetype = 'stamps')
    self.addPort('order', direction = 'out', messagetype = 'stamps')
    self.addPort('stamps', direction = 'in', messagetype = 'stamps')
    
    self.parameters['min'] = 0
    self.parameters['max'] = 2147483647
    self.parameters['buffer'] = 4
    self.parameters['stampname'] = 'it'
    self.parameters['advance'] = 0
    

class FilterMergeIt(flowvrapp.Filter):
  """Merges messages received on input port into one message sent on output port.

  More precisely, when it receives a message on the 'order' port, it
  inspects the incoming message queue ('in' port), discard all
  messages with a non null 'scratch' stamp value, concatenate all
  other messages in one message sent on 'out' port. This message has
  its 'scratch' stamp set to 0 and its 'stamp' stamp set to the sum of
  all 'stamp' stamps of the concatenated messages. The name of the
  'scratch' and 'stamp' stamps is set from the component parameter.
  """
  
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.MergeIt', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("order", direction = 'in', messagetype = 'stamps')
    self.addPort("out", direction = 'out')

    self.parameters["stamp"] = ""
    self.parameters["scratch"] = ""


class FilterLastOrNull(flowvrapp.Filter):
  """ Forward on output messages the last input message or null
  message if there is no input. Discard other messages"""
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.FilterLastOrNull', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("order", direction = 'in', messagetype = 'stamps')
    self.addPort("out", direction = 'out')
  
  

#################################################################
# Filters with a variable number of inputs/outputs

class FilterWithManyInputs(flowvrapp.Filter):
  """ generic Merge-type filter, inheritors should specify static
  fields plugin_name and messagetype"""
  
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = self.plugin_name, host = host)
    self.addPort('out', direction = 'out', messagetype = self.messagetype)    
    self.parameters['nb'] = 0

  def newInputPort(self):
    in_port = self.addPort('in%d' % self.parameters['nb'], direction = 'in', messagetype = self.messagetype)
    self.parameters['nb'] += 1 
    return in_port

class FilterMerge(FilterWithManyInputs):
  """Each time it receives a message on each input port, it sends one
  message consisting of the stamp of the message received on in0 and
  the concatenation of the data received on in0, in1, ... inNb-1"""
  plugin_name = 'flowvr.plugins.Merge'
  messagetype = 'full'
  
class FilterSignalAnd(FilterWithManyInputs):
  """Wait for receiving one message per input (nb inputs), and then
  send one output (use stamp read from message on in0)"""
  plugin_name = 'flowvr.plugins.SignalAnd'
  messagetype = 'stamps'
  
class FilterMultiplex(FilterWithManyInputs):
  """Each time it receives a message on one of its input ports, it
  forwards it to as an output message"""
  plugin_name = 'flowvr.plugins.MultiplexFilter'
  messagetype = 'full'

class FilterUnrotate(FilterWithManyInputs):
  """Expects a message on each of its input in turn, and sends it to output"""
  plugin_name = 'flowvr.plugins.UnrotateFilter'
  messagetype = 'full'
 
class FilterWithManyOutputs(flowvrapp.Filter):
  """ generic dispatch-like filter"""
  
  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = self.plugin_name, host = host)
    self.addPort('in', direction = 'in', messagetype = self.messagetype)    
    self.parameters['nb'] = 0

  def newOutputPort(self):
    out_port = self.addPort('out%d' % self.parameters['nb'], direction = 'out', messagetype = self.messagetype)
    self.parameters['nb'] += 1 
    return out_port

class FilterRotate(FilterWithManyOutputs):
  """Dispatches messages to each of its outputs in turn """
  plugin_name = 'flowvr.plugins.RotateFilter'
  messagetype = 'full'
  
class FilterScatter(FilterWithManyOutputs):
  """Split input message in nb parts of size elementsize and send each
  part to output out0, out1, ..., outNb-1"""
  plugin_name = 'flowvr.plugins.Scatter'
  messagetype = 'full'

  def __init__(self, name, elementsize = 1, host = ''):
    FilterWithManyOutputs.__init__(self, name, host)
    self.parameters['elementsize'] = elementsize

class BroadcastNode(FilterWithManyOutputs):
  """ Filter used to broadcast or route messages."""

  plugin_name = 'flowvr.plugins.PreSignal'
  messagetype = 'full'

  def __init__(self, name, host = '', messagetype = 'full'):
    FilterWithManyOutputs.__init__(self, name, host = host)
    self.addPort('out', direction = 'out', messagetype = messagetype)
    self.parameters['nb'] = 0 # no pre-signal
    self.nb_out = 0 # nb of output ports

  def newOutputPort(self):
    new_port_name = 'out%d' % ( self.nb_out )
    self.nb_out = self.nb_out + 1
    self.ports[new_port_name] = self.getPort('out')
    return self.ports[new_port_name]

    
#################################################################
# Greedy's 


class Greedy(flowvrapp.Component):
  """Composite filter that re-samples messages from a port to
  match the speed of another port.
  
  in: input port to resample
  out: output port
  sync: send a message on out each time a stamp is received on in
  """

  def __init__(self, prefix, filter_class = FilterIt, host = '', presignal_nb = 1):    
    flowvrapp.Component.__init__(self)

    filt = filter_class(prefix + "/filter", host = host)        
    sync = GreedySynchronizor(prefix + '/sync', host = host)
    ps = FilterPreSignal(prefix + "/presignal", nb = presignal_nb, host = host)

    ps.getPort('out').link(sync.getPort('endIt'))
    sync.getPort('order').link(filt.getPort('order'))

    # multiple input ports: an input will automatically be linked to all of them 
    self.ports['in'] = [filt.getPort('in'), sync.getPort('stamps')]
    self.ports['out'] = filt.getPort('out')
    self.ports['sync'] = ps.getPort('in')    

    self.filter, self.synchronizer, self.presignal = filt, sync, ps


class GreedyMultiple(flowvrapp.Component):
  """ Greedy that synchronizes several inputs on a single sync signal.  """ 
  
  def __init__(self, prefix, host = '', presignal_nb = 1):    
    flowvrapp.Component.__init__(self)

    self.sync = GreedySynchronizor(prefix + '/sync', host = host)
    self.ps = FilterPreSignal(prefix + "/presignal", nb = presignal_nb, host = host)
    self.sigand = FilterSignalAnd(prefix + "/signaland", host = host)

    self.ps.getPort('out').link(self.sync.getPort('endIt'))
    self.sigand.getPort('out').link(self.sync.getPort('stamps'))

    self.ports['sync'] = self.ps.getPort('in')
    
    self.nfilter = 0
    self.prefix = prefix
    
  def newPortPair(self, host = '', filter_class = FilterIt):
    """ get a new pair of ports (in, out) that will be synchronized """
    
    filt = filter_class(self.prefix + "/filters/%d" % self.nfilter, host = host)
     
    self.sync.getPort('order').link(filt.getPort('order'))

    in_port = self.ports['in-%d' % self.nfilter] = (filt.getPort('in'), self.sigand.newInputPort())
    out_port = self.ports['out-%d' % self.nfilter] = filt.getPort('out')

    self.nfilter += 1
    
    return (in_port, out_port)
  

#################################################################
# filter tree

def mostCommonHost(ports):
  " find most common host over a set of ports "
  
  # 'flatten' ports (replace tuples with components of tuple)
  ports_flat = []
  for port in ports: 
    if type(port)==type([]): ports_flat += port
    else:                    ports_flat.append(port)      
  hosts = [port.owner.host for port in ports_flat]
  hosts_with_count = [(hosts.count(h), h) for h in set(hosts)]
  return max(hosts_with_count)[1]
    
  

def generateNto1(prefix, in_ports,
                     arity = 2, node_class = FilterMerge):
  """
  returns: output port of the filter tree
  
  Make a N-to-1 tree where each node is a filter specified with node_class
  (support both filters wit a fixed number of input ports and FilterWithManyInputs). Input and output ports
  need to be known so that the hosts of the nodes can be chosen
  efficiently.

  prefix:      three nodes will be prefixed with this
  in_ports:    list of input ports
  arity:       nb of children per node
  node_class:  type of internal node to use
  """
  
  # Check parameters
  if not issubclass(in_ports.__class__, list):
    raise Exception, "in_ports parameter should be a list, found '%s' instead." % in_ports.__class__.__name__
  if not arity >= 2:
    raise Exception, "illegal value %d for arity. Arity represents the max nb of children per node in the filter tree." % arity
  if not issubclass(node_class, flowvrapp.Filter):
    raise Exception, "'%s' is not a subclass of 'filters.Filter'." % node_class

  node_i = 0

  # loop over tree levels, from leaves to root
  while len(in_ports) > 1: 
    new_ports = []
    
    # loop over nodes of current level    
    for i in range(0, len(in_ports), arity):

      # node takes these input ports
      ports = in_ports[i : i + arity]
      host = mostCommonHost(ports)
      
      if len(ports) == 1:
        new_ports.append(ports[0])
      else:
        # make new node
        node = node_class(prefix + "/node%d" % node_i, host = host)
        node_i += 1

        # First time we create a node, check that if the filter
        #  has fixed number of input ports, cit is compatible with arity
        # Also build a list of input port names
        if node_i == 1:
          inport_names = []
          if  not issubclass(node_class,FilterWithManyInputs):
            for i in node.ports:
              if node.ports[i].direction == "in":
                inport_names.append(i)
            if len(inport_names) != arity:
              raise Exception('Node %s has %d input ports. Cannot be used in tree of arity %d' % (node_class,len(inport_names),arity))
              

        # link input ports to node
        j = 0
        for port in ports:
           if issubclass(node_class,FilterWithManyInputs):
              port.link(node.newInputPort())
           else:
              port.link(node.getPort(inport_names[j]))
              j+=1
          
        new_ports.append(node.getPort('out'))
      
    in_ports = new_ports

  # link the root
  return in_ports[0]



def generate1toN(prefix, in_port, out_ports, max_arity = 2, node_class = FilterScatter):
  """
  Make a 1-to-N tree where each node is a filter specified with node_class.

  prefix:      three nodes will be prefixed with this
  in_port:    input port
  max_arity:      max output ports per node
  out_ports:    output ports
  node_class:  type of internal node to use
  """

  # Check parameters
  if not issubclass(in_port.__class__, flowvrapp.Port):
    raise Exception, "in_port should be a 'flowvrapp.Port', found '%s' instead." % in_port.__class__
  if not issubclass(out_ports.__class__, list):
    raise Exception, "out_port parameter should be a list, found '%s' instead." % out_ports.__class__.__name__
  if not max_arity >= 2:
    raise Exception, "illegal value %d for arity. Arity represents the max nb of children per node in the filter tree." % arity
  if not issubclass(node_class, FilterWithManyOutputs):
    raise Exception, "'%s' is not a subclass of 'filters.FilterWithManyOutputs'." % node_class
    
    
  node_i = 0
  
  # loop over tree levels, from leaves to root
  while len(out_ports) > 1: 
    new_ports = []
    
    # loop over nodes of current level    
    for i in range(0, len(out_ports), max_arity):

      # node takes these input ports
      ports = out_ports[i : i + max_arity]

      if len(out_ports) > max_arity: 
        host = mostCommonHost(ports)
      else:
        # last level before root: also take into account output in decision
        host = mostCommonHost(ports + [in_port])
      
      if len(ports) == 1:
        new_ports.append(ports[0])
      else:
        # make new node
        node = node_class(prefix + "/node%d" % node_i, host = host)
        node_i += 1

        # First time we create a node, check that if the filter
        #  has fixed number of input ports, cit is compatible with max_arity
        # Also build a list of input port names
        if node_i == 1:
          outport_names = []
          if  not issubclass(node_class,FilterWithManyOutputs):
            for i in node.ports:
              if node.ports[i].direction == "out":
                inport_names.append(i)
            if len(inport_names) != max_arity:
              raise Exception('Node %s has %d output ports. Cannot be used in tree of max_arity %d' % (node_class,len(outport_names),max_arity))
              

        # link input ports to node
        j = 0
        for port in ports:
           if issubclass(node_class,FilterWithManyOutputs):
              #port.link(node.newInputPort())
              node.newOutputPort().link(port)
           else:
              #port.link(node.getPort(inport_names[j]))
              node.getPort(outport_names[j]).link(port)
              j+=1
          
        new_ports.append(node.getPort('in'))
      
    out_ports = new_ports

  # link the root
  in_port.link(out_ports[0])
  



#################################################################
# A few standard modules (used eg. by traces)


class FWrite(flowvrapp.Module):
  """ writes all messages it gets to a file """
  
  def __init__(self, name, port_name = 'in', filename = '/tmp/dump.fwrite', host = '', raw = False):
    cmdline = "fwrite %s %s=%s " % ('-raw' if raw else '', port_name, filename)
    flowvrapp.Module.__init__(self, name, cmdline = cmdline, host = host)
    self.addPort(port_name, direction = 'in')
    

class FRead(flowvrapp.Module):
  """ read a file dumpled by fwrite, and replays the messages """
  
  def __init__(self, name, port_name = 'out', filename = '/tmp/dump.fwrite', host = ''):
    flowvrapp.Module.__init__(self, name, cmdline = "fread %s=%s" % (port, filename), host = host)
    self.addPort(port_name, direction = 'out')
    

class SpyModule(flowvrapp.Module):
  """This module can be connected on an ouput port, and reports some
  stats about the messages it receives, on an xterm window that it opens.
  Requires the Python module interface to be compiled."""
  
  def __init__(self, name, messagetype = 'full', host = ''):    
    flowvrapp.Module.__init__(self, name, cmdline = "python -m spy_module.py", host = host)
    # -m -> search spy_module.py in the PYTHONPATH
    self.addPort("in", direction = "in", messagetype = messagetype)
    self.run.options += '-x DISPLAY'


class DefaultLogger(flowvrapp.Filter):
  """Filter dedicated to traces : Collect traces from the
  daemon. Create a raw message that will be writen in a file
  """

  def __init__(self, name, host =''):
    flowvrapp.FlowvrdPrimitive.__init__(self, name, run = 'flowvr.plugins.DefaultLogger', host = host)
    self.parameters['period'] = 60
    self.parameters['buffer_nb'] = 5
    self.parameters['buffer_size'] = 512000
    self.addPort('log', direction = 'out')

   
