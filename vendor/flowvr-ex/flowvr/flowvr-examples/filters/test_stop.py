
from flowvrapp import *
from filters import *


class Parrot(Module):
  """ sends out the text at random intervals """
  def __init__(self, name, text):
    Module.__init__(self, name, cmdline = "python modules/parrot.py " + text)
    self.addPort("out", direction = "out")

class Ear(Module):
  """ ouputs all messages it receives """
  def __init__(self, name):
    Module.__init__(self, name, cmdline = "python modules/ear.py")
    self.addPort("in", direction = "in")

class Master(Module):
  """ sends message that flips on and off every 3 s """
  def __init__(self, name):
    Module.__init__(self, name, cmdline = "python modules/master.py")
    self.addPort("out", direction = "out")
    
    
    
class FilterStop(flowvrapp.Filter):
  """Buffer all messages from input if filter not open, forward them
  all on output otherwise. Open is true iff a data messages statrting
  with something else than byte 0 is receved"""

  def __init__(self, name, host = ''):
    flowvrapp.Filter.__init__(self, name, run = 'flowvr.plugins.Stop', host = host)
    self.addPort("in", direction = 'in')
    self.addPort("out", direction = 'out')
    self.addPort("open", direction = 'in', messagetype = 'full')
  

parrot = Parrot("parrot", "hi")

ear = Ear("ear")
stop = FilterStop("stop")

# messages pass through the filter
parrot.getPort("out").link(stop.getPort("in"))
stop.getPort("out").link(ear.getPort("in"))

# filter is controlled by master
master = Master("master")
master.getPort("out").link(stop.getPort("open"))


app.generate_xml("test")
