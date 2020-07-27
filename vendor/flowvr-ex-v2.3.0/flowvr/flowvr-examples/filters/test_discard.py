
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
  

parrot = Parrot("parrot", "hi")

ear = Ear("ear")
discard = FilterDiscard("discard")

# messages pass through the filter
parrot.getPort("out").link(discard.getPort("in"))
discard.getPort("out").link(ear.getPort("in"))

# filter is controlled by master
master = Master("master")
master.getPort("out").link(discard.getPort("open"))


app.generate_xml("test")
