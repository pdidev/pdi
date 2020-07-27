
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

# 2 parrots 
parrotA = Parrot("parrotA", "A")
parrotB = Parrot("parrotB", "B")

# 1 ear
ear = Ear("ear")

# link the 2 parrots to the ear
merge = FilterMultiplex("mux")

parrotA.getPort("out").link(merge.newInputPort())
parrotB.getPort("out").link(merge.newInputPort())

merge.getPort("out").link(ear.getPort("in"))



app.generate_xml("test")
