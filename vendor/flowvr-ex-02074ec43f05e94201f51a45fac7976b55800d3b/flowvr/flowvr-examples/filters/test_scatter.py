

from flowvrapp import *
from filters import *


class LovecraftParrot(Module):
  """ sends out strophes of a poem """
  def __init__(self, name):
    Module.__init__(self, name, cmdline = "python modules/lovecraft_parrot.py")
    self.addPort("out", direction = "out")

class Ear(Module):
  """ ouputs all messages it receives """
  def __init__(self, name):
    Module.__init__(self, name, cmdline = "python modules/ear.py")
    self.addPort("in", direction = "in")

class Translator(Module):
  """ translates messages to uppercase """
  def __init__(self, name, signature):
    Module.__init__(self, name, cmdline = "python modules/translator.py " + signature)
    self.addPort("in", direction = "in")
    self.addPort("out", direction = "out")
  

# 1 parrot
parrot = LovecraftParrot("parrot")

# 1 ear
ear = Ear("ear")

# dispatches to translators
scatter = FilterScatter("scatter")
scatter.parameters['elementsize'] = 50
parrot.getPort("out").link(scatter.getPort("in"))

# combines results 
merge = FilterMerge("merge")

# 5 translators

for i in range(5):
  translator = Translator("trans%d" % i, str(i))

  scatter.new_port_out().link(translator.getPort("in"))
  translator.getPort("out").link(merge.newInputPort())


merge.getPort("out").link(ear.getPort("in"))


app.generate_xml("test")
