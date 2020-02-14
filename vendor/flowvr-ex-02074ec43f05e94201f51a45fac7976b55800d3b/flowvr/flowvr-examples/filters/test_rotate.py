
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

class Translator(Module):
  """ translates parrot messages to uppercase """
  def __init__(self, name, signature):
    Module.__init__(self, name, cmdline = "python modules/translator.py " + signature)
    self.addPort("in", direction = "in")
    self.addPort("out", direction = "out")
  

# 1 parrot
parrot = Parrot("parrot", "hi!")

# 1 ear
ear = Ear("ear")

# 2 slow translators
translatorA = Translator("transA", "A")
translatorB = Translator("transB", "B")

# dispatches to translators
rotate = FilterRotate("rotate")
parrot.getPort("out").link(rotate.getPort("in"))

# combines results 
unrotate = FilterUnrotate("unrotate")

# link translators to rotate/unrotate

rotate.new_port_out().link(translatorA.getPort("in"))
translatorA.getPort("out").link(unrotate.newInputPort())

rotate.new_port_out().link(translatorB.getPort("in"))
translatorB.getPort("out").link(unrotate.newInputPort())


unrotate.getPort("out").link(ear.getPort("in"))


app.generate_xml("test")
