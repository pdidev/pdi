from flowvrapp import *


class PutModule(Module): 
  " a module that puts text messages on its text output port "
  def __init__(self, name, host = ""): 
    # call the parent constructor
    Module.__init__(self, name, cmdline = "bin/put", host = host)
    self.add_port("text", direction = "out")

putmodule = PutModule("put")

getmodule = Module("get", cmdline = "bin/get")
inport = getmodule.add_port("text", direction = "in")

outport = putmodule.getPort("text")
outport.link(inport)

app.generate_xml("tictac")

