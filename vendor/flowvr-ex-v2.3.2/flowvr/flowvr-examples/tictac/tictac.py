from flowvrapp import *

#putmodule = Module("put", cmdline = "bin/put")
putmodule = Module("put", cmdline = ".") 
outport = putmodule.addPort("text", direction = "out")

getmodule = Module("get", cmdline = "bin/get")
inport = getmodule.addPort("text", direction = "in")

outport.link(inport)

app.generate_xml("tictac")
