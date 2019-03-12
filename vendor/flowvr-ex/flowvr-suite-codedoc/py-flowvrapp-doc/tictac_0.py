from flowvrapp import *

putmodule = Module("put", cmdline = "bin/put")
outport = putmodule.add_port("text", direction = "out")

getmodule = Module("get", cmdline = "bin/get")
inport = getmodule.add_port("text", direction = "in")

outport.link(inport)

app.generate_xml("tictac")
