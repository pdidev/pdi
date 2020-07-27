from flowvrapp import *

putmodule = Module("put", cmdline = "bin/put", host = "mohawk")
outport = putmodule.add_port("text", direction = "out")

getmodule = Module("get", cmdline = "bin/get", host = "opata")
inport = getmodule.add_port("text", direction = "in")

outport.link(inport)

app.generate_xml("tictac")
