from flowvrapp import *
from filters import *


# Original TicTac example, calling the python implementation of the module
putmodule = Module("put", cmdline = "python tictac_module.py put")
outport = putmodule.addPort("text", direction = "out")


getmodule = Module("get", cmdline = "python tictac_module.py get")
inport = getmodule.addPort("text", direction = "in")

outport.link(inport)


# Add a max frequency  filter, to cap the producer frequency
freqsync = SyncMaxFrequency("freqsync")
freqsync.parameters["freq"] = 1
freqsync.getPort("out").link(freqsync.getPort("endIt"))
freqsync.getPort("out").link(putmodule.getPort("beginIt"))


# Add a spy filter 
#spy = Module("spy", cmdline = "xterm -e python spy_module.py")
#spy.addPort("in", direction = "in")
#outport.link(spy.getPort("in"))

app.generate_xml("tictac")



