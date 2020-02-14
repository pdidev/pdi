
from flowvrapp import *

from filters import *



freqsync = SyncMaxFrequency("freqsync")
freqsync.parameters["freq"] = 10

# ps = FilterPreSignal("presig", nb = 1)

# ps.getPort("out").link(freqsync.getPort("endIt"))
# freqsync.getPort("out").link(ps.getPort("in"))
                    
freqsync.getPort("out").link(freqsync.getPort("endIt"))

putmodule = Module("put", cmdline = "python tictac_module.py put")
outport = putmodule.add_port("text", direction = "out")

freqsync.getPort("out").link(putmodule.getPort("beginIt"))

getmodule = Module("get", cmdline = "python tictac_module.py get")
inport = getmodule.add_port("text", direction = "in")

outport.link(inport)

spy = Module("spy", cmdline = "xterm -e python spy_module.py")
spy.add_port("in", direction = "in")

outport.link(spy.getPort("in"))

app.generate_xml("tictac")



