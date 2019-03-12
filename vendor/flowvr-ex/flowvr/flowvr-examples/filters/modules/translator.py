
import time, sys

import flowvr


ports = flowvr.vectorPort()
inport = flowvr.InputPort('in')
outport = flowvr.OutputPort('out')

ports.push_back(inport)
ports.push_back(outport)

module = flowvr.initModule(ports);

signature = sys.argv[1]

while module.wait():

  message = inport.get()

  s = message.data.asString()

  # pretty slow translator
  time.sleep(2)

  s = s.upper() + "(translator " + signature + ")"
  
  message = flowvr.MessageWrite()
  message.data = module.allocString(s)
  
  outport.put(message)

module.close()
