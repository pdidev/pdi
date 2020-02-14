
import sys, time, random
import flowvr


ports = flowvr.vectorPort()
port = flowvr.InputPort('in')

ports.push_back(port)
module = flowvr.initModule(ports);

while module.wait():
  message = port.get()

  print "heard", message.data.asString()
  

module.close()
