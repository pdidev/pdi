

import flowvr

ports = flowvr.vectorPort()
port = flowvr.InputPort('toto')
ports.push_back(port)

try:
  print ports[4]
except IndexError:
  print "Behave! you're not in C++ here!"

module = flowvr.initModule(ports);

assert module



if 0: 
  message = flowvr.Message()
  
  flowvr.get(port, message)
else:

  message = flowvr.MessageWrite()
  
  bw = module.allocString("1")
  message.data = bw

