
import time
import flowvr


ports = flowvr.vectorPort()
port = flowvr.OutputPort('out')

ports.push_back(port)
module = flowvr.initModule(ports);

isopen = True

while module.wait():

  message = flowvr.MessageWrite()
  if isopen:
    print "you may talk"
    message.data = module.allocString("XXXXXXX")
  else:
    print "shut up"
    message.data = module.allocString("")  
  port.put(message)
  time.sleep(3)
  isopen = not isopen

  

module.close()
