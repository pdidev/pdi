
import sys, time, random
import flowvr


ports = flowvr.vectorPort()
port = flowvr.OutputPort('out')

ports.push_back(port)
module = flowvr.initModule(ports);


text = sys.argv[1]
no = 0

while module.wait():
  time.sleep(0.1 + 0.9 * random.random())

  message = flowvr.MessageWrite()

  tosay = text + " " + str(no) + " "
  message.data = module.allocString(tosay)

  print "say", tosay
  port.put(message)
  no += 1

module.close()
