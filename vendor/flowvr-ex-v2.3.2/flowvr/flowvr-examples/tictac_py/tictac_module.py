import sys, time
import flowvr



  

if sys.argv[1] == 'get':
  ports = flowvr.vectorPort()
  port = flowvr.InputPort('text')
  ports.push_back(port)
  module = flowvr.initModule(ports);
  
  while module.wait():
    message = port.get()   
    print("get receives {} at it {}".format(message.data.asString().decode(), message.getStamp("it")))


  module.close()
  

elif sys.argv[1] == 'put':

  ports = flowvr.vectorPort()
  port = flowvr.OutputPort('text')

  # define a few stamps 
  port.addStamp("mycounter", type(1))
  port.addStamp("myarray", type(1), 3)
  port.addStamp("mystring", type(''))

  ports.push_back(port)
  module = flowvr.initModule(ports);

  it = 0
  maxiter=50
  extras = []
  while module.wait():
    # port.stamps must be passed in because standard stamps can be
    # defined.
    message = flowvr.MessageWrite(port.stamps)

    # set values of stamps
    message.setStamp("mycounter", it + 1)
    message.setStamp(("myarray", 1), it + 1)
    message.setStamp("mystring", "this it iteration %d" % it)
        
    message.data = module.allocString("tic" if it & 1 else "tac")    

    port.put(message)
    it += 1
    time.sleep(0.4)
    if it > maxiter:
      break;

  module.abort()
  module.close()
