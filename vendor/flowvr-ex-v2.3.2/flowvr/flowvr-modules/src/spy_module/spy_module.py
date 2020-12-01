"""
This module can be connected on an ouput port, and reports some stats
about the messages it receives. It expects to run in an xterm window.
"""

import sys, time, os

import flowvr

# dup stdout to a pipe

fifoname = os.tempnam()
os.mkfifo(fifoname)

print "spy module using fifo", fifoname

try:  
  os.system("xterm -title \"%s\" -e cat %s &" % (os.getenv("FLOWVR_MODNAME"), fifoname))
  out = open(fifoname, "w")

  clear_screen = "\033[H\033[2J"

  ports = flowvr.vectorPort()
  port = flowvr.InputPort('in')
  ports.push_back(port)

  module = flowvr.initModule(ports)

  t0 = time.time()
  n = 0

  while module.wait():
    message = port.get()   

    print >>out, clear_screen

    print >>out,"Spy", module.getID()
    print >>out,"Received message %d of %d + %d bytes at t = %.3f s" % (
      n, 
      message.data.getSize(),
      message.stamps.getSize(),
      time.time() - t0) 
    print >>out

    print >>out,"Stamps:"
    stamps = message.getStamps()
    for name in sorted(stamps.keys()):
      print >>out,"%-30s %s" % (name, stamps[name])

    out.flush()

    n+=1
  module.close()

finally:
  os.unlink(fifoname)

