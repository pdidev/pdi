from flowvrapp import *

class Bundle:
  def __init__(self, name, host):

    run = FlowvrRunSSH("bin/putMultiple", "putMultiple")
    self.module1 = Module("putMultiple/Module1", run = run)
    self.module2 = Module("putMultiple/Module2",
                          run = run)

    #self.module1 = Module(name+"/Module1", cmdline = cmdline)
    #self.module2 = Module(name+"/Module2",
    #                      run = FlowvrRunFake(name+"/Module2"))
    #self.CamSync = Module(name+"/CamSync", run = " ")

bundle = Bundle("putMultiple", "localhost")
outport = bundle.module1.addPort("text", direction = "out")
outport2 = bundle.module2.addPort("text2", direction = "out")

getmodule = Module("getMultiple", cmdline = "bin/getMultiple")
inport = getmodule.addPort("text", direction = "in")
inport2 = getmodule.addPort("text2", direction = "in")

outport.link(inport)
outport2.link(inport2)

app.generate_xml("bundle")
