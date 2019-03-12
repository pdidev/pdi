from flowvrapp import *

sendModule = Module("send", cmdline = "./send_wait_event")
outport = sendModule.addPort("message", direction = "out")

recvModule = Module("recv", cmdline = "./recv_wait_event")
inport = recvModule.addPort("message", direction = "in")

outport.link(inport)

app.generate_xml("wait_event")
