from flowvrapp import *

sendModule = Module("send", cmdline = "./send_copy")
outport = sendModule.addPort("message", direction = "out")

recvModule = Module("recv", cmdline = "./recv_copy")
inport = recvModule.addPort("message", direction = "in")

outport.link(inport)

app.generate_xml("copy_send_recv")
