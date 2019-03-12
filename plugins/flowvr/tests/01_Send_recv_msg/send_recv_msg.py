from flowvrapp import *

sendModule = Module("send", cmdline = "./send_msg")
outport = sendModule.addPort("message", direction = "out")

recvModule = Module("recv", cmdline = "./recv_msg")
inport = recvModule.addPort("message", direction = "in")

outport.link(inport)

app.generate_xml("send_recv_msg")
