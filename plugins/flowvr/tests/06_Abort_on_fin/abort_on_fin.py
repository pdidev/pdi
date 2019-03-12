from flowvrapp import *

sendModule = Module("send", cmdline = "./send_msg_fin")
outport = sendModule.addPort("message", direction = "out")

recvModule = Module("recv", cmdline = "./recv_msg_fin")
inport = recvModule.addPort("message", direction = "in")

outport.link(inport)

app.generate_xml("abort_on_fin")
