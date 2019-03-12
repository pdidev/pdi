from flowvrapp import *

sendModule = Module("send", cmdline = "./send_chunks")
outport = sendModule.addPort("message", direction = "out")

recvModule = Module("recv", cmdline = "./recv_chunks")
inport = recvModule.addPort("message", direction = "in")

outport.link(inport)

app.generate_xml("chunks")
