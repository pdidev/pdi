from flowvrapp import *

sendModule = Module("send", cmdline = "./send_mouse")
outport = sendModule.addPort("mouseOut", direction = "out")

recvModule = Module("recv", cmdline = "./recv_mouse")
inport = recvModule.addPort("mouseIn", direction = "in")

outport.link(inport)

app.generate_xml("event_mouse")
