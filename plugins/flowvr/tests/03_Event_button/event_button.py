from flowvrapp import *

sendModule = Module("send", cmdline = "./send_button")
outport = sendModule.addPort("keysOut", direction = "out")

recvModule = Module("recv", cmdline = "./recv_button")
inport = recvModule.addPort("keysIn", direction = "in")

outport.link(inport)

app.generate_xml("event_button")
