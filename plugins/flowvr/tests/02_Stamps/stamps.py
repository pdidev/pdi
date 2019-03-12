from flowvrapp import *

sendModule = Module("send", cmdline = "./send_stamp")
outport = sendModule.addPort("stamps_out", direction = "out") # how send messagetype = "stamps"?

recvModule = Module("recv", cmdline = "./recv_stamp")
inport = recvModule.addPort("stamps_in", messagetype = "stamps", direction = "in")

outport.link(inport)

app.generate_xml("stamps")
