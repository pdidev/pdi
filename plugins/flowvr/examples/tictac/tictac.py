from flowvrapp import *
from pdi_flowvr import Module_PDI

putmodule = Module_PDI("put", cmdline = "./put", pdi_conf = "put.yml")
getmodule = Module_PDI("get", cmdline = "./get", pdi_conf = "get.yml")

putmodule.getPort("text").link(getmodule.getPort("text"))

app.generate_xml("tictac")
