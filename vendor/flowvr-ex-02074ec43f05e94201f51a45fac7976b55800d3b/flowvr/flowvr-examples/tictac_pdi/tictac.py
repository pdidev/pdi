from flowvrapp import *
from flowvrapp_pdi  import Module_PDI

putmodule = Module_PDI("put", cmdline = "bin/put", pdi_conf = "bin/put.yml")
getmodule = Module_PDI("get", cmdline = "bin/get", pdi_conf = "bin/get.yml")

putmodule.getPort("text").link(getmodule.getPort("text"))

app.generate_xml("tictac")
