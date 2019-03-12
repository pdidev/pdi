from flowvrapp import *
from pdi_flowvr import Module_PDI

run = FlowvrRunSSH("bin/putMultiple", "putMultiple")
putmodules = Module_PDI(run = run, pdi_conf = "config/putMultiple.yml", name_prefix = 'putMultiple/')
getmodule = Module_PDI("getMultiple", cmdline = "bin/getMultiple", pdi_conf = "config/getMultiple.yml")

putmodules["Module1"].getPort("text").link(getmodule.getPort("text"))
putmodules["Module2"].getPort("text2").link(getmodule.getPort("text2"))

app.generate_xml("bundle")
