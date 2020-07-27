from flowvrapp import *
import yaml

def add_port(flowvr_module, port_name, port_conf, direction):
	blockstate = "blocking"
	if 'event_port' in port_conf:
		if port_conf['event_port'] == True:
			blockstate = "nonblocking"
	messagetype = "full"
	if 'data' not in port_conf and 'chunks' not in port_conf and 'event_button' not in port_conf and 'event_mouse' not in port_conf:
		print(port_name + " is stamp only")
		messagetype = "stamps"
	addedPort = flowvr_module.addPort(port_name, messagetype = messagetype, direction = direction)
	addedPort.blockstate = blockstate


def add_ports(flowvr_module, module_conf):
	input_ports_dict = module_conf.get('input_ports', {})
	for port_name, port_conf in list(input_ports_dict.items()):
		add_port(flowvr_module, port_name, port_conf, "in")
	
	output_ports_dict = module_conf.get('output_ports', {})
	for port_name, port_conf in list(output_ports_dict.items()):
		add_port(flowvr_module, port_name, port_conf, "out")

def Module_PDI(name = None, cmdline = None, host = '', cores = '', run = None, pdi_conf = '', name_prefix = ''):
	if not pdi_conf:
		Module(name_prefix + name, cmdline = cmdline, host = host, cores = cores, run = run)
	else:
		with open(pdi_conf, 'r') as conf_file:
			root = yaml.safe_load(conf_file)
			pdi_root = root.get('pdi', root) # if file has pdi subtree use it
			plugins_node = pdi_root.get('plugins', False)
			if not plugins_node:
				raise NameError('\033[31m`' + pdi_conf + "' doesn't have `plugins' node\033[0m")
			flowvr_node = plugins_node.get('flowvr', False)
			if not flowvr_node:
				raise NameError('\033[31m`' + pdi_conf + "' doesn't have `flowvr' node\033[0m")

			#if flowvr_node is list with 1 element
			if isinstance(flowvr_node, list) and len(flowvr_node) == 1:
				flowvr_node = flowvr_node[0]

			if not isinstance(flowvr_node, list):
				#single module declared: flowvr_node is a node
				name_from_conf = flowvr_node.get('name', False)
				if not name:
					if not name_from_conf:
						raise NameError("\033[31mUnknown module name. `name' argument is None and `" + pdi_conf + "' doesn't have `name' node\033[0m")
					name = name_from_conf
				else:
					if name_from_conf and name_from_conf != name:
						print("Warning: `name' argument is `" + name + "', but name in pdi config is `" + name_from_conf + "'. Using `" + name + "'")
				flowvr_module = Module(name_prefix + name, cmdline = cmdline, host = host, cores = cores, run = run)
				add_ports(flowvr_module, flowvr_node)
				return flowvr_module
			else:
				#several modules declared: flowvr_node is a list
				if name:
					print("Warning: `name' argument is `" + name + "', but pdi config has several modules. `" + name + "' won't be used")
				name_conf_dict = {}
				for module in flowvr_node:
					name_node = module.get('name', False)
					if name_node:
						name_conf_dict[name_node] = module
					else:
						raise NameError("\033[31mFound module in `" + pdi_conf + "' that doesn't have `name' node\033[0m")
				flowvr_modules_dict = {}
				for module_name, module_conf in list(name_conf_dict.items()):
					flowvr_module =  Module(name_prefix + module_name, cmdline = cmdline, host = host, cores = cores, run = run)
					add_ports(flowvr_module, module_conf)
					flowvr_modules_dict[module_name] = flowvr_module
				return flowvr_modules_dict
