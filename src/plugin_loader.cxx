/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

//The following is used for doxygen documentation:
/**
* \file plugin_loader.c
* \brief Contains function to load plugins
* \details Plugins name written in config.yml are read/parse by paraconf: if PDI is build with a plugins having the same name, the plugin is loaded.
* \author Julien Bigot (CEA) <julien.bigot@cea.fr>
*/
#include <cstdio>
#include <cerrno>
#include <cstdarg>
#include <sstream>

#include <dlfcn.h>

#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/state.h"
#include "pdi/status.h"

#include "plugin_loader.h"

namespace
{

using namespace PDI;
using std::string;
using std::stringstream;

typedef PDI_status_t (*init_f)(PC_tree_t conf, MPI_Comm *world, PDI_plugin_t *plugin);

void load_plugin(const char *plugin_name, PC_tree_t node, MPI_Comm *world, PDI_plugin_t *plugin)
{
	stringstream plugin_symbol;
	plugin_symbol << "PDI_plugin_" << plugin_name << "_ctor";
	void *plugin_ctor_uncast = dlsym(NULL, plugin_symbol.str().c_str());
	
	// case where the library was not prelinked
	if (!plugin_ctor_uncast) {
		stringstream libname;
		libname << "lib" << plugin_name << ".so";
		void *lib_handle = dlopen(libname.str().c_str(), RTLD_NOW);
		if (!lib_handle) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin file: %s", plugin_name, dlerror()};
		}
		plugin_ctor_uncast = dlsym(lib_handle, plugin_symbol.str().c_str());
		if (!plugin_ctor_uncast) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin from file: %s", plugin_name, dlerror()};
		}
	}
	
	// call the ctor
	(reinterpret_cast<init_f>(plugin_ctor_uncast))(node, world, plugin);
}

} // namespace <anonymous>

namespace PDI
{

void try_load_plugin(PC_tree_t conf, int plugin_id, MPI_Comm *world)
{
	string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
	try {
		PC_tree_t plugin_conf = PC_get(conf, ".plugins<%d>", plugin_id);
		PDI_plugin_t *plugin = new PDI_plugin_t;
		load_plugin(plugin_name.c_str(), plugin_conf, world, plugin);
		PDI_state.plugins.emplace(plugin_name, std::shared_ptr<PDI_plugin_t>(plugin));
	} catch (const std::exception &e) {
		throw Error{PDI_ERR_SYSTEM, "Error while loading plugin `%s': %s", plugin_name.c_str(), e.what()};
	}
}

}
