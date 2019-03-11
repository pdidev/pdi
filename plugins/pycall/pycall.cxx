/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <pybind11/pybind11.h>
#include <pybind11/embed.h>
#include <pybind11/numpy.h>

#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/python/tools.h>


namespace {

using PDI::Array_datatype;
using PDI::Context;
using PDI::Datatype;
using PDI::Error;
using PDI::Expression;
using PDI::len;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_rw;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;
using pydict = pybind11::dict;
using pymod = pybind11::module;
using pyobj = pybind11::object;
using namespace pybind11::literals;
using std::cerr;
using std::endl;
using std::exception;
using std::string;
using std::unordered_multimap;
using std::vector;

/** An alias mapping a python variable name to a PDI expression
 */
class Alias
{
private:
	/// name of the python variable to expose the alias as
	string m_name;
	
	/// PDI value that is aliased
	Expression m_value;
	
public:
	Alias(const string& name, const string& var):
		m_name {name},
		m_value {var}
	{}
	
	/** exposes the alias
	 */
	void expose(Context& ctx, pydict pyscope)
	{
		Ref r = m_value.to_ref(ctx);
		
		PDI::Data_descriptor& desc = ctx.desc(m_name);
		
		pyscope[m_name.c_str()] = to_python(r);
	}
	
}; // class Alias


/** A trigger for a function call
 */
class Trigger
{
	string m_code;
	
	/// all the aliases to setup
	vector<Alias> m_aliases;
	
public:
	/// parse tree to initialiaze this instance
	Trigger(string code, PC_tree_t with):
		m_code{move(code)}
	{
		if (!PC_status(PC_get(with, "{0}"))) {   // parameters
			int nwith = len(with);
			for (int ii = 0; ii < nwith; ii++) {
				string alias_name = to_string(PC_get(with, "{%d}", ii));
				string var = to_string(PC_get(with, "<%d>", ii));
				m_aliases.emplace_back(alias_name, var);
			}
		}
	}
	
	/// parse tree to initialiaze this instance
	Trigger(string code, string with):
		m_code{move(code)}
	{
		string var = string("$") + with;
		m_aliases.emplace_back(with, var);
	}
	
	/// call the function that has been registered
	void call(Context& ctx)
	{
		// a python context we fill with exposed variables
		pydict pyscope = pymod::import("__main__").attr("__dict__");
		pyscope["pdi"] = pymod::import("pdi");
		
		for (auto&& alias : m_aliases) {
			//create alias and share it with the plug-in
			alias.expose(ctx, pyscope);
		}
		try {
			pybind11::exec(m_code, pyscope);
		} catch ( const std::exception& e ) {
			cerr << " *** [PDI/PyCall] Error: while calling python, caught exception: "<<e.what()<<endl;
		} catch (...) {
			cerr << " *** [PDI/PyCall] Error: while calling python, caught exception"<<endl;
		}
	}
	
}; // class Trigger

struct pycall_plugin: Plugin {
	/// Trigger to call on named events
	unordered_multimap<string, Trigger> events_triggers;
	
	/// Trigger to call on data events
	unordered_multimap<string, Trigger> data_triggers;
	
	pybind11::scoped_interpreter interpreter;
	
	pycall_plugin(Context& ctx, PC_tree_t conf):
		Plugin{ctx}
	{
		// Loading configuration for events
		PC_tree_t on_event = PC_get(conf, ".on_event");
		int nb_events = len(on_event, 0);
		for (int map_id = 0; map_id < nb_events; map_id++) {
			string event_name = to_string(PC_get(on_event, "{%d}", map_id));
			PC_tree_t event = PC_get(on_event, "<%d>", map_id);
			events_triggers.emplace(event_name, Trigger {to_string(PC_get(event, ".exec")), PC_get(event, ".with")});
		}
		
		// Loading configuration for data
		PC_tree_t on_data = PC_get(conf, ".on_data");
		int nb_data = len(on_data, 0);
		for (int map_id = 0; map_id < nb_data; map_id++) {
			string data_name = to_string(PC_get(on_data, "{%d}", map_id));
			PC_tree_t data = PC_get(on_data, "<%d>", map_id);
			data_triggers.emplace(data_name, Trigger {to_string(data), data_name});
		}
		
	}
	
	void event(const char* event) override
	{
		auto&& evrange = events_triggers.equal_range(event);
		// invoke all required functions
		for (auto evit = evrange.first; evit != evrange.second; ++evit) {
			evit->second.call(context());
		}
	}
	
	void data(const char* name, Ref) override
	{
		auto&& dtrange = data_triggers.equal_range(name);
		// invoke all required functions
		for (auto dtit = dtrange.first; dtit != dtrange.second; ++dtit) {
			dtit->second.call(context());
		}
	}
	
}; // struct pycall_plugin

} // namespace <anonymous>

PDI_PLUGIN(pycall)
