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

#include <mpi.h>

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <dlfcn.h>
#include <link.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/expression.h>


namespace {

using PDI::Context;
using PDI::Data_descriptor;
using PDI::Ref;
using PDI::Error;
using PDI::len;
using PDI::Plugin;
using PDI::Expression;
using PDI::to_string;
using std::cerr;
using std::endl;
using std::exception;
using std::string;
using std::unordered_multimap;
using std::vector;

/** An alias from one variable name to another
 */
class Alias;

/** An instance standing for an exposed alias. Exposure ends when the instance
 * is destroyed (RAII)
 */
class ExposedAlias
{
	Data_descriptor* m_desc;
	
public:
	ExposedAlias(Context& ctx, const Alias& alias);
	
	ExposedAlias(const ExposedAlias&) = delete;
	
	ExposedAlias(ExposedAlias&& alias);
	
	/** stop exposing the alias
	    */
	~ExposedAlias()
	{
		if (m_desc) m_desc->release();
	}
	
}; // class ExposedAlias

class Alias
{
private:
	/// name to expose the alias as
	string m_name;
	
	/// value that is aliased
	Expression m_value;
	
public:
	Alias(const string& name, const string& var):
		m_name {name},
		m_value {var}
	{}
	
	friend class ExposedAlias;
	
	/** exposes the alias
	    */
	ExposedAlias expose(Context& ctx)
	{
		return ExposedAlias{ctx, *this};
	}
	
}; // class Alias

ExposedAlias::ExposedAlias(Context& ctx, const Alias& alias):
	m_desc{&ctx.desc(alias.m_name.c_str())}
{
	try {
		m_desc->share(alias.m_value.to_ref(ctx), false, false);
	} catch (const Error& e) {
		throw Error {e.status(), "Could not alias `%s' because %s", alias.m_name.c_str(), e.what()};
	}
}

ExposedAlias::ExposedAlias(ExposedAlias&& alias):
	m_desc {alias.m_desc}
{
	alias.m_desc = NULL;
}


/** A trigger for a function call
    */
class Trigger
{
	/// function pointer
	typedef void (*ptr_fct_t)(void);
	
	/// function to call on event
	ptr_fct_t m_fct;
	
	/// all the aliases to setup
	vector<Alias> m_aliases;
	
public:
	/// parse tree to initialiaze this instance
	Trigger(string funcname, PC_tree_t params)
	{
		void* fct_uncast = dlsym(RTLD_DEFAULT, funcname.c_str());
		if (!fct_uncast) { // force loading from the main exe
			void* exe_handle = dlopen(NULL, RTLD_NOW);
			if (!exe_handle) {
				throw Error {PDI_ERR_SYSTEM, "Unable to dlopen the main executable: %s", dlerror()};
			}
			fct_uncast = dlsym(exe_handle, funcname.c_str());
		}
		if (!fct_uncast) {
			throw Error {PDI_ERR_SYSTEM, "Unable to load user function `%s': %s", funcname.c_str(), dlerror()};
		}
		m_fct = reinterpret_cast<ptr_fct_t>(fct_uncast);
		
		if (!PC_status(PC_get(params, "{0}"))) {   // parameters
			int nparams = len(params);
			for (int ii = 0; ii < nparams; ii++) {
				string alias_name = to_string(PC_get(params, "{%d}", ii));
				string var = to_string(PC_get(params, "<%d>", ii));
				m_aliases.emplace_back(alias_name, var);
			}
		}
	}
	
	/// call the function that has been registered
	void call(Context& ctx)
	{
		// all exposed aliases that will be unexposed on destroy
		vector<ExposedAlias> exposed_aliases;
		for (auto&& alias : m_aliases) {
			//create alias and share it with the plug-in
			exposed_aliases.emplace_back(alias.expose(ctx));
		}
		try {
			m_fct();
		} catch ( const std::exception& e ) {
			cerr << "while calling user code, caught exception: "<<e.what()<<endl;
		} catch (...) {
			cerr << "while calling user code, caught exception"<<endl;
		}
	}
	
}; // class Trigger

struct user_code_plugin: Plugin {
	/// User-code to call on named events
	unordered_multimap<string, Trigger> events_uc;
	
	/// User-code to call on data events
	unordered_multimap<string, Trigger> data_uc;
	
	user_code_plugin(Context& ctx, PC_tree_t conf, MPI_Comm*):
		Plugin{ctx}
	{
		// Loading configuration for events
		PC_tree_t on_event = PC_get(conf, ".on_event");
		int nb_events = len(on_event, 0);
		for (int map_id = 0; map_id < nb_events; map_id++) {
			string event_name = to_string(PC_get(on_event, "{%d}", map_id));
			PC_tree_t event = PC_get(on_event, "<%d>", map_id);
			if (!PC_status(PC_get(event, ".call"))) {
				events_uc.emplace(event_name, Trigger {to_string(PC_get(event, ".call")), PC_get(event, ".params")});
			} else {
				int nb_call = len(event, 0);
				for (int call_id = 0; call_id < nb_call; ++call_id) {
					events_uc.emplace(event_name, Trigger {to_string(PC_get(event, "{%d}", call_id)), PC_get(event, "<%d>", call_id)});
				}
			}
		}
		
		// Loading configuration for data
		PC_tree_t on_data = PC_get(conf, ".on_data");
		int nb_data = len(on_data, 0);
		for (int map_id = 0; map_id < nb_data; map_id++) {
			string data_name = to_string(PC_get(on_data, "{%d}", map_id));
			PC_tree_t data = PC_get(on_data, "<%d>", map_id);
			if (!PC_status(PC_get(data, ".call"))) {
				events_uc.emplace(data_name, Trigger {to_string(PC_get(data, ".call")), PC_get(data, ".params")});
			} else {
				int nb_call = len(data, 0);
				for (int call_id = 0; call_id < nb_call; ++call_id) {
					events_uc.emplace(data_name, Trigger {to_string(PC_get(data, "{%d}", call_id)), PC_get(data, "<%d>", call_id)});
				}
			}
		}
	}
	
	void event(const char* event) override
	{
		auto&& evrange = events_uc.equal_range(event);
		// invoke all required functions
		for (auto evit = evrange.first; evit != evrange.second; ++evit) {
			evit->second.call(context());
		}
	}
	
	void data(const char* name, Ref) override
	{
		auto&& dtrange = events_uc.equal_range(name);
		// invoke all required functions
		for (auto dtit = dtrange.first; dtit != dtrange.second; ++dtit) {
			dtit->second.call(context());
		}
	}
	
}; // struct user_code_plugin

} // namespace <anonymous>

PDI_PLUGIN(user_code)
