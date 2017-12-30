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

#include <mpi.h>

#include <cstring>
#include <iostream>
#include <unordered_set>
#include <unordered_map>
#include <vector>

#include <dlfcn.h> 	// dynamic loading of function 

#include <pdi.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>


/// Verbose level : 0 Error, 1 Warning, 2 Debug
#define UC_verbose 1

#if UC_verbose > 1
#define UC_dbg(...) do{ fprintf(stderr, "[PDI/user_code] Debug: ");\
		fprintf(stderr, __VA_ARGS__);\
		fprintf(stderr, "\n"); \
		fflush(stderr); } while(0);
#else  // Does nothing
#define UC_dbg(...)
#endif

#if UC_verbose > 0
#define UC_warn(...) do { fprintf(stderr, "[PDI/user_code] Warning: ");\
		fprintf(stderr, __VA_ARGS__);\
		fprintf(stderr, "\n"); \
		fflush(stderr);} while(0);
#else  // Does nothing
#define UC_warn(...)
#endif


namespace {

using PDI::Error;
using PDI::len;
using PDI::to_string;
using std::string;
using std::unordered_set;
using std::unordered_map;
using std::vector;

/// function pointer
typedef void (*ptr_fct_t)(void);

/// Structure for aliases
class Alias
{
private:
	/// name to expose the alias as
	string m_name;
	
	/// variable that is aliased
	string m_var;
	
public:
	Alias(const string& name, const string& var):
			m_name{name},
			m_var{var}
	{}
	
	/** exposes the alias
	 */
	void expose()
	{
		try {
			PDI_state.desc(m_name).share(PDI_state.desc(m_var).ref(), false, false);
		} catch (const Error& e) {
			throw Error{e.status(), "Could not alias `%s' as `%s' because %s", m_var.c_str(), m_name.c_str(), e.what()};
		}
	}
	
	/** stop exposing the alias
	 */
	void unexpose()
	{
		PDI_state.desc(m_name).release();
	}
	
};


/** Structure to organize the interaction between functions and PDI
 *
 * When a specific event happens, call a function
 */
class Trigger
{
private:
	/// function to call on event
	ptr_fct_t m_fct;
	
	/// all the aliases to setup
	vector<Alias> m_aliases;
	
public:
	/// parse tree to initialiaze this instance
	Trigger(PC_tree_t config)
	{
		string funcname = to_string(PC_get(config, ".call"));
		
		void *fct_uncast = dlsym(NULL, funcname.c_str());
		if (!fct_uncast) {
			throw Error{PDI_ERR_SYSTEM, "Unable to load fct `%s': %s", funcname.c_str(), dlerror()};
		}
		// ugly data to function ptr cast to be standard compatible (though undefined behavior)
		m_fct = *((ptr_fct_t *)&fct_uncast);
		
		PC_tree_t params = PC_get(config, ".params");
		if ( !PC_status(params) ) { // parameters
			int nparams = len(params);
			for (int ii = 0; ii < nparams; ii++) {
				string alias_name = to_string(PC_get(params, "{%d}", ii));
				string var = to_string(PC_get(params, "<%d>", ii));
				m_aliases.emplace_back(alias_name, var);
			}
		}
	}
	
	/// call the function that has been registered
	void call()
	{
		for (auto &&alias : m_aliases) {
			alias.expose(); //create alias and share it with the plug-in
		}
		m_fct();
		for (auto &&alias : m_aliases) {
			alias.unexpose(); //create alias and share it with the plug-in
		}
	}
	
};


/// User-code to call on named events
unordered_map<string, Trigger> events_uc;

/// User-code to call on data events
unordered_map<string, Trigger> data_uc;


PDI_status_t PDI_user_code_init(PC_tree_t conf, MPI_Comm *)
{
	PDI_status_t status = PDI_OK;
	
	events_uc.clear();
	data_uc.clear();
	
	if (PC_status(conf)) {
		throw Error{PDI_ERR_CONFIG, "Invalid configuration"};
	}
	
	PC_tree_t on_event = PC_get(conf, ".on_event");
	int conf_len = len(conf);
	for ( int ii=0; ii<conf_len; ++ii ) {
		string name = to_string(PC_get(conf, "{%d}", ii));
	}
	if ( !PC_status(on_event) ) { // Loading configuration for events
		int nb_events = len(on_event);
		for (int map_id = 0; map_id < nb_events; map_id++) {
			string name = to_string(PC_get(on_event, "{%d}", map_id));
			events_uc.emplace(name, Trigger{PC_get(on_event, "<%d>", map_id)});
		}
	}
	PC_tree_t on_data = PC_get(conf, ".on_data");
	if ( !PC_status(on_data) ) { // Loading configuration for data
		int nb_data = len(on_data);
		for (int map_id = 0; map_id < nb_data; map_id++) {
			string name = to_string(PC_get(on_data, "{%d}", map_id));
			data_uc.emplace(name, Trigger{PC_get(on_data, "<%d>", map_id)});
		}
	}
	
	return status;
}


PDI_status_t PDI_user_code_finalize()
{
	events_uc.clear();
	data_uc.clear();
	return PDI_OK;
}


PDI_status_t PDI_user_code_event(const char *event)
{
	auto&& ucit = events_uc.find(event);
	if ( ucit != events_uc.end() ) {
		// invoke function if required
		ucit->second.call();
	}
	return PDI_OK;
}


PDI_status_t PDI_user_code_data(const std::string &name, PDI::Data_ref)
{
	auto&& ucit = data_uc.find(name);
	if ( ucit != data_uc.end() ) {
		// invoke function if required
		ucit->second.call();
	}
	return PDI_OK;
}

} // namespace <anonymous>

PDI_PLUGIN(user_code)
