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

#include "config.h"

#include <mpi.h>

#include <cstring>
#include <iostream>
#include <unordered_set>
#include <unordered_map>
#include <vector>

#include <dlfcn.h> 	// dynamic loading of function 

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>

#define UC_stderr stderr

/// Verbose level : 0 Error, 1 Warning, 2 Debug
#define UC_verbose 2

#if UC_verbose > 1
#define UC_dbg(...) do{ fprintf(UC_stderr, "[PDI/user_code] Debug: ");\
		fprintf(UC_stderr, __VA_ARGS__);\
		fprintf(UC_stderr, "\n"); \
		fflush(UC_stderr); } while(0);
#else  // Does nothing
#define UC_dbg(...) do{ if(0) printf( __VA_ARGS__) ;} while(0);
#endif

#if UC_verbose > 0
#define UC_warn(...) do { fprintf(UC_stderr, "[PDI/user_code] Warning: ");\
		fprintf(UC_stderr, __VA_ARGS__);\
		fprintf(UC_stderr, "\n"); \
		fflush(UC_stderr);} while(0);
#else  // Does nothing
#define UC_warn(...) do { if(0) printf( __VA_ARGS__); } while(0);
#endif

#define UC_err(...) do {fprintf(UC_stderr, "[PDI/user_code] Error: " );\
		fprintf(UC_stderr, __VA_ARGS__);\
		fprintf(UC_stderr, "\n"); \
		fflush(UC_stderr);} while(0);


namespace {

using std::string;
using std::unordered_set;
using std::unordered_map;
using std::vector;

/// function pointer
typedef void (*ptr_fct_t)(void);

/// Structure for aliases
class UC_alias
{
public:
	UC_alias() = default;
	
	UC_alias(const string& name, const string& var):
			m_name{name},
			m_var{var}
	{}
	
	/** exposes the alias
	 */
	void expose()
	{
		try {
			PDI_state.desc(m_name).share(PDI_state.desc(m_var).ref(), false, false);
		} catch (...) {
			UC_err("Could not alias `%s' as `%s'", m_var.c_str(), m_name.c_str());
			throw;
		}
		UC_dbg("Aliased `%s' as `%s'", m_var.c_str(), m_name.c_str());
	}
	
	/** stop exposing the alias
	 */
	void unexpose()
	{
		PDI_state.desc(m_name).release();
	}
	
private:
	/// name to expose the alias as
	string m_name;
	
	/// variable that is aliased
	string m_var;
	
};


/** Structure to organize the interaction between functions and PDI
 *
 * When a specific event happens, call a function
 */
class UC_trigger
{
public:
	/// parse tree to initialiaze this instance
	UC_trigger(PC_tree_t config)
	{
		string funcname;
		{
			char *c_name = nullptr;
			PC_string(PC_get(config, ".call"), &c_name);
			funcname = c_name;
			free(c_name);
		}
		
		void *fct_uncast = dlsym(NULL, funcname.c_str());
		if (!fct_uncast) {
			UC_err("Unable to load fct `%s': %s", funcname.c_str(), dlerror());
			return;
		}
		// ugly data to function ptr cast to be standard compatible (though undefined behavior)
		m_fct = *((ptr_fct_t *)&fct_uncast);
		UC_dbg("Loaded `%s' => %p", funcname.c_str(), m_fct);
		
		
		PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
		PC_tree_t params = PC_get(config, ".params");
		PC_errhandler(errh);
		if ( !PC_status(params) ) { // parameters
			int len = 0; PC_len(params, &len);
			for (int ii = 0; ii < len; ii++) {
				char *alias_name; PC_string(PC_get(params, "{%d}", ii), &alias_name);
				char *var; PC_string(PC_get(params, "<%d>", ii), &var);
				m_aliases.push_back(UC_alias{alias_name, var});
				free(var);
				free(alias_name);
			}
		}
	}
	
	/// call the function that has been registered
	void call()
	{
		UC_dbg("Calling %p", m_fct);
		for (auto &&alias : m_aliases) {
			alias.expose(); //create alias and share it with the plug-in
		}
#if UC_verbose > 1
		for ( auto&& desc: PDI_state.descriptors() ) {
			UC_dbg(" * `%s'", desc.name().c_str());
		}
#endif
		m_fct();
		for (auto &&alias : m_aliases) {
			alias.unexpose(); //create alias and share it with the plug-in
		}
	}
	
private:
	/// function to call on event
	ptr_fct_t m_fct;
	
	/// all the aliases to setup
	vector<UC_alias> m_aliases;
	
};


/// User-code to call on named events
unordered_map<string, UC_trigger> events_uc;

/// User-code to call on data events
unordered_map<string, UC_trigger> data_uc;


PDI_status_t PDI_user_code_init(PC_tree_t conf, MPI_Comm *)
{
	PDI_status_t status = PDI_OK;
	
	events_uc.clear();
	data_uc.clear();
	
	if (PC_status(conf)) {
		UC_err("Invalid configuration");
		return PDI_ERR_CONFIG;
	}
	
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	PC_tree_t on_event = PC_get(conf, ".on_event");
	PC_errhandler(errh);
	int conf_len=-1; PC_len(conf, &conf_len);
	for ( int ii=0; ii<conf_len; ++ii ) {
		char*name; PC_string(PC_get(conf, "{%d}", ii), &name);
		UC_dbg("Node: %s", name);
	}
	if ( !PC_status(on_event) ) { // Loading configuration for events
		int nb_events = 0; PC_len(on_event, &nb_events);
		for (int map_id = 0; map_id < nb_events; map_id++) {
			char* name; PC_string(PC_get(on_event, "{%d}", map_id), &name);
			events_uc.emplace(name, UC_trigger{PC_get(on_event, "<%d>", map_id)});
			free(name);
		}
	}
	errh = PC_errhandler(PC_NULL_HANDLER);
	PC_tree_t on_data = PC_get(conf, ".on_data");
	PC_errhandler(errh);
	if ( !PC_status(on_data) ) { // Loading configuration for data
		int nb_data = 0; PC_len(on_data, &nb_data);
		for (int map_id = 0; map_id < nb_data; map_id++) {
			char* name; PC_string(PC_get(on_data, "{%d}", map_id), &name);
			data_uc.emplace(name, UC_trigger{PC_get(on_data, "<%d>", map_id)});
			free(name);
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
	UC_dbg("Received event %s", event);
	auto&& ucit = events_uc.find(event);
	if ( ucit != events_uc.end() ) {
		UC_dbg("Calling function for event %s", event);
		// invoke function if required
		ucit->second.call();
	}
	return PDI_OK;
}


PDI_status_t PDI_user_code_data(const std::string &name, PDI::Data_ref)
{
	auto&& ucit = data_uc.find(name);
	if ( ucit != data_uc.end() ) {
		UC_dbg("Calling function for data %s", name.c_str());
		// invoke function if required
		ucit->second.call();
	}
	return PDI_OK;
}

} // namespace <anonymous>

PDI_PLUGIN(user_code)
