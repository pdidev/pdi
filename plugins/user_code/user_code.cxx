/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <dlfcn.h>
#include <link.h>
#include <spdlog/spdlog.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/expression.h>


namespace {

using PDI::Context;
using PDI::Data_descriptor;
using PDI::Ref;
using PDI::each;
using PDI::Error;
using PDI::opt_each;
using PDI::Plugin;
using PDI::Expression;
using PDI::to_string;
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
			each(params, [&](PC_tree_t alias, PC_tree_t var) {
				m_aliases.emplace_back(to_string(alias), to_string(var));
			});
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
			ctx.logger()->error("While calling user code, caught exception: {}", e.what());
		} catch (...) {
			ctx.logger()->error("While calling user code, caught exception");
		}
	}
	
}; // class Trigger

struct user_code_plugin: Plugin {
	user_code_plugin(Context& ctx, PC_tree_t conf):
		Plugin{ctx}
	{
		ctx.logger()->set_pattern("[PDI][User-code][%T] *** %^%l%$: %v");
		
		// Loading configuration for events
		PC_tree_t on_event = PC_get(conf, ".on_event");
		if (!PC_status(on_event)) each(on_event, [&](PC_tree_t event_name, PC_tree_t events) {
			opt_each(events, [&](PC_tree_t one_event) {
				each(one_event, [&](PC_tree_t function_name, PC_tree_t parameters) {
					Trigger event_trigger{to_string(function_name), parameters};
					ctx.add_event_callback([&ctx, event_trigger](const std::string&  name) mutable {
						event_trigger.call(ctx);
					}, to_string(event_name));
				});
			});
		});
		
		// Loading configuration for data
		PC_tree_t on_data = PC_get(conf, ".on_data");
		if (!PC_status(on_data)) each(on_data, [&](PC_tree_t data_name, PC_tree_t datas) {
			opt_each(datas, [&](PC_tree_t one_data) {
				each(one_data, [&](PC_tree_t function_name, PC_tree_t parameters) {
					Trigger data_trigger{to_string(function_name), parameters};
					ctx.add_data_callback([&ctx, data_trigger](const std::string& name, Ref ref) mutable {
						data_trigger.call(ctx);
					}, to_string(data_name));
				});
			});
		});
		
		ctx.logger()->info("Plugin loaded successfully");
	}
	
	~user_code_plugin()
	{
		context().logger()->info("Closing plugin");
	}
	
}; // struct user_code_plugin

} // namespace <anonymous>

PDI_PLUGIN(user_code)
