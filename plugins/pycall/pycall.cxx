// SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <pybind11/embed.h>
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/python/tools.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

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
	/** Python alias for descriptors
	 * \param name alias name for python
	 * \param var expression value for this alias
	 */
	Alias(const string& name, const string& var)
		: m_name{name}
		, m_value{var}
	{}

	/** Exposes the alias
	 * \param ctx the PDI context for this alias
	 * \param pyscope python dictionary
	 */
	void expose(Context& ctx, pydict pyscope)
	{
		Ref r = m_value.to_ref(ctx);
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
	/** Parse tree to initialiaze this instance
	 * \param code python code to call
	 * \param with aliases of descriptors for python code
	 */
	Trigger(string code, PC_tree_t with)
		: m_code{move(code)}
	{
		if (!PC_status(PC_get(with, "{0}"))) { // parameters
			int nwith = len(with);
			for (int ii = 0; ii < nwith; ii++) {
				string alias_name = to_string(PC_get(with, "{%d}", ii));
				string var = to_string(PC_get(with, "<%d>", ii));
				m_aliases.emplace_back(alias_name, var);
			}
		}
	}

	/** Parse tree to initialiaze this instance
	 * \param code python code to call
	 * \param with alias of descriptor for python code
	 */
	Trigger(string code, string with)
		: m_code{move(code)}
	{
		string var = string("$") + with;
		m_aliases.emplace_back(with, var);
	}

	/** Call the function that has been registered
	 * \param ctx the PDI context for this trigger
	 */
	void call(Context& ctx)
	{
		// a python context we fill with exposed variables
		pydict pyscope = pymod::import("__main__").attr("__dict__");
		pyscope["pdi"] = pymod::import("pdi");

		for (auto&& alias: m_aliases) {
			//create alias and share it with the plug-in
			alias.expose(ctx, pyscope);
		}
		try {
			pybind11::exec(m_code, pyscope);
		} catch (const std::exception& e) {
			ctx.logger().error("while calling python, caught exception: {}", e.what());
		} catch (...) {
			ctx.logger().error("while calling python, caught exception");
		}
	}

}; // class Trigger

struct pycall_plugin: Plugin {
	//Determine if python interpreter is initialized by the plugin.
	bool interpreter_initialized_in_plugin = false;

	pycall_plugin(Context& ctx, PC_tree_t conf)
		: Plugin{ctx}
	{
		if (!Py_IsInitialized()) {
			pybind11::initialize_interpreter();
			interpreter_initialized_in_plugin = true;
			ctx.logger().debug("Python interpreter is initialized by the plugin");
		}

		// Loading configuration for events
		PC_tree_t on_event = PC_get(conf, ".on_event");
		int nb_events = len(on_event, 0);
		for (int map_id = 0; map_id < nb_events; map_id++) {
			PC_tree_t event = PC_get(on_event, "<%d>", map_id);
			if (PDI::is_list(event)) {
				size_t len = PDI::len(event);
				std::vector<Trigger> triggers;
				for (int i = 0; i < len; i++) {
					triggers.emplace_back(to_string(PC_get(event, "[%d].exec", i)), PC_get(event, "[%d].with", i));
				}
				ctx.callbacks().add_event_callback(
					[&ctx, triggers](const std::string&) mutable {
						for (auto&& trigger: triggers) {
							trigger.call(ctx);
						}
					},
					to_string(PC_get(on_event, "{%d}", map_id))
				);
			} else {
				Trigger event_trigger{to_string(PC_get(event, ".exec")), PC_get(event, ".with")};
				ctx.callbacks().add_event_callback(
					[&ctx, event_trigger](const std::string&) mutable { event_trigger.call(ctx); },
					to_string(PC_get(on_event, "{%d}", map_id))
				);
			}
		}

		// Loading configuration for data
		PC_tree_t on_data = PC_get(conf, ".on_data");
		int nb_data = len(on_data, 0);
		for (int map_id = 0; map_id < nb_data; map_id++) {
			string data_name = to_string(PC_get(on_data, "{%d}", map_id));
			Trigger data_trigger{to_string(PC_get(on_data, "<%d>", map_id)), data_name};
			ctx.callbacks().add_data_callback([&ctx, data_trigger](const std::string&, Ref) mutable { data_trigger.call(ctx); }, data_name);
		}
	}

	~pycall_plugin()
	{
		if (interpreter_initialized_in_plugin) {
			context().logger().debug("Finalizing python interpreter");
			pybind11::finalize_interpreter();
		}
	}

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "PyCall"; }

}; // struct pycall_plugin

} // namespace

PDI_PLUGIN(pycall)
