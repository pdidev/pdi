/******************************************************************************
 * Copyright (c) 2020-2024 Centre national de la recherche scientifique (CNRS)
 * Copyright (c) 2020-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
 * Copyright (c) 2020-2023 Institut national de recherche en informatique et en automatique (Inria)
 * Copyright (c) 2020-2024 Université Paris-Saclay
 * Copyright (c) 2020-2024 Université de Versailles Saint-Quentin-en-Yvelines
 *
 * SPDX-License-Identifier: MIT
 *
 *****************************************************************************/

#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <pybind11/embed.h>
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <mpi.h>
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

namespace py = pybind11;

using namespace PDI;
using namespace py::literals;
using pydict = py::dict;
using pymod = py::module;
using pyobj = py::object;

/** The deisa plugin
 */
class deisa_plugin: public Plugin
{
	static constexpr char PYTHON_LIBRARY_COMPATIBILITY[] = "0.3.2"; // Used to check compatibility with Deisa's python library

	bool m_interpreter_initialized_in_plugin = false; // Determine if python interpreter is initialized by the plugin
	Expression m_scheduler_info;
	std::unordered_map<std::string, Datatype_template_sptr> m_deisa_arrays;
	Expression m_rank;
	Expression m_size;
	Expression m_time_step;

public:
	static std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> dependencies() { return {{"mpi"}, {"mpi"}}; }

	deisa_plugin(Context& ctx, PC_tree_t conf)
		: Plugin{ctx}
	{
		if (!Py_IsInitialized()) {
			py::initialize_interpreter();
			py::exec("bridge = None"); // needed because check in dtor.
			m_interpreter_initialized_in_plugin = true;
		}
		check_compatibility();
		// init params
		each(conf, [&](PC_tree_t key_tree, PC_tree_t value) {
			std::string key = to_string(key_tree);
			if (key == "scheduler_info") {
				m_scheduler_info = to_string(value);
			} else if (key == "deisa_arrays") {
				each(value, [&](PC_tree_t key_map, PC_tree_t value_map) { m_deisa_arrays.emplace(to_string(key_map), ctx.datatype(value_map)); });
			} else if (key == "time_step") {
				m_time_step = to_string(value);
			} else if (key == "map_in") {
				//
			} else if (key == "logging" || key == "init_on") {
				//
			} else {
				throw Config_error{key_tree, "Unknown key in Deisa file configuration: '{}'", key};
			}
		});


		int mpi_size;
		m_rank = Expression{Ref_r{ctx.desc("MPI_COMM_WORLD_rank").ref()}.scalar_value<long>()};
		MPI_Comm comm = *static_cast<const MPI_Comm*>(Ref_r{ctx.desc("MPI_COMM_WORLD").ref()}.get());
		MPI_Comm_size(comm, &mpi_size);
		m_size = Expression{static_cast<long>(mpi_size)};


		// plugin init
		PC_tree_t init_tree = PC_get(conf, ".init_on");
		if (!PC_status(init_tree)) {
			ctx.callbacks().add_event_callback([&](const std::string&) { init_deisa(); }, to_string(init_tree));
		} else {
			throw Config_error{conf, "Deisa plugin requires init_on key "};
		}

		// map_in
		PC_tree_t map_tree = PC_get(conf, ".map_in");
		if (!PC_status(map_tree)) {
			each(map_tree, [&](PC_tree_t key_map, PC_tree_t value_map) {
				ctx.callbacks().add_data_callback(
					[&, deisa_array_name = to_string(value_map)](const std::string&, const Ref& data_ref) {
						try {
							// start a python context and call bridge.publish_data(...)
							pydict pyscope = pymod::import("__main__").attr("__dict__");
							pyscope[deisa_array_name.c_str()] = to_python(data_ref);
							pyscope["time_step"] = m_time_step.to_long(ctx);
							pyscope["name"] = deisa_array_name.c_str();

#ifdef NDEBUG
							py::exec("bridge.publish_data(" + deisa_array_name + ", name, time_step, debug=False)", pyscope);
#else
							py::exec("bridge.publish_data(" + deisa_array_name + ", name, time_step, debug=True)", pyscope);
#endif
							pyscope[deisa_array_name.c_str()] = NULL;
						} catch (const std::exception& e) {
							std::cerr << " *** [PDI/Deisa] Error: while publishing data, caught exception: " << e.what() << std::endl;
						} catch (...) {
							std::cerr << " *** [PDI/Deisa] Error: while publishing data." << std::endl;
						}
					},
					to_string(key_map)
				);
			});
		}
	}

	~deisa_plugin() noexcept override
	{
		try {
			if (m_interpreter_initialized_in_plugin) {
				py::exec(R"(
				if bridge:
				  bridge.release()
                		)"); // call bridge release() so that we can clear things before destructor is called (if it is ever called !).
				py::finalize_interpreter();
			}
		} catch (const std::exception& e) {
			std::cerr << " *** [PDI/Deisa] Error in destructor, caught exception: " << e.what() << std::endl;
		} catch (...) {
			std::cerr << " *** [PDI/Deisa] Error in destructor. " << std::endl;
		}
	}

private:
	/**
	 * Check that the PDI plugin is compatible with Deisa's python Bridge (i.e, that the python API is what it should be).
	 * The plugin is compatible if PYTHON_LIBRARY_COMPATIBILITY == deisa.__version__.__version__ (python)
	 */
	static void check_compatibility()
	{
		pymod deisa = pymod::import("deisa.__version__");
		const auto python_library_version = py::str(deisa.attr("__version__")).cast<std::string>();
		if (python_library_version != PYTHON_LIBRARY_COMPATIBILITY) {
			throw Plugin_error(
				"Deisa PDI plugin is expecting Deisa python version " + std::string(PYTHON_LIBRARY_COMPATIBILITY) + " but found version "
				+ python_library_version
			);
		}
	}

	void init_deisa()
	{
		std::unordered_map<std::string, std::unordered_map<std::string, std::vector<size_t>>> darrs;
		std::unordered_map<std::string, py::dtype> darrs_dtype;
		for (auto&& key_value: m_deisa_arrays) {
			std::unordered_map<std::string, std::vector<size_t>> darr;
			std::vector<size_t> sizes;
			std::vector<size_t> starts;
			std::vector<size_t> subsizes;
			std::vector<size_t> timedim;
			std::string deisa_array_name = key_value.first;
			Datatype_sptr type_sptr = key_value.second->evaluate(context());
			timedim.emplace_back(key_value.second->attribute("timedim").to_long(context()));
			// get info from datatype
			while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type_sptr)) {
				sizes.emplace_back(array_type->size());
				starts.emplace_back(array_type->start());
				subsizes.emplace_back(array_type->subsize());
				type_sptr = array_type->subtype();
			}
			darr["sizes"] = sizes;
			darr["starts"] = starts;
			darr["subsizes"] = subsizes;
			darr["timedim"] = timedim;
			darrs[deisa_array_name] = darr;
			darrs_dtype[deisa_array_name] = datatype_to_pydtype(std::dynamic_pointer_cast<const Scalar_datatype>(type_sptr));
		}

		try {
			// setup python context and instantiate bridge
			pydict pyscope = pymod::import("__main__").attr("__dict__");
			pyscope["deisa"] = pymod::import("deisa");
			pymod deisa = pymod::import("deisa");
			pyscope["get_bridge_instance"] = deisa.attr("get_bridge_instance");
			pyscope["scheduler_info"] = to_python(m_scheduler_info.to_ref(context()));
			pyscope["rank"] = to_python(m_rank.to_ref(context()));
			pyscope["size"] = to_python(m_size.to_ref(context()));
			pyscope["deisa_arrays"] = darrs;
			pyscope["deisa_arrays_dtype"] = darrs_dtype;

			// TODO: use_ucx
			py::exec("bridge = get_bridge_instance(scheduler_info, rank, size, deisa_arrays, deisa_arrays_dtype)", pyscope);
		} catch (const std::exception& e) {
			std::cerr << " *** [PDI/Deisa] Error: while initializing deisa, caught exception: " << e.what() << std::endl;
			throw Plugin_error("Could not initialize Deisa plugin. Caught exception: " + std::string(e.what()));
		} catch (...) {
			std::cerr << " *** [PDI/Deisa] Error: while initializing deisa" << std::endl;
			throw Plugin_error("Could not initialize Deisa plugin. Unknown exception.");
		}
	}

}; // class deisa_plugin

} // namespace


PDI_PLUGIN(deisa)
