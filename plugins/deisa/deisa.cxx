/******************************************************************************
 * Copyright (c) 2020-2025 Centre national de la recherche scientifique (CNRS)
 * Copyright (c) 2020-2025 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
 * Copyright (c) 2020-2023 Institut national de recherche en informatique et en automatique (Inria)
 * Copyright (c) 2020-2025 Université Paris-Saclay
 * Copyright (c) 2020-2025 Université de Versailles Saint-Quentin-en-Yvelines
 *
 * SPDX-License-Identifier: MIT
 *
 *****************************************************************************/

#include <memory>
#include <memory_resource>
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
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/python/tools.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

namespace {

namespace py = pybind11;

using namespace PDI;
using namespace py::literals;

/** The deisa plugin
 */
class deisa_plugin: public Plugin
{
	static constexpr char DEISA_COMPATIBLE_VERSION[] = "0.3.2"; // Used to check compatibility with Deisa's python library

	Expression m_scheduler_info;
	std::unordered_map<std::string, Datatype_template_sptr> m_deisa_arrays;
	Expression m_time_step;
	py::object m_bridge = py::none();

public:
	static std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> dependencies() { return {{"mpi"}, {"mpi"}}; }

	deisa_plugin(Context& ctx, PC_tree_t conf)
		: Plugin{ctx}
	{
		if (!Py_IsInitialized()) {
			py::initialize_interpreter();
		}
		assert(Py_IsInitialized());

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

		// plugin init
		PC_tree_t init_tree = PC_get(conf, ".init_on");
		if (!PC_status(init_tree)) {
			ctx.callbacks().add_event_callback(
				[&](const std::string&) { m_bridge = on_init_event(ctx, m_scheduler_info, m_deisa_arrays); },
				to_string(init_tree)
			);
		} else {
			throw Config_error{conf, "Deisa plugin requires init_on key "};
		} // TODO: replace with try/catch when #480 is fixed.

		// map_in
		PC_tree_t map_in = PC_get(conf, ".map_in");
		if (!PC_status(map_in)) {
			each(map_in, [&](PC_tree_t key_map, PC_tree_t value_map) { // TODO: when #481 is fixed, use `or_none` variant.
				ctx.callbacks().add_data_callback(
					[&, deisa_array_name = to_string(value_map)](const std::string&, const Ref& data_ref) { on_data(deisa_array_name, data_ref); },
					to_string(key_map)
				);
			});
		}
	}

	~deisa_plugin() noexcept
	{
		context().logger().info("Closing plugin");
		try {
			// call bridge release() so that we can clear things before destructor is called (if it is ever called !).
			assert(m_bridge != py::none());
			assert(hasattr(m_bridge, "release"));
			m_bridge.attr("release")();
			m_bridge = py::none();
		} catch (const std::exception& e) {
			context().logger().error("Exception in destructor, caught exception {}", e.what());
		} catch (...) {
			context().logger().error("Exception in destructor");
		}
	}

private:
	void on_data(const std::string& deisa_array_name, const Ref& data_ref)
	{
		try {
			assert(m_bridge != py::none());
			assert(hasattr(m_bridge, "publish_data"));
			py::object publish_data = m_bridge.attr("publish_data");
#ifdef NDEBUG
			publish_data(to_python(data_ref), deisa_array_name.c_str(), m_time_step.to_long(context()), "debug"_a = false);
#else
			publish_data(to_python(data_ref), deisa_array_name.c_str(), m_time_step.to_long(context()), "debug"_a = true);
#endif
		} catch (const std::exception& e) {
			throw Plugin_error("While publishing data. Caught exception: {}", std::string(e.what()));
		} catch (...) {
			throw Plugin_error("While publishing data.");
		}
	}

	/**
	 * Check that the PDI plugin is compatible with Deisa's python Bridge (i.e, that the python API is what it should be).
	 * The plugin is compatible if DEISA_COMPATIBLE_VERSION == deisa.__version__.__version__ (python)
	 */
	static void check_compatibility()
	{
		py::module deisa = py::module::import("deisa.__version__");
		const auto python_library_version = py::str(deisa.attr("__version__")).cast<std::string>();
		if (python_library_version != DEISA_COMPATIBLE_VERSION) {
			throw Plugin_error(
				"Deisa PDI plugin is expecting Deisa python version {} but found version {}",
				DEISA_COMPATIBLE_VERSION,
				python_library_version
			);
		}
	}

	static py::object
	on_init_event(Context& ctx, const Expression& scheduler_info, const std::unordered_map<std::string, Datatype_template_sptr>& deisa_arrays)
	{
		std::unordered_map<std::string, std::unordered_map<std::string, std::vector<size_t>>> darrs;
		std::unordered_map<std::string, py::dtype> darrs_dtype;
		for (auto&& [deisa_array_name, type_tpl]: deisa_arrays) {
			std::unordered_map<std::string, std::vector<size_t>> darr;
			std::vector<size_t> sizes;
			std::vector<size_t> starts;
			std::vector<size_t> subsizes;
			std::vector<size_t> timedim;

			Datatype_sptr type_sptr = type_tpl->evaluate(ctx);
			timedim.emplace_back(type_tpl->attribute("timedim").to_long(ctx));
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
			darrs_dtype[deisa_array_name] = to_python(std::dynamic_pointer_cast<const Scalar_datatype>(type_sptr));
		}

		try {
			int mpi_size;
			long rank = Ref_r{ctx.desc("MPI_COMM_WORLD_rank").ref()}.scalar_value<long>();
			MPI_Comm comm = *static_cast<const MPI_Comm*>(Ref_r{ctx.desc("MPI_COMM_WORLD").ref()}.get());
			MPI_Comm_size(comm, &mpi_size);
			long size = static_cast<long>(mpi_size);

			// instantiate bridge
			py::module deisa = py::module::import("deisa");
			py::object get_bridge_instance = deisa.attr("get_bridge_instance");

			check_compatibility();

			// TODO: use_ucx
			return get_bridge_instance(to_python(scheduler_info.to_ref(ctx)), rank, size, darrs, darrs_dtype);
		} catch (const std::exception& e) {
			throw Plugin_error("Could not initialize Deisa plugin. Caught exception: {}", e.what());
		} catch (...) {
			throw Plugin_error("Could not initialize Deisa plugin. Unknown exception.");
		}
	}

}; // class deisa_plugin

} // namespace


PDI_PLUGIN(deisa)
