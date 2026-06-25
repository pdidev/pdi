/*
# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0
*/

#ifdef CATALYST_IS_PARALLEL
#include <mpi.h>
#endif

#include "catalyst.hpp"

#include <cstdlib> // need to retrive the environement variable
#include <iostream>
#include <stack>
#include <unordered_map>
#include <vector>

#include <pdi/context.h>
#include <pdi/error.h>

#include "pdi_catalyst_plugin.h"

catalyst_plugin::catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
	: Plugin{ctx}
	, m_spec_tree(spec_tree)
	, catalyst_is_initialized{false}
{
	auto communicator_spec = PC_get(m_spec_tree, ".communicator");

	if (PC_status(communicator_spec)) {
		// case no communicator is given:
		//  - communicator is considered to be  MPI_COMM_WORLD
		//	- call catalyst_initialize on_init
		ctx.callbacks().add_init_callback([this]() { this->process_pdi_init(); });
		ctx.callbacks().add_event_callback([this](const std::string& event_name) { this->process_event(event_name); });
	} else {
		// case communicator is given:
		//	- call catalyst_initialize on event given in initialize_on_event

		auto initialize_event_spec = PC_get(m_spec_tree, ".initialize_on_event");
		if (PC_status(initialize_event_spec)) {
			throw PDI::Spectree_error{m_spec_tree, "Catalyst: A communicator is given without specified initialize_on_event"};
		} else {
			m_pdi_initialize_event_name = PDI::to_string(initialize_event_spec);
			m_pdi_execute_event_name = read_pdi_execute_event_name();

			ctx.callbacks().add_event_callback([this](const std::string& event_name) { this->process_event(event_name); });
			ctx.callbacks().add_event_callback([this](const std::string& event_name) { this->process_pdi_init_with_event(event_name); });
		}
	}
}

catalyst_plugin::~catalyst_plugin() noexcept(false)
{
	try {
		run_catalyst_finalize();
	} catch (const std::exception& e) {
		if (std::uncaught_exceptions()) {
			throw;
		} else {
			context().logger().error("When closing catalyst plugin `{}'", e.what());
		}
	} catch (...) {
		if (std::uncaught_exceptions()) {
			throw;
		} else {
			context().logger().error("When closing catalyst plugin");
		}
	}
	context().logger().info("Closing plugin");
}

void catalyst_plugin::process_pdi_init()
{
	this->run_catalyst_initialize();
	this->m_pdi_execute_event_name = this->read_pdi_execute_event_name();
}

// void catalyst_plugin::process_pdi_init_with_communicator(const std::string& event_name)
// {
// 	int world_rank;
// 	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

// 	if (event_name == this->m_pdi_initialize_event_name && !catalyst_is_initialized) {
// 		context().logger().trace("call run_catalyst_initialize in event `{}'", event_name);
// 		this->run_catalyst_initialize();
// 		//this->process_pdi_init();
// 	} else {
// 		this->process_event(event_name);
// 	}

// 	context().logger().debug("I'm arrived to the barrier in event `{}'... rank `{}'", event_name, world_rank);
// 	MPI_Barrier(MPI_COMM_WORLD);
// }

void catalyst_plugin::process_pdi_init_with_event(const std::string& event_name)
{
	if (event_name == this->m_pdi_initialize_event_name && !catalyst_is_initialized) {
		context().logger().trace("call run_catalyst_initialize in event `{}'", event_name);
		this->process_pdi_init();
	}
}

void catalyst_plugin::process_event(const std::string& event_name)
{
	if (event_name == this->m_pdi_execute_event_name) {
		if (catalyst_is_initialized) {
			context().logger().trace("call run_catalyst_execute in event `{}'...", event_name);
			run_catalyst_execute();
		} else {
#ifdef CATALYST_IS_PARALLEL
			if (m_communicator) {
				MPI_Comm tmp_comm = *(static_cast<const MPI_Comm*>(PDI::Ref_r{m_communicator.to_ref(context())}.get()));
				if (tmp_comm != MPI_COMM_NULL) {
					throw PDI::System_error("Try to execute catalyst_execute before catalyst_initialize.");
				} else {
					context().logger().debug("catalyst_execute is not called for this process.");
				}
			} else {
				throw PDI::System_error("Try to execute catalyst_execute before catalyst_initialize.");
			}
#else
			throw PDI::System_error("Try to execute catalyst_execute before catalyst_initialize.");
#endif
		}
	}
}

void catalyst_plugin::run_catalyst_initialize()
{
	conduit_cpp::Node node;

	context().logger().trace("Read information for script.");
	auto scripts_spec = PC_get(this->m_spec_tree, ".scripts");
	if (PC_status(scripts_spec)) {
		throw PDI::Spectree_error(m_spec_tree, "No scripts tree is defiend for catalyst plugin.");
	}

	int script_number = 0;
	PC_len(scripts_spec, &script_number);
	if (script_number == 0) {
		throw PDI::Spectree_error(scripts_spec, "Zero python script is defined for catalyst python.");
	} else {
		context().logger().debug("The number of python script is `{}'", script_number);
	}

	auto scripts_node = node["catalyst/scripts"];
	for (int index = 0; index < script_number; ++index) {
		auto key = PC_get(scripts_spec, "{%d}", index);
		auto value = PC_get(scripts_spec, "<%d>", index);
		scripts_node[PDI::to_string(key)] = PDI::to_string(value);
	}

	// Remark: Each script is defined as a string (i.e. node["catalyst/scripts/[name_of_the_script]"] = filename )
	//
	// We don't consider yet the object script supported by the last version of paraview.
	// In others word, the following node is not supported:
	// node["catalyst/scripts/[name_of_the_script]/filename"] = string
	// node["catalyst/scripts/[name_of_the_script]/args"] = string


	bool process_run_catalayst = true;

#ifdef CATALYST_IS_PARALLEL

	const char* env_catalyst_backend = std::getenv("CATALYST_IMPLEMENTATION_NAME");

	if (env_catalyst_backend == nullptr) {
		context().logger().warn("No CATALYST_IMPLEMENTATION_NAME is given");
		context().logger().warn("The communicator correspond to MPI_COMM_WORD.");
	} else {
		std::string st_env_catalyst_backend = env_catalyst_backend;
		if (st_env_catalyst_backend == "paraview") {
			// define the communicator if it exist

			auto communicator_spec = PC_get(this->m_spec_tree, ".communicator");
			if (!PC_status(communicator_spec)) {
				context().logger().trace("Read communicator");
				m_communicator = PDI::to_string(communicator_spec);
				MPI_Comm tmp_comm = *(static_cast<const MPI_Comm*>(PDI::Ref_r{m_communicator.to_ref(context())}.get()));

				// create communicator node
				auto communicator_node = node["catalyst/mpi_comm"];

				// set the fortran MPI_COMMUNICATOR
				communicator_node.set_int64(static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));

				context().logger().debug("value of the communicator is {}:", static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));

				if (tmp_comm == MPI_COMM_NULL) {
					process_run_catalayst = false;
				}
			} else {
				// context().logger().warn("value of the communicator is {}:", static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));
				//throw PDI::Spectree_error{communicator_spec, "No communicator is given."};
				context().logger().warn("No communicator is given by default the communicator is MPI_COMM_WORD.");
			}
		} else if (st_env_catalyst_backend == "stub") {
			context().logger().warn("The communicator correspond to MPI_COMM_WORD.");
		} else {
			throw PDI::System_error("CATALYST_IMPLEMENTATION_NAME is not recognized: `{}'", env_catalyst_backend);
		}
	}
#else
	context().logger().debug("Catalyst is used with no mpi");
	auto communicator_spec = PC_get(this->m_spec_tree, ".communicator");
	// warning instead of spectree error
	if (!PC_status(communicator_spec)) {
		context().logger().warn("Catalyst is compiled with no mpi support and a communicator is defined.");
	}

#endif

	// The following node is supported in the last version of Paraview
	// These nodes are not defined yet because we need some investigations.
	// node["catalyst_load/implementation"].set("stub") ;
	// node["catalyst_load/search_paths"].set("/path/to/install/catalyst/lib/catalyst/");
	// node["catalyst/pipelines"]
	// node["catalyst/python_path"]

	if (process_run_catalayst) {
		context().logger().debug("catalyst_initialize call...");
		auto result = catalyst_initialize(conduit_cpp::c_node(&node));
		if (result != catalyst_status_ok) {
			context().logger().error("catalyst_initialize failure");
			throw PDI::System_error("catalyst_initialize failure");
		}
		catalyst_is_initialized = true;
	}
}

void catalyst_plugin::read_info_for_creating_vtk_ghost(
	conduit_node* execute_node,
	PC_tree_t& execute_spec,
	std::vector<Catalyst_plugin_structured_ghost>& list_vtkGhostType_to_create
)
{
	// walk the spec tree and create corresponding catalyst nodes.
	struct Spec_tree_node {
		PC_tree_t tree;
		std::string name;
		conduit_node* parent_node;
	};

	// value to keep the parent tree of the current.tree
	PC_tree_t current_parent_tree;


	std::stack<Spec_tree_node> remaining_tree_and_parent_node;
	remaining_tree_and_parent_node.push({execute_spec, "catalyst", execute_node});
	current_parent_tree = execute_spec; // initialize parent tree as execute_spec

	while (!remaining_tree_and_parent_node.empty()) {
		auto current = remaining_tree_and_parent_node.top();
		remaining_tree_and_parent_node.pop();

		context().logger().debug("Read node of name`{}'", current.name);
		if (current.name == "ghost_layers") {
			context().logger().debug("Ghost layer node: `{}'", current.name);

			if (current.tree.node->type == YAML_MAPPING_NODE) {
				int data_tree_size = PDI::len(current.tree);
				if (data_tree_size == 0) {
					throw PDI::Spectree_error(current.tree, "Ghost_layers node defined with a mapping node of size 0.");
				} else {
					context().logger().debug("Number of topologies to consider for ghost layers= `{}'", data_tree_size);
				}

				// loop over the meshes in the ghost layers tree
				for (int index = data_tree_size - 1; index >= 0; --index) {
					list_vtkGhostType_to_create.emplace_back(context(), current.parent_node, current.tree, current_parent_tree, index);
				}
			} else {
				throw PDI::Spectree_error(current.tree, "Ghost_layers node only support yaml mapping node.");
			}
		} else {
			if (current.tree.node->type == YAML_MAPPING_NODE) {
				int data_tree_size = PDI::len(current.tree);
				std::list<std::string> name_to_skip{"coordsets", "topologies", "fields", "matsets", "adjsets", "state"};

				// reverse order to get the correct order when poping the stack.
				for (int index = data_tree_size - 1; index >= 0; --index) {
					auto key = PC_get(current.tree, "{%d}", index);
					std::string keyname = PDI::to_string(key);

					bool skip_key = false;
					for (auto&& elem: name_to_skip) {
						if (elem == keyname) {
							skip_key = true;
							break;
						}
					}
					if (!skip_key) {
						auto value = PC_get(current.tree, "<%d>", index);
						if (conduit_cpp::cpp_node(current.parent_node).has_path(current.name)) {
							auto current_node
								= conduit_cpp::cpp_node(current.parent_node)[current.name]; // Attention: creation of the node if doesn't exit
							remaining_tree_and_parent_node.push({value, PDI::to_string(key), conduit_cpp::c_node(&current_node)});

							current_parent_tree = current.tree;

						} else {
							throw PDI::System_error("Error in creating vtkGhostType: a conduit node doesn't exist !!");
						}
					}
				}
			}
		}
	}
}

void catalyst_plugin::create_catalyst_execute_conduit_node(conduit_node* execute_node, PC_tree_t& execute_spec)
{
	// walk the spec tree and create corresponding catalyst nodes.
	struct Spec_tree_node {
		PC_tree_t tree;
		std::string name;
		conduit_node* parent_node;
	};

	std::stack<Spec_tree_node> remaining_tree_and_parent_node;
	remaining_tree_and_parent_node.push({execute_spec, "catalyst", execute_node});
	while (!remaining_tree_and_parent_node.empty()) {
		auto current = remaining_tree_and_parent_node.top();
		remaining_tree_and_parent_node.pop();
		context().logger().debug("Read node of name `{}'", current.name);
		if (current.name == "ghost_layers") {
			context().logger().debug(" ghost_layers keys will be read after");
		} else {
			auto current_node = conduit_cpp::cpp_node(current.parent_node)[current.name];
			switch (current.tree.node->type) {
			case YAML_NO_NODE:
				throw PDI::Spectree_error{current.tree, "Unsupported Empty YAML Node for variable `{}'", current.name};
				// context().logger().error("Unsupported Empty YAML Node for variable `{}'", current.name);
				break;
			case YAML_SCALAR_NODE:
				switch (current.tree.node->data.scalar.style) {
				case YAML_PLAIN_SCALAR_STYLE:
				case YAML_SINGLE_QUOTED_SCALAR_STYLE:
				case YAML_DOUBLE_QUOTED_SCALAR_STYLE: {
					// handle integer or float/double type that depend perhaps on scalar PDI data
					std::string data_name{PDI::to_string(current.tree)};
					context().logger().debug("Read value of an integer or float or string for data=`{}'", data_name);
					PDI::Expression data_expression{PDI::to_string(current.tree)};

					PDI::Ref_r spec_ref = data_expression.to_ref(context());
					if (!spec_ref) {
						context().logger().debug("problem !spec_ref");
						throw PDI::Value_error{"Error of right access for: `{}'", data_name};
					}
					auto data_type = spec_ref.type()->evaluate(context());
					if (!data_type) {
						context().logger().debug("problem !data_type");
						throw PDI::Spectree_error{current.tree, "Error of right access for: `{}'", data_name};
					}
					if (auto&& scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type)) {
						context().logger().debug("case scalar type `{}'", data_name);
						set_value_for_pdi_scalar_datatype(conduit_cpp::c_node(&current_node), current.tree, data_name, *scalar_datatype, spec_ref);
					} else if (auto&& array_datatype = std::dynamic_pointer_cast<const PDI::Array_datatype>(data_type)) {
						context().logger().debug("case array type `{}'", data_name);

						// check the array_datatype is a string
						PDI::Datatype_sptr type = array_datatype->subtype();
						// case multi dimensional array ??
						while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
							type = array_type->subtype();
						}
						auto array_scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type);
						if (!array_scalar_datatype) {
							// context().logger().error("Array subtype of variable {} should be scalar type.", name);
							throw PDI::Spectree_error{current.tree, "Array subtype of variable `{}' should be scalar type.", current.name};
						}
						PDI::Scalar_kind scalar_kind = array_scalar_datatype->kind();
						if (scalar_kind == PDI::Scalar_kind::SIGNED && array_scalar_datatype->buffersize() == sizeof(char)) {
							context().logger().debug("scalar kind of the array is signed");
							current_node.set_string(PDI::to_string(current.tree));
						} else if (scalar_kind == PDI::Scalar_kind::UNSIGNED && array_scalar_datatype->buffersize() == sizeof(unsigned char)) {
							context().logger().debug("scalar_kind of the array is unsigned");
							current_node.set_string(PDI::to_string(current.tree));
						} else {
							throw PDI::Spectree_error{current.tree, "The scalar type must be a string for `{}'.", current.name};
						}
					} else {
						throw PDI::Spectree_error{current.tree, "The scalar type must be a string for `{}'.", current.name};
					}
				} break;
				case YAML_LITERAL_SCALAR_STYLE:
				case YAML_FOLDED_SCALAR_STYLE:
				case YAML_ANY_SCALAR_STYLE:
					throw PDI::Spectree_error{current.tree, "Unsupported YAML scalar style for variable `{}'", current.name};
					break;
				}
				break;
			case YAML_SEQUENCE_NODE:
				throw PDI::Spectree_error{current.tree, "Unsupported Sequence YAML Node for variable `{}'", current.name};
				break;
			case YAML_MAPPING_NODE:
				int data_tree_size = PDI::len(current.tree);
				// Check for dynamic PDI Data array
				bool pdi_data_array = false;
				for (int index = data_tree_size - 1; index >= 0; --index) {
					auto key = PC_get(current.tree, "{%d}", index);
					if (PDI::to_string(key) == "PDI_data_array") {
						this->fill_node_with_pdi_data_array(conduit_cpp::c_node(&current_node), current.tree);
						pdi_data_array = true;
						break; // break the loop
					}
				}
				if (pdi_data_array) {
					break; // break the case
				}
				// reverse order to get the correct order when poping the stack.
				for (int index = data_tree_size - 1; index >= 0; --index) {
					auto key = PC_get(current.tree, "{%d}", index);
					auto value = PC_get(current.tree, "<%d>", index);
					remaining_tree_and_parent_node.push({value, PDI::to_string(key), conduit_cpp::c_node(&current_node)});
				}
			}
		}
	}
}

void catalyst_plugin::run_catalyst_execute()
{
	assert(catalyst_is_initialized == true);

	context().logger().trace("Run catalyst_execute()");
	conduit_cpp::Node node;
	std::vector<Catalyst_plugin_structured_ghost> list_vtkGhostType_to_create; // object contain vector vtkGhostType
	auto execute_spec = PC_get(this->m_spec_tree, ".execute");

	context().logger().debug("Read m_spec_tree execute");
	conduit_node* node_pointer = conduit_cpp::c_node(&node);
	// create the conduit node for catalyst_execute;
	create_catalyst_execute_conduit_node(node_pointer, execute_spec);

	context().logger().debug("Read Ghost layers for creating vtk_ghost_type");
	// read information to create the vtkGhostType for paraview (read "ghost_layers" node in the yaml file)
	//create_node_for_mask_ghost( node_pointer, execute_spec, list_vtkGhostType_to_create);
	read_info_for_creating_vtk_ghost(node_pointer, execute_spec, list_vtkGhostType_to_create);

	// creation vtkGhostType vector
	for (auto&& vtk_ghost_type: list_vtkGhostType_to_create) {
		std::string tmp_path = vtk_ghost_type.get_node_path();
		std::string meshname = vtk_ghost_type.get_topology_name();
		vtk_ghost_type.create_vtk_ghost_type();

		// create the node corresponding to the different vtkGhostType
		node[tmp_path + "/fields/vtkGhostType/association"].set("element");
		node[tmp_path + "/fields/vtkGhostType/topology"].set(meshname);
		node[tmp_path + "/fields/vtkGhostType/volume_dependent"].set("false");
		node[tmp_path + "/fields/vtkGhostType/values"].set_external(vtk_ghost_type.get_vector(), vtk_ghost_type.get_size(), 0, 1, sizeof(uint8_t));
	}

	if (context().logger().level() == spdlog::level::debug || context().logger().level() == spdlog::level::trace) {
		context().logger().info("Print conduit node including vtk ghost type created ...");
		node.print();
	}

	context().logger().trace("catalyst_execute call...");
	auto result = catalyst_execute(node_pointer);
	if (result != catalyst_status_ok) {
		throw PDI::System_error{"catalyst_execute failure"};
	}
	context().logger().trace("end catalyst_execute call...");
}

void catalyst_plugin::run_catalyst_finalize()
{
	if (catalyst_is_initialized) {
		context().logger().debug("catalyst_finalize call...");
		conduit_cpp::Node node;
		auto result = catalyst_finalize(conduit_cpp::c_node(&node));
		if (result != catalyst_status_ok) {
			// context().logger().error("catalyst_finalize failure");
			throw PDI::System_error{"catalyst_finalize failure"};
		}
	}
}

void catalyst_plugin::fill_node_with_pdi_data_array(conduit_node* node, PC_tree_t& tree)
{
	// check the function is called with a PC_tree containg
	auto name_spec = PC_get(tree, ".PDI_data_array");
	if (PC_status(name_spec)) {
		throw PDI::Spectree_error{tree, "No \"name\" child in PDI_data_array spec."};
	}

	std::string name = PDI::to_string(name_spec);
	PDI::Ref_r ref_r = context()[name].ref();
	// check the data can be read from PDI
	if (!ref_r) {
		context().logger().warn("Cannot read `{}' this data is not available", name);
		// Remark: This error can arrive outside PDI_initilialize. This implies that is not really a config error
		throw PDI::System_error{"No \"name\" child in PDI_data_array spec `{}'.", name};
	}

	auto data_type = ref_r.type();
	if (auto array_datatype = std::dynamic_pointer_cast<const PDI::Array_datatype>(data_type)) {
		set_value_for_pdi_array_datatype(node, name, tree, *array_datatype, ref_r);
	} else {
		// Remark: This error can arrive outside PDI_initilialize. This implies that is not really a config error
		throw PDI::System_error{"Unsupported datatype for variable: `{}'. The type should be array type.", name};
	}
}

void catalyst_plugin::set_value_for_pdi_scalar_datatype(
	conduit_node* node,
	PC_tree_t& tree,
	const std::string& name,
	const PDI::Scalar_datatype& scalar_datatype,
	PDI::Ref_r& ref_r
)
{
	// remark: the different type of conduit integer and float is defined in the configuration step of cmake.
	PDI::Scalar_kind scalar_kind = scalar_datatype.kind();
	if (scalar_kind == PDI::Scalar_kind::SIGNED) {
		auto buffer_size = scalar_datatype.buffersize();
		if (buffer_size == sizeof(conduit_int8)) {
			catalyst_conduit_node_set_int8(node, *static_cast<const conduit_int8*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_int16)) {
			catalyst_conduit_node_set_int16(node, *static_cast<const conduit_int16*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_int32)) {
			catalyst_conduit_node_set_int32(node, *static_cast<const conduit_int32*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_int64)) {
			catalyst_conduit_node_set_int64(node, *static_cast<const conduit_int64*>(ref_r.get()));
		} else {
			throw PDI::Spectree_error{tree, "Unknown SIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else if (scalar_kind == PDI::Scalar_kind::UNSIGNED) {
		auto buffer_size = scalar_datatype.buffersize();
		if (buffer_size == sizeof(conduit_uint8)) {
			catalyst_conduit_node_set_uint8(node, *static_cast<const conduit_uint8*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_uint16)) {
			catalyst_conduit_node_set_uint16(node, *static_cast<const conduit_uint16*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_uint32)) {
			catalyst_conduit_node_set_uint32(node, *static_cast<const conduit_uint32*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_uint64)) {
			catalyst_conduit_node_set_uint64(node, *static_cast<const conduit_uint64*>(ref_r.get()));
		} else {
			throw PDI::Spectree_error{tree, "Unknown UNSIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else if (scalar_kind == PDI::Scalar_kind::FLOAT) {
		auto buffer_size = scalar_datatype.buffersize();
		if (buffer_size == sizeof(conduit_float32)) {
			catalyst_conduit_node_set_float32(node, *static_cast<const conduit_float32*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_float64)) {
			catalyst_conduit_node_set_float64(node, *static_cast<const conduit_float64*>(ref_r.get()));
		} else {
			throw PDI::Spectree_error{tree, "Unknown FLOAT buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else {
		throw PDI::Spectree_error{tree, "Unknown Scalar Type for variable `{}'", name};
	}
}

void catalyst_plugin::get_conduit_index_t_value(PC_tree_t& spec, const std::string& name, conduit_index_t& value)
{
	if (!PC_status(spec)) {
		long tmp_value;
		if (spec.node->type == YAML_SCALAR_NODE) {
			PDI::Expression data_expression{PDI::to_string(spec)};
			PDI::Ref_r spec_ref = data_expression.to_ref(context());
			if (!spec_ref) {
				throw PDI::System_error("The PDIData named \"{}\" is not readable.", name);
			}
			auto data_type = spec_ref.type()->evaluate(context());
			if (auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type)) {
				PDI::Scalar_kind scalar_kind = (*scalar_datatype).kind();
				if (scalar_kind == PDI::Scalar_kind::SIGNED || scalar_kind == PDI::Scalar_kind::UNSIGNED) {
					// return spec_ref.scalar_value<long>();
					tmp_value = data_expression.to_long(context());
				} else {
					throw PDI::Spectree_error{
						spec,
						"Unknown Scalar Type for variable `{}'. The type must be an integer signed or unsigned)",
						PDI::to_string(spec)
					};
				}
			} else {
				throw PDI::Spectree_error{spec, "The datatype must be a scalar datatype for variable: `{}'", PDI::to_string(spec)};
			}
		} else {
			throw PDI::Spectree_error{spec, "Supported only YAML_SCALAR_NODE for variable `{}'", name};
		}

		// return value in conduit_index_t
		if (std::is_same<conduit_index_t, long>::value) {
			value = tmp_value;
		} else {
			// case conduit_index_t is 32-bits
			value = static_cast<conduit_index_t>(tmp_value);
			if (value != tmp_value) {
				throw PDI::System_error{"Error in cast of a type conduit_index_t in long. `{}' != `{}'", value, tmp_value};
			}
		}
	}
}

void catalyst_plugin::set_value_for_pdi_array_datatype(
	conduit_node* node,
	const std::string& name,
	PC_tree_t& tree,
	const PDI::Array_datatype& array_datatype,
	PDI::Ref_r& ref_r
)
{
	PDI::Datatype_sptr type = array_datatype.subtype();
	while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
		type = array_type->subtype();
	}
	auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type);
	if (!scalar_datatype) {
		throw PDI::Spectree_error{tree, "Array subtype of variable `{}' should be scalar type.", name};
		return;
	}

	conduit_index_t num_elements = 0;
	auto size_spec = PC_get(tree, ".size");
	get_conduit_index_t_value(size_spec, name, num_elements);

	if (num_elements == 0) {
		throw PDI::System_error("Unknown the size of an array of name `{}' passed to catalyst.", name);
	}

	conduit_index_t offset = 0;
	auto offset_spec = PC_get(tree, ".offset");
	get_conduit_index_t_value(offset_spec, name, offset);

	conduit_index_t stride = 1;
	auto stride_spec = PC_get(tree, ".stride");
	get_conduit_index_t_value(stride_spec, name, stride);

	// computer endianness is used
	conduit_index_t endianness = CONDUIT_ENDIANNESS_DEFAULT_ID;

	PDI::Scalar_kind scalar_kind = scalar_datatype->kind();
	if (scalar_kind == PDI::Scalar_kind::SIGNED) {
		auto buffer_size = scalar_datatype->buffersize();
		if (buffer_size == sizeof(conduit_int8)) {
			conduit_index_t element_bytes = 1;
			auto pointer = const_cast<conduit_int8*>(static_cast<const conduit_int8*>(ref_r.get()));
			catalyst_conduit_node_set_external_int8_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_int16)) {
			conduit_index_t element_bytes = 2;
			auto pointer = const_cast<conduit_int16*>(static_cast<const conduit_int16*>(ref_r.get()));
			catalyst_conduit_node_set_external_int16_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_int32)) {
			conduit_index_t element_bytes = 4;
			auto pointer = const_cast<conduit_int32*>(static_cast<const conduit_int32*>(ref_r.get()));
			catalyst_conduit_node_set_external_int32_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_int64)) {
			conduit_index_t element_bytes = 8;
			auto pointer = const_cast<conduit_int64*>(static_cast<const conduit_int64*>(ref_r.get()));
			catalyst_conduit_node_set_external_int64_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else {
			throw PDI::System_error{"Unknown SIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else if (scalar_kind == PDI::Scalar_kind::UNSIGNED) {
		auto buffer_size = scalar_datatype->buffersize();
		if (buffer_size == sizeof(conduit_uint8)) {
			conduit_index_t element_bytes = 1;
			auto pointer = const_cast<conduit_uint8*>(static_cast<const conduit_uint8*>(ref_r.get()));
			catalyst_conduit_node_set_external_uint8_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_uint16)) {
			conduit_index_t element_bytes = 2;
			auto pointer = const_cast<conduit_uint16*>(static_cast<const conduit_uint16*>(ref_r.get()));
			catalyst_conduit_node_set_external_uint16_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_uint32)) {
			conduit_index_t element_bytes = 4;
			auto pointer = const_cast<conduit_uint32*>(static_cast<const conduit_uint32*>(ref_r.get()));
			catalyst_conduit_node_set_external_uint32_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_uint64)) {
			conduit_index_t element_bytes = 8;
			auto pointer = const_cast<conduit_uint64*>(static_cast<const conduit_uint64*>(ref_r.get()));
			catalyst_conduit_node_set_external_uint64_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else {
			throw PDI::System_error{"Unknown UNSIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else if (scalar_kind == PDI::Scalar_kind::FLOAT) {
		auto buffer_size = scalar_datatype->buffersize();
		if (buffer_size == sizeof(conduit_float32)) {
			conduit_index_t element_bytes = 4;
			auto pointer = const_cast<conduit_float32*>(static_cast<const conduit_float32*>(ref_r.get()));
			catalyst_conduit_node_set_external_float32_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else if (buffer_size == sizeof(conduit_float64)) {
			conduit_index_t element_bytes = 8;
			auto pointer = const_cast<conduit_float64*>(static_cast<const conduit_float64*>(ref_r.get()));
			catalyst_conduit_node_set_external_float64_ptr_detailed(
				node,
				pointer,
				num_elements,
				offset * element_bytes,
				stride * element_bytes,
				element_bytes,
				endianness
			);
		} else {
			throw PDI::System_error{"Unknown FLOAT buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else {
		throw PDI::System_error{"Unknown Scalar Type for variable `{}'", name};
	}
}

std::string catalyst_plugin::read_pdi_execute_event_name()
{
	std::string event_name;
	auto execute_spec = PC_get(this->m_spec_tree, ".on_event");
	if (PC_status(execute_spec) == PC_OK) {
		event_name = PDI::to_string(execute_spec);
	} else {
		throw PDI::Spectree_error{execute_spec, "No event name for catalyst plugin is given `{}'.", event_name};
	}
	context().logger().trace("catalyst_execute will be call in event `{}'.", event_name);
	return event_name;
}
