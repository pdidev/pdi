/*
# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0
*/

#ifdef CATALYST_IS_PARALLEL
#include <mpi.h>
#endif

#include "catalyst.hpp"

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
	, catalyst_is_initialize{false}
{
	ctx.callbacks().add_init_callback([this]() { this->process_pdi_init(); });
	//	ctx.callbacks().add_data_callback([this](const std::string& data_name, PDI::Ref ref) { this->process_data(data_name, ref); });
	ctx.callbacks().add_event_callback([this](const std::string& event_name) { this->process_event(event_name); });
}

catalyst_plugin::~catalyst_plugin()
{
	run_catalyst_finalize();
}

void catalyst_plugin::process_pdi_init()
{
	this->run_catalyst_initialize();
	this->m_pdi_execute_event_name = this->read_pdi_execute_event_name();
}

// void catalyst_plugin::process_data(const std::string& data_name, PDI::Ref ref)
// {
// 	context().logger().debug("User has shared a data named `{}'", data_name);
// 	auto it = this->m_current_pdi_data.find(data_name);
// 	if (it != this->m_current_pdi_data.end()) {
// 		context().logger().warn("Data named '{}' already recorded, the previous value will overwritten.", data_name);
// 		it->second = ref.copy();
// 	} else {
// 		this->m_current_pdi_data.emplace(data_name, ref);
// 	}
// }

void catalyst_plugin::process_event(const std::string& event_name)
{
	if (event_name == this->m_pdi_execute_event_name) {
		run_catalyst_execute();
	}
}

void catalyst_plugin::run_catalyst_initialize()
{
	conduit_cpp::Node node;

	context().logger().info("Read information for script.");
	auto scripts_spec = PC_get(this->m_spec_tree, ".scripts");
	if (PC_status(scripts_spec)) {
		throw PDI::Config_error(m_spec_tree, "No scripts tree is defiend for catalyst plugin.");
	}

	int script_number = 0;
	PC_len(scripts_spec, &script_number);
	if (script_number == 0) {
		throw PDI::Config_error(scripts_spec, "Zero python script is defined for catalyst python.");
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

#ifdef CATALYST_IS_PARALLEL
	context().logger().info("Read mpi_comm.");

	auto communicator_spec = PC_get(this->m_spec_tree, ".communicator");
	if (!PC_status(communicator_spec)) {
		PDI::Expression communicator = PDI::to_string(communicator_spec);
		MPI_Comm tmp_comm = *(static_cast<const MPI_Comm*>(PDI::Ref_r{communicator.to_ref(context())}.get()));

		// create communicator node
		auto communicator_node = node["catalyst/mpi_comm"];

		// set the fortran MPI_COMMUNICATOR
		communicator_node.set_int64(static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));

		context().logger().debug("value of the communicator is {}:", static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));
	} else {
		// context().logger().warn("value of the communicator is {}:", static_cast<int64_t>(MPI_Comm_c2f(tmp_comm)));
		//throw PDI::Config_error{communicator_spec, "No communicator is given."};
		context().logger().warn("No communicator is given by default the communicator is MPI_COMM_WORD.");
	}
#else
	context().logger().info("Catalyst is used with no mpi");
	auto communicator_spec = PC_get(this->m_spec_tree, ".communicator");
	if (!PC_status(communicator_spec)) {
		context().logger().info("Used Catalyst with no mpi support. Invalid communicator: `{}'", PDI::to_string(communicator_spec));
		throw PDI::Config_error{
			communicator_spec,
			"Used Catalyst with no mpi support. Invalid communicator: `{}'",
			PDI::to_string(communicator_spec)
		};
	}
#endif

	// The following node is supported in the last version of Paraview
	// These nodes are not defined yet because we need some investigations.
	// node["catalyst_load/implementation"].set("stub") ;
	// node["catalyst_load/search_paths"].set("/path/to/install/catalyst/lib/catalyst/");
	// node["catalyst/pipelines"]
	// node["catalyst/python_path"]

	if (context().logger().level() == spdlog::level::debug || context().logger().level() == spdlog::level::trace) {
		context().logger().debug("Print node before catalyst_initialize call...");
		node.print();
	}

	context().logger().debug("catalyst_initialize call...");
	auto result = catalyst_initialize(conduit_cpp::c_node(&node));
	if (result != catalyst_status_ok) {
		// context().logger().error("catalyst_initialize failure");
		throw PDI::System_error("catalyst_initialize failure");
	}
	catalyst_is_initialize = true;
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
		PC_tree_t parent_tree; // Adding parent_tree for config error message
	};

	std::stack<Spec_tree_node> remaining_tree_and_parent_node;
	remaining_tree_and_parent_node.push({execute_spec, "catalyst", execute_node, execute_spec});
	while (!remaining_tree_and_parent_node.empty()) {
		auto current = remaining_tree_and_parent_node.top();
		remaining_tree_and_parent_node.pop();

		context().logger().info("Read node of name`{}'", current.name);
		if (current.name == "ghost_layers") {
			context().logger().info("Ghost layer node: `{}'", current.name);

			if (current.tree.node->type == YAML_MAPPING_NODE) {
				int data_tree_size = PDI::len(current.tree);
				if (data_tree_size == 0) {
					throw PDI::Config_error(current.tree, "ghost_layers node defined with a mapping node of size 0.");
				} else {
					context().logger().info("number of meshes(topologies) to consider = `{}'", data_tree_size);
				}

				// loop over the meshes in the ghost layers tree
				for (int index = data_tree_size - 1; index >= 0; --index) {
					list_vtkGhostType_to_create.emplace_back(context(), current.parent_node, current.tree, current.parent_tree, index);
				}
			} else {
				throw PDI::Config_error(current.tree, "ghost_layers node only support yaml mapping node.");
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
							remaining_tree_and_parent_node.push({value, PDI::to_string(key), conduit_cpp::c_node(&current_node), current.tree});
						} else {
							throw PDI::System_error("Error in creating vtkGhostType: a conduit node doesn't exist !!");
						}
					}
				}
			}
		}
	}
}

void catalyst_plugin::create_catalyst_conduit_node(conduit_node* execute_node, PC_tree_t& execute_spec)
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

		context().logger().info("Read node of name`{}'", current.name);
		if (current.name == "ghost_layers") {
			context().logger().info(" ghost_layers keys will be read after");
		// } else if  (current.name == "elements") {
		// 	int data_tree_size = PDI::len(current.tree);
		// 	// Check for dynamic PDI Data array
		// 	bool pdi_data_array = false;
		// 	for (int index = data_tree_size - 1; index >= 0; --index) {
		// 		auto key = PC_get(current.tree, "{%d}", index);
		// 		if (PDI::to_string(key) == "dims") {
		// 			PDI::Config_error{current.tree, "I found dims dans elements `{}'", current.name};
		// 		} else {
		// 			PDI::Config_error{current.tree, "The key is not dims and it is `{}'", PDI::to_string(key)};	
		// 		}
		// 	}
		} else {
			auto current_node = conduit_cpp::cpp_node(current.parent_node)[current.name];
			switch (current.tree.node->type) {
			case YAML_NO_NODE:
				throw PDI::Config_error{current.tree, "Unsupported Empty YAML Node for variable `{}'", current.name};
				// context().logger().error("Unsupported Empty YAML Node for variable `{}'", current.name);
				break;
			case YAML_SCALAR_NODE:
				switch (current.tree.node->data.scalar.style) {
				case YAML_PLAIN_SCALAR_STYLE:
				case YAML_SINGLE_QUOTED_SCALAR_STYLE:
					// handle integer or float/double type that depend perhaps on scalar PDI data
					{
						std::string data_name{PDI::to_string(current.tree)};
						PDI::Expression data_expression{PDI::to_string(current.tree)};
						PDI::Ref_r spec_ref = data_expression.to_ref(context());
						auto data_type = spec_ref.type()->evaluate(context());

						if (auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type)) {
							fill_node_with_scalar_pdi_data(conduit_cpp::c_node(&current_node), current.tree, data_name, *scalar_datatype, spec_ref);
						} else {
							// context().logger().error("Unsupported datatype for variable: `{}'. It should be scalar type (integer or float).", data_name);
							throw PDI::Config_error{
								current.tree,
								"Unsupported datatype for variable: `{}'. It should be scalar type (integer or float).",
								data_name
							};
						}
					}
					break;
				case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
					// handle string type
					current_node.set_string(PDI::to_string(current.tree));
					break;
				case YAML_LITERAL_SCALAR_STYLE:
				case YAML_FOLDED_SCALAR_STYLE:
				case YAML_ANY_SCALAR_STYLE:
					// context().logger().error("Unsupported YAML scalar style for variable `{}'", current.name);
					throw PDI::Config_error{current.tree, "Unsupported YAML scalar style for variable `{}'", current.name};
					break;
				}
				break;
			case YAML_SEQUENCE_NODE:
				// context().logger().error("Unsupported Sequence YAML Node for variable `{}'", current.name);
				throw PDI::Config_error{current.tree, "Unsupported Sequence YAML Node for variable `{}'", current.name};
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
				break;
			}
		}
	}
}

void catalyst_plugin::run_catalyst_execute()
{
	conduit_cpp::Node node;
	std::vector<Catalyst_plugin_structured_ghost> list_vtkGhostType_to_create; // object contain vector vtkGhostType
	auto execute_spec = PC_get(this->m_spec_tree, ".execute");

	conduit_node* node_pointer = conduit_cpp::c_node(&node);
	// create the conduit node for catalyst_execute
	create_catalyst_conduit_node(node_pointer, execute_spec);

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

	context().logger().debug("Print conduit node including vtk ghost type created ...");
	if (context().logger().level() == spdlog::level::debug || context().logger().level() == spdlog::level::trace) {
		node.print();
	}

	context().logger().debug("catalyst_execute call...");
	auto result = catalyst_execute(node_pointer);
	if (result != catalyst_status_ok) {
		// context().logger().error("catalyst_execute failure");
		throw PDI::System_error{"catalyst_execute failure"};
	}

	// clear m_current_pdi_data at each iteration
	// this->m_current_pdi_data.clear();
}

void catalyst_plugin::run_catalyst_finalize()
{
	if (catalyst_is_initialize) {
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
	auto name_spec = PC_get(tree, ".PDI_data_array");
	if (PC_status(name_spec)) {
		// context().logger().error("No \"name\" child in PDI_data_array spec.");
		throw PDI::Config_error{tree, "No \"name\" child in PDI_data_array spec."};
		return;
	}

	std::string name = PDI::to_string(name_spec);
	PDI::Ref_r ref_r = context()[name].ref();
	if (!ref_r) {
		context().logger().warn("Cannot read `{}' this data is not available", name);
		// Remark: This error can arrive outside PDI_initilialize. This implies that is not really a config error
		throw PDI::System_error{"No \"name\" child in PDI_data_array spec `{}'.", name};
		return;
	}

	auto data_type = ref_r.type();
	if (auto array_datatype = std::dynamic_pointer_cast<const PDI::Array_datatype>(data_type)) {
		fill_node_with_array_pdi_data(node, name, tree, *array_datatype, ref_r);
	} else {
		// context().logger().error("Unsupported datatype for variable: {}. The type should be array type.", name);
		// throw PDI::Config_error{tree, "Unsupported datatype for variable: `{}'. The type should be array type.", name};
		// Remark: This error can arrive outside PDI_initilialize. This implies that is not really a config error
		throw PDI::System_error{"Unsupported datatype for variable: `{}'. The type should be array type.", name};
	}
}

void catalyst_plugin::fill_node_with_scalar_pdi_data(
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
			// context().logger().error("Unknown SIGNED buffer size of {} for variable `{}'", buffer_size, name);
			throw PDI::Config_error{tree, "Unknown SIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
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
			// context().logger().error("Unknown UNSIGNED buffer size of {} for variable `{}'", buffer_size, name);
			throw PDI::Config_error{tree, "Unknown UNSIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else if (scalar_kind == PDI::Scalar_kind::FLOAT) {
		auto buffer_size = scalar_datatype.buffersize();
		if (buffer_size == sizeof(conduit_float32)) {
			catalyst_conduit_node_set_float32(node, *static_cast<const conduit_float32*>(ref_r.get()));
		} else if (buffer_size == sizeof(conduit_float64)) {
			catalyst_conduit_node_set_float64(node, *static_cast<const conduit_float64*>(ref_r.get()));
		} else {
			// context().logger().error("Unknown FLOAT buffer size of {} for variable `{}'", buffer_size, name);
			throw PDI::Config_error{tree, "Unknown FLOAT buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else {
		// context().logger().error("Unknown Scalar Type for variable `{}'", name);
		throw PDI::Config_error{tree, "Unknown Scalar Type for variable `{}'", name};
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
				// context().logger().error("The PDIData named \"{}\" is not readable.", name);
				throw PDI::System_error("The PDIData named \"{}\" is not readable.", name);

			}
			auto data_type = spec_ref.type()->evaluate(context());
			if (auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type)) {
				PDI::Scalar_kind scalar_kind = (*scalar_datatype).kind();
				if (scalar_kind == PDI::Scalar_kind::SIGNED || scalar_kind == PDI::Scalar_kind::UNSIGNED) {
					// return spec_ref.scalar_value<long>();
					tmp_value = data_expression.to_long(context());
				} else {
					throw PDI::Config_error{
						spec,
						"Unknown Scalar Type for variable `{}'. The type must be an integer signed or unsigned)",
						PDI::to_string(spec)
					};
					// context().logger().error("Unknown Scalar Type for variable {}. The type must be an integerc (signed or unsigned)", PDI::to_string(spec));
				}
			} else {
				throw PDI::Config_error{spec, "The datatype must be a scalar datatype for variable: `{}'", PDI::to_string(spec)};
				// context().logger().error("The datatype must be a scalar datatype for variable: `{}'", PDI::to_string(spec));
			}
		} else {
			// context().logger().error("Supported only YAML_SCALAR_NODE for variable `{}'", name);
			throw PDI::Config_error{spec, "Supported only YAML_SCALAR_NODE for variable `{}'", name};
		}
		
		// return value in conduit_index_t
		if (std::is_same<conduit_index_t, long>::value) {
			value = tmp_value;
		} else {
			// case conduit_index_t is 32-bits
			value = static_cast<conduit_index_t>(tmp_value);
			if (value != tmp_value) {
				// context().logger().error("Error in cast of a type conduit_index_t in long. {} != `{}'", value, tmp_value);
				throw PDI::System_error{"Error in cast of a type conduit_index_t in long. `{}' != `{}'", value, tmp_value};
			}
		}
	}
}

void catalyst_plugin::fill_node_with_array_pdi_data(
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
		// context().logger().error("Array subtype of variable {} should be scalar type.", name);
		throw PDI::Config_error{tree, "Array subtype of variable `{}' should be scalar type.", name};
		return;
	}

	conduit_index_t num_elements = 0;
	auto size_spec = PC_get(tree, ".size");
	get_conduit_index_t_value(size_spec, name, num_elements);

	if (num_elements == 0) {
		// context().logger().error("Unknown the size of an array of name {} passed to catalyst.", name);
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
			// throw PDI::Config_error{tree, "Unknown SIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
			// context().logger().error("Unknown SIGNED buffer size of {} for variable `{}'", buffer_size, name);
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
			//throw PDI::Config_error{tree, "Unknown UNSIGNED buffer size of `{}' for variable `{}'", buffer_size, name};
			// context().logger().error("Unknown UNSIGNED buffer size of {} for variable `{}'", buffer_size, name);
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
			// throw PDI::Config_error{tree, "Unknown FLOAT buffer size of `{}' for variable `{}'", buffer_size, name};
			// context().logger().error("Unknown FLOAT buffer size of {} for variable `{}'", buffer_size, name);
			throw PDI::System_error{"Unknown FLOAT buffer size of `{}' for variable `{}'", buffer_size, name};
		}
	} else {
		// context().logger().error("Unknown Scalar Type for variable `{}'", name);
		// throw PDI::Config_error{tree, "Unknown Scalar Type for variable `{}'", name};
		throw PDI::System_error{"Unknown Scalar Type for variable `{}'", name};
	}
}

// long catalyst_plugin::get_long_value_from_spec_node(PC_tree_t& spec, const std::string& name)
// {
// 	if (spec.node->type == YAML_SCALAR_NODE) {
// 		PDI::Expression data_expression{PDI::to_string(spec)};
// 		PDI::Ref_r spec_ref = data_expression.to_ref(context());
// 		if (!spec_ref) {
// 			context().logger().error("The PDIData named \"{}\" is not readable.", name);
// 			return 0;
// 		}
// 		auto data_type = spec_ref.type()->evaluate(context());
// 		if (auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type)) {
// 			PDI::Scalar_kind scalar_kind = (*scalar_datatype).kind();
// 			if (scalar_kind == PDI::Scalar_kind::SIGNED || scalar_kind == PDI::Scalar_kind::UNSIGNED) {
// 				// return spec_ref.scalar_value<long>();
// 				return data_expression.to_long(context());
// 			} else {
// 				throw PDI::Config_error{
// 					spec,
// 					"Unknown Scalar Type for variable `{}'. The type must be an integerc (signed or unsigned)",
// 					PDI::to_string(spec)
// 				};
// 				// context().logger().error("Unknown Scalar Type for variable {}. The type must be an integerc (signed or unsigned)", PDI::to_string(spec));
// 			}
// 		} else {
// 			throw PDI::Config_error{spec, "The datatype must be a scalar datatype for variable: `{}'", PDI::to_string(spec)};
// 			// context().logger().error("The datatype must be a scalar datatype for variable: `{}'", PDI::to_string(spec));
// 		}
// 		return 0;
// 	} else {
// 		// context().logger().error("Supported only YAML_SCALAR_NODE for variable `{}'", name);
// 		throw PDI::Config_error{spec, "Supported only YAML_SCALAR_NODE for variable `{}'", name};
// 	}
// 	return 0;
// }

std::string catalyst_plugin::read_pdi_execute_event_name()
{
	std::string event_name;
	auto execute_spec = PC_get(this->m_spec_tree, ".on_event");
	if (PC_status(execute_spec) == PC_OK) {
		event_name = PDI::to_string(execute_spec);
	} else {
		throw PDI::Config_error{execute_spec, "No event name for catalyst plugin is given"};
	}

	return event_name;
}
