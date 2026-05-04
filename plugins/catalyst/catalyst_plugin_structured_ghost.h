#ifndef catalyst_plugin_structured_ghost_H
#define catalyst_plugin_structured_ghost_H

#include "catalyst.hpp"

#include <iostream>
#include <stack>
#include <vector>

#include <pdi/context.h>
#include <pdi/error.h>

class Catalyst_plugin_structured_ghost
{
	/// Context of this object
	PDI::Context& m_ctx;

	/// The tree representing the ghost config
	PC_tree_t m_ghost_tree;
	/// The parent tree of m_ghost_tree for specification tree error message
	PC_tree_t m_parent_tree;

	/// name of the mesh (It correspond to the topology name in the mesh blue print)
	std::string m_topology_name; // topology name

	/// dimensions of the mesh in each direction (including ghosts)
	std::vector<int> m_dimensions;

	/// start and size in each direction
	std::vector<PDI::Expression> m_start;
	std::vector<PDI::Expression> m_size;
	std::string m_association;

	/// path in the conduit node for catalyst
	std::string m_parent_node_path;

	/// vtkGhostType vector for paraview
	std::vector<uint8_t> m_vtk_ghost_type;

public:
	Catalyst_plugin_structured_ghost(PDI::Context& ctx, conduit_node* parent_node, PC_tree_t& tree, PC_tree_t& parent_tree, const int& index)
		: m_ctx{ctx}
		, m_ghost_tree(tree)
		, m_parent_tree(parent_tree)
		, m_vtk_ghost_type{1, 0}
	{
		// get the name of mesh(topology)
		auto m_topology_name_spec = PC_get(m_ghost_tree, "{%d}", index);
		if (!PC_status(m_topology_name_spec)) {
			m_topology_name = PDI::to_string(m_topology_name_spec);
		} else {
			throw PDI::Spectree_error{m_ghost_tree, "The name of the topology is not defined."};
		}

		// A TESTER:
		std::string value_type = get_name_from_parent_node(parent_node, "type");

		m_ctx.logger().info("topology type is `{}'", value_type);

		//===============================
		// topo=structured
		if (value_type == "structured") {
			std::string path_to_dims = "topologies/" + m_topology_name + "/elements/dims/";
			std::string PC_to_dataname = ".topologies." + m_topology_name;

			get_dimension(parent_node, path_to_dims, PC_to_dataname, "topology", m_topology_name);
		}
		//================
		// topo=uniform
		else if (value_type == "uniform")
		{
			// A TESTER:
			std::string value_coordset = get_name_from_parent_node(parent_node, "coordset");

			m_ctx.logger().info("For the uniform topology `{}', the name of coordset is `{}'", m_topology_name, value_coordset);

			std::string path_to_origins = "topologies/" + m_topology_name + "/origin";

			if (conduit_cpp::cpp_node(parent_node).has_path(path_to_origins)) {
				throw PDI::Spectree_error{
					m_ghost_tree,
					"For uniform topology, we dont support origin keyword to generate vtkGhostType for paraview."
				};
			}

			std::string path_to_dims = "coordsets/" + value_coordset + "/dims/";
			std::string PC_to_dataname = ".coordsets." + value_coordset;

			get_dimension(parent_node, path_to_dims, PC_to_dataname, "coordset", value_coordset);

			// The value in dims_vecGhost corresponding to the number of points in each direction
			// We remove 1 because we consider the number of elements in each direction when we create vtkGhostType
			for (int ii = 0; ii < m_dimensions.size(); ++ii) {
				m_dimensions[ii]--;
			}
		} else {
			// Config error is return because we are in a constructor.
			throw PDI::Spectree_error{
				m_ghost_tree,
				"ghost_layers yaml node is only valid with uniform and structured topology. The topology is `{}'",
				value_type
			};
		}

		// define the conduit for the vtkGhostType

		auto mask_ghost_spec = PC_get(m_ghost_tree, "<%d>", index);

		PDI::each(mask_ghost_spec, [&](PC_tree_t key_tree, PC_tree_t value) {
			std::string key = PDI::to_string(key_tree);
			m_ctx.logger().debug("read key= {} in ghost layers section.", key);
			if (key == "size") {
				PDI::opt_each(value, [&](PC_tree_t size) { m_size.emplace_back(PDI::to_string(size)); });
			} else if (key == "start") {
				PDI::opt_each(value, [&](PC_tree_t start) { m_start.emplace_back(PDI::to_string(start)); });
			} else if (key == "association") {
				m_association = PDI::to_string(value);
			} else {
				throw PDI::Spectree_error{key_tree, "Invalid configuration key in mask_ghost for topology `{}': `{}'", m_topology_name, key};
			}
		});

		if (m_size.size() != m_start.size()) {
			throw PDI::Spectree_error{
				m_ghost_tree,
				"Invalid configuration in mask_ghost for topology `{}' the number of elements in size and in start are not the same.",
				m_topology_name
			};
		}
		if (m_size.size() != m_dimensions.size()) {
			throw PDI::Spectree_error{
				m_parent_tree,
				"Invalid configuration in mask_ghost for topology `{}', the dimension of the problem `{}' is not equal to `{}' the number of "
			    "elements in size and in start.",
				m_topology_name,
				m_dimensions.size(),
				m_size.size()
			};
		}

		// check size + start + dims (TODO:  en dernier)

		m_ctx.logger().info("space dimension {}", m_dimensions.size());
		for (int ii = 0; ii < m_dimensions.size(); ++ii) {
			m_ctx.logger().info("`{}'-th dimensions of the mesh `{}'", ii, m_dimensions[ii]);
		}

		m_parent_node_path = conduit_cpp::cpp_node(parent_node).path();
		m_ctx.logger().info("conduit node path for the parent node is `{}'", m_parent_node_path);
	}

	~Catalyst_plugin_structured_ghost() {}

	/// creation of the mask ghost (VtkGhostType) need by paraview
	void create_vtk_ghost_type()
	{
		int space_dimension = m_dimensions.size();

		std::vector<long> last(space_dimension);
		std::vector<long> start(space_dimension);

		for (int size_id = 0; size_id < space_dimension; ++size_id) {
			last[size_id] = m_size[size_id].to_long(m_ctx);
			start[size_id] = m_start[size_id].to_long(m_ctx);
		}

		std::transform(start.begin(), start.end(), last.begin(), last.begin(), [](long start, long last) { return last + start; });

		for (int ii = 0; ii < space_dimension; ++ii) {
			m_ctx.logger().info("start: `{}'-th dimensions of the mesh `{}'", ii, start[ii]);
			m_ctx.logger().info("last: `{}'-th dimensions of the mesh `{}'", ii, last[ii]);
		}

		if (space_dimension == 2) {
			size_t vsize = m_dimensions[0] * m_dimensions[1];
			m_vtk_ghost_type.resize(vsize);

			for (int ii = 0; ii < m_dimensions[1]; ++ii) {
				for (int jj = 0; jj < m_dimensions[0]; ++jj) {
					if (jj < start[0] || jj >= last[0] || ii < start[1] || ii >= last[1]) {
						m_vtk_ghost_type[ii * (m_dimensions[0]) + jj] = (uint8_t)1;
					} else {
						m_vtk_ghost_type[ii * (m_dimensions[0]) + jj] = (uint8_t)0;
					}
				}
			}
		} else if (space_dimension == 3) {
			size_t vsize = m_dimensions[0] * m_dimensions[1] * m_dimensions[2];
			m_vtk_ghost_type.resize(vsize);
			for (int ii = 0; ii < m_dimensions[2]; ++ii) {
				for (int jj = 0; jj < m_dimensions[1]; ++jj) {
					for (int kk = 0; kk < m_dimensions[0]; ++kk) {
						if (kk < start[0] || jj < start[1] || ii < start[2]) {
							m_vtk_ghost_type[ii * (m_dimensions[1] * m_dimensions[0]) + jj * m_dimensions[0] + kk] = 1;
						} else if (kk >= last[0] || jj >= last[1] || ii >= last[2]) {
							m_vtk_ghost_type[ii * (m_dimensions[1] * m_dimensions[0]) + jj * m_dimensions[0] + kk] = 1;
						} else {
							m_vtk_ghost_type[ii * (m_dimensions[1] * m_dimensions[0]) + jj * m_dimensions[0] + kk] = 0;
						}
					}
				}
			}
		} else {
			std::cout << " Error in the creation of the vtkGhostType for the users: The dimension for the mesh must be 2 or 3." << std::endl;
		}
	}

	/// return the pointer to the mask ghost
	uint8_t* get_vector() { return m_vtk_ghost_type.data(); }

	/// get the size of the pointer of the mask ghost
	size_t get_size() { return m_vtk_ghost_type.size(); }

	/// get the name path in the conduit node
	const std::string& get_node_path() const { return m_parent_node_path; }

	/// @brief get the name of topology(mesh)
	const std::string& get_topology_name() const { return m_topology_name; }

	/// @brief retrieve the corresponding PC_tree for a given coordset or a given topology
	PC_tree_t retrieve_pc_tree_from_parent_node(const std::string& structname, const std::string& dataname)
	{
		std::string index_all = "." + structname + "." + m_topology_name + "." + dataname;
		auto dataname_tree = PC_get(m_parent_tree, index_all.c_str());

		return dataname_tree;
	}

	std::string get_name_from_parent_node(conduit_node* parent_node, const std::string& dataname)
	{
		std::string path_to_type = "topologies/" + m_topology_name + "/" + dataname;
		std::string PC_to_type = ".topologies." + m_topology_name + "." + dataname;

		bool dataname_is_empty = conduit_cpp::cpp_node(parent_node)[path_to_type].dtype().is_empty();
		if (!dataname_is_empty) {
			bool dataname_is_string = conduit_cpp::cpp_node(parent_node)[path_to_type].dtype().is_string();
			if (dataname_is_string) {
				return conduit_cpp::cpp_node(parent_node)[path_to_type].as_string();
			} else {
				PC_tree_t msg_tree = retrieve_pc_tree_from_parent_node("topologies", dataname);
				throw PDI::Spectree_error{
					msg_tree,
					"... Vec Ghost Type catalyst ... The {} for topology `{}' is not defined as a string.",
					dataname,
					m_topology_name
				};
			}
		} else {
			throw PDI::Spectree_error{
				m_parent_tree,
				"... Vec Ghost Type catalyst ... The {} for topology `{}' is not defined.",
				dataname,
				m_topology_name
			};
		}
	}

	/// @brief Retrieve the dimension of the mask ghost
	/// @param parent_node
	/// @param path_to_dims    // path in the conduit node where the dimensions are
	/// @param PC_to_dataname  // path in the PC_tree to get the PC_tree for error message
	/// @param data_type       // type of data (coordset or topology) where the dimensions are defiened for error message
	/// @param data_type_name  // name of coordset or name of topology for error message
	void get_dimension(
		conduit_node* parent_node,
		std::string& path_to_dims,
		std::string& PC_to_dataname,
		std::string data_type,
		std::string& data_type_name
	)
	{
		std::string msg_data = data_type + " " + data_type_name;
		PC_tree_t msg_tree = PC_get(m_parent_tree, PC_to_dataname.c_str());

		if (PC_status(msg_tree)) {
			throw PDI::Spectree_error(msg_tree, "");
		} else {
			if (conduit_cpp::cpp_node(parent_node).has_path(path_to_dims)) {
				std::list<std::string> list_dims{"i", "j", "k"};
				for (auto&& elem: list_dims) {
					// verify dims/{elem} exist in the node m_ghost_tree
					std::string path_leaf = path_to_dims + elem;
					if (conduit_cpp::cpp_node(parent_node).has_path(path_leaf)) {
						auto node_path = conduit_cpp::cpp_node(parent_node)[path_leaf];
						// check the variable is an integer
						if (node_path.dtype().is_integer()) {
							int tmp_int = (int)node_path.to_int();
							m_dimensions.emplace_back(tmp_int);
							m_ctx.logger().info("dims/`{}' = `{}' for the `{}'.", elem, tmp_int, msg_data);
						} else if (node_path.dtype().is_long()) {
							int tmp_int = (int)node_path.to_long();
							m_dimensions.emplace_back(tmp_int);
							m_ctx.logger().info("dims/`{}' = `{}' for the `{}'.", elem, tmp_int, msg_data);
						} else {
							throw PDI::Spectree_error{msg_tree, "For `{}' the value of dims/`{}' is not an integer or a long", msg_data, elem};
						}
					} else {
						// info message in case of dims/i, dims/j, dims/k  doesn't exist.
						m_ctx.logger().info("No dims/`{}' is not defined for the `{}'.", elem, msg_data);
					}
				}
				if (m_dimensions.size() == 0) {
					throw PDI::Spectree_error{msg_tree, "No dims/i , dims/j and dims/k are defined for the `{}'", msg_data};
				}
			} else {
				throw PDI::Spectree_error(msg_tree, "For the `{}', we need dims keyword to generate vtkGhostType for catalyst.", msg_data);
			}
		}
	}
};



#endif // catalyst_plugin_structured_ghost_H
