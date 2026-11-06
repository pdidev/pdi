#ifndef CATALYST_PLUGIN_H
#define CATALYST_PLUGIN_H

#include <mpi.h>
#include <unordered_set>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

#include "catalyst_plugin_structured_ghost.h"

struct conduit_node_impl;
typedef struct conduit_node_impl conduit_node;

/**
 * \brief Translates PDI calls to Catalyst calls.
 *
 * The PDI Catalyst Plugin is an adapter to call Catalyst API (initialize, execute, finalize)
 * from PDI API calls (PDI_init, PDI_multi_expose, PDI_finalize).
 *
 * It leverages the specification tree to copy only pointer to data. The Conduit Node structure of
 * the catalyst_execute call is defined in the spec tree.
 *
 */
class catalyst_plugin: public PDI::Plugin
{
public:
	/** the list of plugins that depend catalyst_plugin
	 *
	 * \return the dependecies of the plugin
	 */
	static std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> dependencies()
	{
		return {std::unordered_set<std::string>{}, {"mpi"}};
	}

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Catalyst"; }

	/// \brief Builds a catalyst_plugin specification tree from its yaml config
	catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree);

	~catalyst_plugin() noexcept;

private:
	/// \brief callback to trigger action of catalyst_execute when an event occur
	/// \param event_name: name of the event
	void process_event(const std::string& event_name);

	// /// \brief callback to trigger action of catalyst_initialize when an event occur
	// /// This function is used when a communicator is given by the user
	// /// \param event_name: name of the current event
	// void process_pdi_init_with_event(const std::string& event_name);

	/// \brief function running in pdi_init
	void run_catalyst_initialize(std::optional<PDI::Ref_r> ref_comm = std::nullopt);

	/// \brief function running in the event corresponding to catalyst_execute
	void run_catalyst_execute();

	/// \brief function running in pdi_finalize
	void run_catalyst_finalize();

	/// \brief Builds Conduit node corresponding to vtkGhostType object for the user
	/// \param execute_node Conduit node in which we set the vtkGhostType
	/// \param execute_spec the tree representing the execute section
	/// \param list_vtkGhostType_to_create the object reprensenting the vtkGhostType field(s)
	void build_vtk_ghost_type(
		conduit_node* execute_node,
		PC_tree_t& execute_spec,
		std::vector<Catalyst_plugin_structured_ghost>& list_vtkGhostType_to_create
	);

	/// \brief Creates a conduit_node for catalyst_excute from specification tree
	/// \param execute_node Conduit Node that will be created
	/// \param execute_spec the tree representing the execute section
	void create_catalyst_execute_conduit_node(conduit_node* execute_node, PC_tree_t& execute_spec);

	/// \brief Fills a Conduit Node corresponding to array shared with pdi from a yaml tree.
	/// \param the node in which to operate
	/// \param tree specification tree containing a PDI_data_array
	void fill_node_with_pdi_data_array(conduit_node* node, PC_tree_t& tree);

	/// \brief Sets value of a Conduit Node corresponding to a pdi scalar datatype from a yaml tree
	/// \param node the node in which we set the value
	/// \param name name of the array
	/// \param tree specification tree containing a PDI_data_array
	/// \param scalar_datatype type of the scalar
	/// \param ref_r reference of the array
	void set_value_for_pdi_scalar_datatype(
		conduit_node* node,
		PC_tree_t& tree,
		const std::string& name,
		const PDI::Scalar_datatype& scalar_datatype,
		PDI::Ref_r& ref_r
	);

	/// \brief Sets values of a Conduit Node corresponding to a pdi array datatype from a yaml tree
	/// \param node the node in which we set the value
	/// \param name name of the array
	/// \param tree specification tree containing a PDI_data_array
	/// \param array_datatype type of the array
	/// \param ref_r reference of the array
	void set_value_for_pdi_array_datatype(
		conduit_node* node,
		const std::string& name,
		PC_tree_t& tree,
		const PDI::Array_datatype& array_datatype,
		PDI::Ref_r& ref_r
	);

	/// \brief return an index description (i.e. size, offset, stride) of the array that corresponds to a Conduit Node
	/// \param spec : specification tree where the index is defined
	/// \param name : The name of the data that corresponding to the index.
	/// \param value: value of the index
	void get_conduit_index_t_value(PC_tree_t& spec, const std::string& name, conduit_index_t& value);


#ifdef CATALYST_IS_PARALLEL
	void init_communicator(std::string data_name, PDI::Ref ref);
#endif

	/// \return the event name to call catalyst_execute
	std::string read_pdi_execute_event_name();

	/// variable to know if catalyst_initialize is called.
	/// Remark: The call of catalyst_finalize doesn't return a okay status if catalyst_initialize is not called before.
	///         Example: In case of config error in the yaml file needed by catalyst_initialize.
	///         Moreover, the config error message cannot be see by the user.
	bool catalyst_is_initialized;


	/// specification tree for catalyst plugin
	PC_tree_t m_spec_tree;

	/////////////////////////////////////////////////////////
	// variable read from the specification tree

	// /// name of event use to call catalyst_initialize
	// std::string m_pdi_initialize_event_name; // keep only for logger message

#ifdef CATALYST_IS_PARALLEL
	/// nale of the communicator use in catalyst
	std::string m_communicator;
	/// variable to know if the processor is inside the communicator (warning message only)
	bool m_proc_in_communicator;
	/// variable to know if the communicator is exposed to PDI before
	bool m_communicator_is_exposed;
#endif

	/// when condition for catalyst_execute
	PDI::Expression m_when;

	// end variable read from the specification tree
	/////////////////////////////////////////////////////////
};

PDI_PLUGIN(catalyst)

#endif // CATALYST_PLUGIN_H
