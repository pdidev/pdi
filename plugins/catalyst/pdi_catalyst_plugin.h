#ifndef CATALYST_PLUGIN_H
#define CATALYST_PLUGIN_H

#include "catalyst.hpp" // ???
#include "catalyst_plugin_structured_ghost.h" // ???

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

struct conduit_node_impl; // ??
typedef struct conduit_node_impl conduit_node; // ??

/**
 * @brief Translates PDI calls to Catalyst calls.
 *
 * The PDI Catalyst Plugin is an adapter to call Catalyst API (initialize, execute, finalize)
 * from PDI API calls (PDI_init, PDI_multi_expose, PDI_finalize).
 *
 * It leverages the specification tree to copy only pointer to data. The conduit node structure of
 * the catalyst_execute call is defined in the spec tree.
 *
 */
class catalyst_plugin: public PDI::Plugin
{
public:
	/// @brief Builds a catalsyt_plugin specification tree from its yaml config
	catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree);

	~catalyst_plugin() noexcept(false);

private:
	/// @brief callback used at pdi_init
	void process_pdi_init();

	/// @brief trigger action of catalyst plugin when a data is exposed to pdi
	/// @param data_name: name of the current data exposed to pdi
	/// @param ref: reference of the data exposed to pdi
	void process_data(const std::string& data_name, PDI::Ref ref);

	/// @brief trigger action of catalyst plugin when an event occur
	/// @param event_name: name of the current event
	void process_event(const std::string& event_name);

	/// @brief function running in pdi_init
	void run_catalyst_initialize();

	/// @brief function running in the event corresponding to catalyst_execute
	void run_catalyst_execute();

	/// @brief function running in pdi_finalize
	void run_catalyst_finalize();

	/// @brief TO COMPLET
	/// @param execute_node
	/// @param execute_spec
	/// @param list_vtkGhostType_to_create
	void read_info_for_creating_vtk_ghost(
		conduit_node* execute_node,
		PC_tree_t& execute_spec,
		std::vector<Catalyst_plugin_structured_ghost>& list_vtkGhostType_to_create
	);

	/// @brief creates a conduit_node for catalyst_excute from yaml tree
	/// @param execute_node conduit node that will be created
	/// @param execute_spec The tree representing the execute section
	void create_catalyst_execute_conduit_node(conduit_node* execute_node, PC_tree_t& execute_spec);

	/// @brief Fills a conduit node corresponding to array shared with pdi from a yaml tree. 
	/// @param the node in which to operate
	/// @param tree specification tree containing a PDI_data_array
	void fill_node_with_pdi_data_array(conduit_node* node, PC_tree_t& tree);

	/// @brief Sets value of a conduit node corresponding to a pdi scalar datatype from a yaml tree
	/// @param node the node in which we set the value
	/// @param name name of the array
	/// @param tree specification tree containing a PDI_data_array
	/// @param scalar_datatype type of the scalar
	/// @param ref_r reference of the array 
	void set_value_for_pdi_scalar_datatype(
		conduit_node* node,
		PC_tree_t& tree,
		const std::string& name,
		const PDI::Scalar_datatype& scalar_datatype,
		PDI::Ref_r& ref_r
	);

	/// @brief Sets values of a conduit node corresponding to a pdi array datatype from a yaml tree
	/// @param node the node in which we set the value
	/// @param name name of the array
	/// @param tree specification tree containing a PDI_data_array
	/// @param array_datatype type of the array
	/// @param ref_r reference of the array 
	void set_value_for_pdi_array_datatype(
		conduit_node* node,
		const std::string& name,
		PC_tree_t& tree,
		const PDI::Array_datatype& array_datatype,
		PDI::Ref_r& ref_r
	);

	/// @brief return an index description (i.e. size, offset, stride) of the array that correspond to a conduit node
	/// @param spec : specification tree where the index is defined
	/// @param name : The name of the data that corresponding to the index.
	/// @param value: value of the index
	void get_conduit_index_t_value(PC_tree_t& spec, const std::string& name, conduit_index_t& value);

	/// @brief return the event name to call catalyst_execute
	std::string read_pdi_execute_event_name();

	/// @brief  variable to know if catalyst_initialize is called.
	/// Remark: The call of catalyst_finalize doesn't return a okay status if catalyst_initialize is not called before.
	///         Example: In case of config error in the yaml file needed by catalyst_initialize. Moreover, the config error message cannot be see by the user.
	bool catalyst_is_initialize;

	/// @brief specification tree for catalyst plugin
	PC_tree_t m_spec_tree;

	/// @brief list of pdi data array passed to catalyst in catalyst_execute
	/// std::unordered_map<std::string, PDI::Ref> m_current_pdi_data;

	/// @brief name of event use to call catalyst_execute
	std::string m_pdi_execute_event_name;
};

PDI_PLUGIN(catalyst)

#endif // CATALYST_PLUGIN_H
