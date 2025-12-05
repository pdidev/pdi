#ifndef CATALYST_PLUGIN_H
#define CATALYST_PLUGIN_H

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

struct conduit_node_impl;
typedef struct conduit_node_impl conduit_node;

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
	catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree);
	~catalyst_plugin();

private:
	void process_pdi_init();
	void process_data(const std::string& data_name, PDI::Ref ref);
	void process_event(const std::string& event_name);

	void run_catalyst_initialize();
	void run_catalyst_execute();
	void run_catalyst_finalize();
	void fill_node_with_pdi_data_array(conduit_node* node, PC_tree_t tree);
	void fill_node_with_scalar_pdi_data(conduit_node* node, const std::string& name, const PDI::Scalar_datatype& scalar_datatype, PDI::Ref_r& ref_r);
	void fill_node_with_array_pdi_data(
		conduit_node* node,
		const std::string& name,
		PC_tree_t& tree,
		const PDI::Array_datatype& array_datatype,
		PDI::Ref_r& ref_r
	);
	long get_long_value_from_spec_node(PC_tree_t& spec, const std::string& name);
	std::string read_pdi_execute_event_name();

	PC_tree_t m_spec_tree;
	std::unordered_map<std::string, PDI::Ref> m_current_pdi_data;
	std::string m_pdi_execute_event_name;
};

PDI_PLUGIN(catalyst)

#endif // CATALYST_PLUGIN_H
