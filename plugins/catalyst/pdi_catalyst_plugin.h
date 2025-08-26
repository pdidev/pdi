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
 * the catalyst_execute call is defined in the spec tree, and dynamic data are referenced with the
 * special keyword "PDI_data".
 *
 */
class catalyst_plugin : public PDI::Plugin
{
public:
  catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree);
  ~catalyst_plugin();

private:
  void ProcessPDIInit();
  void ProcessData(const std::string& data_name, PDI::Ref ref);
  void ProcessEvent(const std::string& event_name);

  void RunCatalystInitialize();
  void RunCatalystExecute();
  void RunCatalystFinalize();
  void FillNodeWithPDIDataArray(conduit_node* node, PC_tree_t tree);
  void FillNodeWithScalarPDIData(conduit_node* node, const std::string& name,
    const PDI::Scalar_datatype& scalar_datatype, PDI::Ref_r& ref_r);
  void FillNodeWithArrayPDIData(conduit_node* node, const std::string& name, PC_tree_t& tree,
    const PDI::Array_datatype& array_datatype, PDI::Ref_r& ref_r);
  long GetLongValueFromSpecNode(PC_tree_t& spec, const std::string& name);
  std::string ReadPDIExecuteEventName();

  PC_tree_t SpecTree;
  std::unordered_map<std::string, PDI::Ref> CurrentPDIData;
  std::string PDIExecuteEventName;
};

PDI_PLUGIN(catalyst)

#endif // CATALYST_PLUGIN_H
