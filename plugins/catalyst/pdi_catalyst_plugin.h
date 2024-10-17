#ifndef CATALYST_PLUGIN_H
#define CATALYST_PLUGIN_H

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

struct conduit_node_impl;
typedef struct conduit_node_impl conduit_node;

struct catalyst_plugin : public PDI::Plugin
{
  catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree);
  ~catalyst_plugin();

private:
  void ProcessPDIInit();
  void ProcessData(const std::string& data_name, PDI::Ref ref);
  void ProcessEvent(const std::string& event_name);

  void RunCatalystInitialize();
  void RunCatalystExecute();
  void RunCatalystFinalize();
  void FillNodeWithPDIData(conduit_node* node, PC_tree_t tree);
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
