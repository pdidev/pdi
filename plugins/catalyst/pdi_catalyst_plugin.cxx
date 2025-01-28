#include "pdi_catalyst_plugin.h"

#include "catalyst.hpp"

#include <iostream>
#include <stack>

catalyst_plugin::catalyst_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
  : Plugin{ ctx }
  , SpecTree(spec_tree)
{
  ctx.callbacks().add_init_callback([this]() { this->ProcessPDIInit(); });
  ctx.callbacks().add_data_callback(
    [this](const std::string& data_name, PDI::Ref ref) { this->ProcessData(data_name, ref); });
  ctx.callbacks().add_event_callback(
    [this](const std::string& event_name) { this->ProcessEvent(event_name); });
}

catalyst_plugin::~catalyst_plugin()
{
  RunCatalystFinalize();
}

void catalyst_plugin::ProcessPDIInit()
{
  this->RunCatalystInitialize();
  this->PDIExecuteEventName = this->ReadPDIExecuteEventName();
}

void catalyst_plugin::ProcessData(const std::string& data_name, PDI::Ref ref)
{
  context().logger().debug("User has shared a data named {}", data_name);
  auto it = this->CurrentPDIData.find(data_name);
  if (it != this->CurrentPDIData.end())
  {
    context().logger().warn(
      "Data named '{}' already recorded, the previous value will overwritten.", data_name);
    it->second = ref.copy();
  }
  else
  {
    this->CurrentPDIData.emplace(data_name, ref);
  }
}

void catalyst_plugin::ProcessEvent(const std::string& event_name)
{
  if (event_name == this->PDIExecuteEventName)
  {
    RunCatalystExecute();
  }
}

void catalyst_plugin::RunCatalystInitialize()
{
  conduit_cpp::Node node;
  auto scripts_node = node["catalyst/scripts"];
  auto scripts_spec = PC_get(this->SpecTree, ".scripts");
  int script_number = 0;
  PC_len(scripts_spec, &script_number);
  for (int i = 0; i < script_number; ++i)
  {
    auto key = PC_get(scripts_spec, "{%d}", i);
    auto value = PC_get(scripts_spec, "<%d>", i);
    scripts_node[PDI::to_string(key)] = PDI::to_string(value);
  }

  context().logger().debug("catalyst_initialize call...");
  auto result = catalyst_initialize(conduit_cpp::c_node(&node));
  if (result != catalyst_status_ok)
  {
    context().logger().error("catalyst_initialize failure");
  }
}

void catalyst_plugin::RunCatalystExecute()
{
  conduit_cpp::Node node;

  auto execute_spec = PC_get(this->SpecTree, ".execute");

  // walk the spec tree and create corresponding catalyst nodes.
  struct SpecTreeNode
  {
    PC_tree_t tree;
    std::string name;
    conduit_node* parentNode;
  };

  std::stack<SpecTreeNode> remainingTreeAndParentNode;
  remainingTreeAndParentNode.push({ execute_spec, "catalyst", conduit_cpp::c_node(&node) });
  while (!remainingTreeAndParentNode.empty())
  {
    auto current = remainingTreeAndParentNode.top();
    remainingTreeAndParentNode.pop();

    auto current_node = conduit_cpp::cpp_node(current.parentNode)[current.name];

    switch (current.tree.node->type)
    {
      case YAML_NO_NODE:
        context().logger().error("Unsupported Empty YAML Node for variable {}", current.name);
        break;
      case YAML_SCALAR_NODE:
        switch (current.tree.node->data.scalar.style)
        {
          case YAML_PLAIN_SCALAR_STYLE:
            // TODO: handle float/double type.
            current_node.set_int64(PDI::to_long(current.tree));
            break;
          case YAML_SINGLE_QUOTED_SCALAR_STYLE:
          case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
            current_node.set_string(PDI::to_string(current.tree));
            break;
          case YAML_LITERAL_SCALAR_STYLE:
          case YAML_FOLDED_SCALAR_STYLE:
          case YAML_ANY_SCALAR_STYLE:
            context().logger().error("Unsupported YAML scalar style for variable {}", current.name);
            break;
        }
        break;
      case YAML_SEQUENCE_NODE:
        context().logger().error("Unsupported Sequence YAML Node for variable {}", current.name);
        break;
      case YAML_MAPPING_NODE:
        int data_tree_size = PDI::len(current.tree);
        // Check for dynamic PDI Data.
        if (data_tree_size == 1)
        {
          auto key = PC_get(current.tree, "{%d}", 0);
          if (PDI::to_string(key) == "PDI_data")
          {
            auto value = PC_get(current.tree, "<%d>", 0);
            this->FillNodeWithPDIData(conduit_cpp::c_node(&current_node), value);
            break;
          }
        }
        // reverse order to get the correct order when poping the stack.
        for (int i = data_tree_size - 1; i >= 0; --i)
        {
          auto key = PC_get(current.tree, "{%d}", i);
          auto value = PC_get(current.tree, "<%d>", i);
          // std::cout << "Mapping Node: " << PDI::to_string(key) << std::endl;
          remainingTreeAndParentNode.push(
            { value, PDI::to_string(key), conduit_cpp::c_node(&current_node) });
        }
        break;
    }
  }

  if (context().logger().level() == spdlog::level::debug ||
    context().logger().level() == spdlog::level::trace)
  {
    node.print();
  }
  context().logger().debug("catalyst_execute call...");
  auto result = catalyst_execute(conduit_cpp::c_node(&node));
  if (result != catalyst_status_ok)
  {
    context().logger().error("catalyst_execute failure");
  }

  this->CurrentPDIData.clear();
}

void catalyst_plugin::RunCatalystFinalize()
{
  context().logger().debug("catalyst_finalize call...");
  conduit_cpp::Node node;
  auto result = catalyst_finalize(conduit_cpp::c_node(&node));
  if (result != catalyst_status_ok)
  {
    context().logger().error("catalyst_finalize failure");
  }
}

void catalyst_plugin::FillNodeWithPDIData(conduit_node* node, PC_tree_t tree)
{
  auto name_spec = PC_get(tree, ".name");
  if (PC_status(name_spec))
  {
    context().logger().error("No \"name\" child in PDI_data spec.");
    return;
  }

  std::string name = PDI::to_string(name_spec);
  auto it = this->CurrentPDIData.find(name);
  if (it == this->CurrentPDIData.end())
  {
    context().logger().error("Can't find the PDI_data named: {}", name);
    return;
  }
  auto ref = it->second;
  PDI::Ref_r ref_r{ ref };
  if (!ref_r)
  {
    context().logger().error("The PDIData named \"{}\" is not readable.", name);
    return;
  }

  auto data_type = ref_r.type();
  if (auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(data_type))
  {
    FillNodeWithScalarPDIData(node, name, *scalar_datatype, ref_r);
  }
  else if (auto array_datatype = std::dynamic_pointer_cast<const PDI::Array_datatype>(data_type))
  {
    FillNodeWithArrayPDIData(node, name, tree, *array_datatype, ref_r);
  }
  else
  {
    context().logger().error("Unsupported datatype for variable: {}", name);
  }
}

void catalyst_plugin::FillNodeWithScalarPDIData(conduit_node* node, const std::string& name,
  const PDI::Scalar_datatype& scalar_datatype, PDI::Ref_r& ref_r)
{
  PDI::Scalar_kind scalar_kind = scalar_datatype.kind();
  if (scalar_kind == PDI::Scalar_kind::SIGNED)
  {
    auto buffer_size = scalar_datatype.buffersize();
    if (buffer_size == sizeof(conduit_int8))
    {
      catalyst_conduit_node_set_int8(node, *static_cast<const conduit_int8*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_int16))
    {
      catalyst_conduit_node_set_int16(node, *static_cast<const conduit_int16*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_int32))
    {
      catalyst_conduit_node_set_int32(node, *static_cast<const conduit_int32*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_int64))
    {
      catalyst_conduit_node_set_int64(node, *static_cast<const conduit_int64*>(ref_r.get()));
    }
    else
    {
      context().logger().error(
        "Unknown SIGNED buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else if (scalar_kind == PDI::Scalar_kind::UNSIGNED)
  {
    auto buffer_size = scalar_datatype.buffersize();
    if (buffer_size == sizeof(conduit_uint8))
    {
      catalyst_conduit_node_set_uint8(node, *static_cast<const conduit_uint8*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_uint16))
    {
      catalyst_conduit_node_set_uint16(node, *static_cast<const conduit_uint16*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_uint32))
    {
      catalyst_conduit_node_set_uint32(node, *static_cast<const conduit_uint32*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_uint64))
    {
      catalyst_conduit_node_set_uint64(node, *static_cast<const conduit_uint64*>(ref_r.get()));
    }
    else
    {
      context().logger().error(
        "Unknown UNSIGNED buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else if (scalar_kind == PDI::Scalar_kind::FLOAT)
  {
    auto buffer_size = scalar_datatype.buffersize();
    if (buffer_size == sizeof(conduit_float32))
    {
      catalyst_conduit_node_set_float32(node, *static_cast<const conduit_float32*>(ref_r.get()));
    }
    else if (buffer_size == sizeof(conduit_float64))
    {
      catalyst_conduit_node_set_float64(node, *static_cast<const conduit_float64*>(ref_r.get()));
    }
    else
    {
      context().logger().error(
        "Unknown FLOAT buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else
  {
    context().logger().error("Unknown Scalar Type for variable {}", name);
  }
}

void catalyst_plugin::FillNodeWithArrayPDIData(conduit_node* node, const std::string& name,
  PC_tree_t& tree, const PDI::Array_datatype& array_datatype, PDI::Ref_r& ref_r)
{
  PDI::Datatype_sptr type = array_datatype.subtype();
  while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type))
  {
    type = array_type->subtype();
  }
  auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type);
  if (!scalar_datatype)
  {
    context().logger().error("Array subtype of variable {} should be scalar type.", name);
    return;
  }

  conduit_index_t num_elements = 0;
  auto size_spec = PC_get(tree, ".size");
  if (PC_status(size_spec) == PC_OK)
  {
    num_elements = GetLongValueFromSpecNode(size_spec, name);
  }

  conduit_index_t offset = 0;
  auto offset_spec = PC_get(tree, ".offset");
  if (PC_status(offset_spec) == PC_OK)
  {
    offset = GetLongValueFromSpecNode(offset_spec, name);
  }

  conduit_index_t stride = 1;
  auto stride_spec = PC_get(tree, ".stride");
  if (PC_status(stride_spec) == PC_OK)
  {
    stride = GetLongValueFromSpecNode(stride_spec, name);
  }

  conduit_index_t endianness = CONDUIT_ENDIANNESS_DEFAULT_ID;

  PDI::Scalar_kind scalar_kind = scalar_datatype->kind();
  if (scalar_kind == PDI::Scalar_kind::SIGNED)
  {
    auto buffer_size = scalar_datatype->buffersize();
    if (buffer_size == sizeof(conduit_int8))
    {
      conduit_index_t element_bytes = 1;
      auto pointer = const_cast<conduit_int8*>(static_cast<const conduit_int8*>(ref_r.get()));
      catalyst_conduit_node_set_external_int8_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_int16))
    {
      conduit_index_t element_bytes = 2;
      auto pointer = const_cast<conduit_int16*>(static_cast<const conduit_int16*>(ref_r.get()));
      catalyst_conduit_node_set_external_int16_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_int32))
    {
      conduit_index_t element_bytes = 4;
      auto pointer = const_cast<conduit_int32*>(static_cast<const conduit_int32*>(ref_r.get()));
      catalyst_conduit_node_set_external_int32_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_int64))
    {
      conduit_index_t element_bytes = 8;
      auto pointer = const_cast<conduit_int64*>(static_cast<const conduit_int64*>(ref_r.get()));
      catalyst_conduit_node_set_external_int64_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else
    {
      context().logger().error(
        "Unknown SIGNED buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else if (scalar_kind == PDI::Scalar_kind::UNSIGNED)
  {
    auto buffer_size = scalar_datatype->buffersize();
    if (buffer_size == sizeof(conduit_uint8))
    {
      conduit_index_t element_bytes = 1;
      auto pointer = const_cast<conduit_uint8*>(static_cast<const conduit_uint8*>(ref_r.get()));
      catalyst_conduit_node_set_external_uint8_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_uint16))
    {
      conduit_index_t element_bytes = 2;
      auto pointer = const_cast<conduit_uint16*>(static_cast<const conduit_uint16*>(ref_r.get()));
      catalyst_conduit_node_set_external_uint16_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_uint32))
    {
      conduit_index_t element_bytes = 4;
      auto pointer = const_cast<conduit_uint32*>(static_cast<const conduit_uint32*>(ref_r.get()));
      catalyst_conduit_node_set_external_uint32_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_uint64))
    {
      conduit_index_t element_bytes = 8;
      auto pointer = const_cast<conduit_uint64*>(static_cast<const conduit_uint64*>(ref_r.get()));
      catalyst_conduit_node_set_external_uint64_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else
    {
      context().logger().error(
        "Unknown UNSIGNED buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else if (scalar_kind == PDI::Scalar_kind::FLOAT)
  {
    auto buffer_size = scalar_datatype->buffersize();
    if (buffer_size == sizeof(conduit_float32))
    {
      conduit_index_t element_bytes = 4;
      auto pointer = const_cast<conduit_float32*>(static_cast<const conduit_float32*>(ref_r.get()));
      catalyst_conduit_node_set_external_float32_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else if (buffer_size == sizeof(conduit_float64))
    {
      conduit_index_t element_bytes = 8;
      auto pointer = const_cast<conduit_float64*>(static_cast<const conduit_float64*>(ref_r.get()));
      catalyst_conduit_node_set_external_float64_ptr_detailed(node, pointer, num_elements,
        offset * element_bytes, stride * element_bytes, element_bytes, endianness);
    }
    else
    {
      context().logger().error(
        "Unknown FLOAT buffer size of {} for variable {}", buffer_size, name);
    }
  }
  else
  {
    context().logger().error("Unknown Scalar Type for variable {}", name);
  }
}

long catalyst_plugin::GetLongValueFromSpecNode(PC_tree_t& spec, const std::string& name)
{
  if (spec.node->type == YAML_SCALAR_NODE)
  {
    return PDI::to_long(spec);
  }
  else if (spec.node->type == YAML_MAPPING_NODE)
  {
    auto pdi_data_spec = PC_get(spec, ".PDI_data");
    if (PC_status(pdi_data_spec))
    {
      context().logger().error("Unsupported mapping under the variable: {}", name);
      return 0;
    }
    auto name_spec = PC_get(pdi_data_spec, ".name");
    if (PC_status(name_spec))
    {
      context().logger().error("No \"name\" child in PDI_data spec.");
      return 0;
    }

    std::string variable_name = PDI::to_string(name_spec);
    auto it = this->CurrentPDIData.find(variable_name);
    if (it == this->CurrentPDIData.end())
    {
      context().logger().error("Can't find the PDI_data named: {}", variable_name);
    }
    auto ref = it->second;
    PDI::Ref_r ref_r{ ref };
    if (!ref_r)
    {
      context().logger().error("The PDIData named \"{}\" is not readable.", variable_name);
      return 0;
    }

    auto scalar_datatype = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(ref_r.type());
    if (!scalar_datatype)
    {
      context().logger().error(
        "PDI Data subtype of variable {} should be scalar type.", variable_name);
      return 0;
    }
    auto value = ref_r.scalar_value<long>();

    long multiply = 1;
    auto multiply_spec = PC_get(pdi_data_spec, ".multiply");
    if (!PC_status(multiply_spec))
    {
      multiply = PDI::to_long(multiply_spec);
    }
    return value * multiply;
  }
  return 0;
}

std::string catalyst_plugin::ReadPDIExecuteEventName()
{
  std::string eventName;
  auto execute_spec = PC_get(this->SpecTree, ".PDI_execute_event_name");
  if (PC_status(execute_spec) == PC_OK)
  {
    eventName = PDI::to_string(execute_spec);
  }
  return eventName;
}
