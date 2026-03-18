/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include "config.h"

#include <cstdlib>
#include <functional>
#include <map>
#include <memory>
#include <vector>

#include <dlfcn.h>
#include <unistd.h>

#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/version.h"

#include "data_descriptor_impl.h"

#include "global_context.h"


using std::exception;
using std::forward_as_tuple;
using std::map;
using std::pair;
using std::piecewise_construct;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

namespace PDI {

namespace {

void load_data(Context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);

	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = ctx.desc(to_string(PC_get(node, "{%d}", map_id)).c_str());
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}
	if (is_metadata) {
		ctx.logger().trace("Loaded {} metadata", map_len);
	} else {
		ctx.logger().trace("Loaded {} data", map_len);
	}
}

} // namespace

unique_ptr<Global_context> Global_context::s_context;

void Global_context::init(PC_tree_t conf)
{
	s_context.reset(new Global_context(conf));
}

bool Global_context::initialized()
{
	return static_cast<bool>(s_context);
}

Global_context& Global_context::context()
{
	if (!s_context) throw State_error{"PDI not initialized"};
	return *s_context;
}

void Global_context::finalize()
{
	s_context.reset();
}

// //
static void print_indent(int indent)
{
    for(int i = 0; i < indent; i++)
        printf("  ");
}

static const char* yaml_node_type_str(yaml_node_type_t t)
{
    switch(t)
    {
        case YAML_NO_NODE: return "YAML_NO_NODE";
        case YAML_SCALAR_NODE: return "YAML_SCALAR_NODE";
        case YAML_SEQUENCE_NODE: return "YAML_SEQUENCE_NODE";
        case YAML_MAPPING_NODE: return "YAML_MAPPING_NODE";
        default: return "INVALID_NODE_TYPE";
    }
}

static void print_node_ptr(yaml_document_t *doc, yaml_node_t *node, int indent)
{
    if(!node)
        return;

    switch(node->type)
    {
        case YAML_SCALAR_NODE:
            printf("%s", (char*)node->data.scalar.value);
            break;

        case YAML_SEQUENCE_NODE:
        {
            yaml_node_item_t *item = node->data.sequence.items.start;

            while(item < node->data.sequence.items.top)
            {
                yaml_node_t *child = yaml_document_get_node(doc, *item);

                print_indent(indent);
                printf("- ");

                if(child->type == YAML_SCALAR_NODE)
                {
                    printf("%s\n", (char*)child->data.scalar.value);
                }
                else if(child->type == YAML_MAPPING_NODE)
                {
                    printf("\n");
                    print_node_ptr(doc, child, indent + 1);
                }
                else
                {
                    printf("\n");
                    print_node_ptr(doc, child, indent + 1);
                }

                item++;
            }

            break;
        }

        case YAML_MAPPING_NODE:
        {
            yaml_node_pair_t *pair = node->data.mapping.pairs.start;

            while(pair < node->data.mapping.pairs.top)
            {
                yaml_node_t *key = yaml_document_get_node(doc, pair->key);
                yaml_node_t *val = yaml_document_get_node(doc, pair->value);

                print_indent(indent);
                printf("%s:", (char*)key->data.scalar.value);

                if(val->type == YAML_SCALAR_NODE)
                {
                    printf(" %s\n", (char*)val->data.scalar.value);
                }
                else
                {
                    printf("\n");
                    print_node_ptr(doc, val, indent + 1);
                }

                pair++;
            }

            break;
        }

        default:
        {
            const char *tag = node->tag ? (char*)node->tag : "(none)";

            fprintf(stderr,
                "\nYAML ERROR: unknown node type\n"
                "  type: %s (%d)\n"
                "  tag : %s\n"
                "  indent level: %d\n",
                yaml_node_type_str(node->type),
                node->type,
                tag,
                indent
            );

            abort();
        }
    }
}
// //

void PC_debug_print(PC_tree_t tree)
{
    if(!tree.document)
    {
        printf("No YAML document\n");
        return;
    }

    if(!tree.node)
    {
        printf("No YAML root node\n");
        return;
    }

    print_node_ptr(tree.document, tree.node, 0);

    printf("\n");
}
// //

void load_pdi(Global_context& ctx, PC_tree_t conf)
{
	ctx.logger().debug("A load_pdi");

	PC_tree_t root = conf;
	// si on a un noeud ".pdi", on descend dedans, sinon on reste à ce niveau (pour le yaml racine, ou pour un yaml inclus)
	PC_tree_t pdi = PC_get(conf, ".pdi");
	if (!PC_status(pdi)) {
		root = pdi;
	}

	PC_tree_t includes = PC_get(root, ".include");
	if (!PC_status(includes)) {
		PDI::each(includes, [&](PC_tree_t inc){
			load_pdi(ctx, PC_parse_path((PDI::to_string(inc)).c_str()));
		});
		// ctx.logger().warn("Includes are defined in specification tree");
		ctx.logger().debug("Includes are defined in specification tree IN");
	} else {
		// ctx.logger().warn("Includes are not defined in specification tree");
		ctx.logger().debug("Includes are not defined in specification tree IN ELSE");
	}

	// load user datatypes
	Datatype_template::load_user_datatypes(ctx, PC_get(root, ".types"));

	// no metadata is not an error
	PC_tree_t metadata = PC_get(root, ".metadata");
	if (!PC_status(metadata)) {
		load_data(ctx, metadata, true);
	} else {
		// ctx.logger().warn("Metadata is not defined in specification tree IN");
		ctx.logger().debug("Metadata is not defined in specification tree IN");
	}

	// no data is spurious, but not an error
	PC_tree_t data = PC_get(root, ".data");
	if (!PC_status(data)) {
		load_data(ctx, data, false);
	} else {
		// ctx.logger().warn("Data is not defined in specification tree IN");
		ctx.logger().debug("Data is not defined in specification tree IN");
	}
	ctx.logger().debug("Out of a load_pdi");
}

Global_context::Global_context(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_plugins{*this, conf}
	, m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);

	m_plugins.load_plugins();

	load_pdi(*this, conf);

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(*this);


	m_callbacks.call_init_callbacks();
	m_logger.info("Initialization successful");
}

Data_descriptor& Global_context::desc(const char* name)
{
	return *(m_descriptors.emplace(name, unique_ptr<Data_descriptor>{new Data_descriptor_impl{*this, name}}).first->second);
}

Data_descriptor& Global_context::desc(const string& name)
{
	return desc(name.c_str());
}

Data_descriptor& Global_context::operator[] (const char* name)
{
	return desc(name);
}

Data_descriptor& Global_context::operator[] (const string& name)
{
	return desc(name.c_str());
}

Global_context::Iterator Global_context::begin()
{
	return Context::get_iterator(m_descriptors.begin());
}

Global_context::Iterator Global_context::end()
{
	return Context::get_iterator(m_descriptors.end());
}

PDI::Context::Iterator Global_context::find(const string& name)
{
	return Context::get_iterator(m_descriptors.find(name));
}

void Global_context::event(const char* name)
{
	m_callbacks.call_event_callbacks(name);
}

Logger& Global_context::logger()
{
	return m_logger;
}

Datatype_template_sptr Global_context::datatype(PC_tree_t node)
{
	string type;
	try {
		type = to_string(PC_get(node, ".type"));
	} catch (const Error& e) {
		type = to_string(node);
	}

	// check if someone didn't mean to create an array with the old syntax
	if (type != "array") {
		if (!PC_status(PC_get(node, ".size"))) {
			logger().warn("In line {}: Non-array type with a `size' property", node.node->start_mark.line);
		}
		if (!PC_status(PC_get(node, ".sizes"))) {
			logger().warn("In line {}: Non-array type with a `sizes' property", node.node->start_mark.line);
		}
	}

	auto&& func_it = m_datatype_parsers.find(type);
	if (func_it != m_datatype_parsers.end()) {
		return (func_it->second)(*this, node);
	}
	throw Config_error{node, "Unknown data type: `{}'", type};
}

void Global_context::add_datatype(const string& name, Datatype_template_parser parser)
{
	if (!m_datatype_parsers.emplace(name, move(parser)).second) {
		//if a datatype with the given name already exists
		throw Type_error{"Datatype already defined `{}'", name};
	}
}

Callbacks& Global_context::callbacks()
{
	return m_callbacks;
}

void Global_context::finalize_and_exit()
{
	Global_context::finalize();
	exit(0);
}

Global_context::~Global_context()
{
	m_logger.info("Finalization");
}

} // namespace PDI
