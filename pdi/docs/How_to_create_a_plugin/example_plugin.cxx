/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the names of CEA, nor the names of the contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/

/// \file
/// The plugin that the "How to create a plugin" documentation builds up step by
/// step.
/// Every step is a snippet of this one file: they all describe the same
/// `example' plugin, so that the whole tutorial compiles as a single plugin
/// rather than as a handful of files that each repeat the same skeleton.

//! [includes]
#include <string>
#include <unordered_map>
#include <vector>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
//! [includes]

/// The specification tree this plugin reads.
/// The tutorial shows it in pieces, one per configuration-reading step.
const char* CONFIG_YAML =
	R"PDIYAML(
#! [yaml]
plugins:
  example:
    scalar: some_string
    array: [0, 1, 2]
    custom_subtree:
      here: 0
      can: 1
      be: 2
      any: 3
      subtree: 4
#! [yaml]
)PDIYAML";

namespace {

struct example_plugin: PDI::Plugin {
	example_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		//! [on_data]
		ctx.on_data([this](const std::string& data_name, PDI::Ref ref) {
			this->context().logger().info("User has shared a data named {}", data_name);
		});
		//! [on_data]

		//! [rw_data]
		ctx.on_data([](const std::string& data_name, PDI::Ref ref) {
			if (PDI::Ref_rw ref_rw{ref}) {
				// the plugin can read and write
				int* some_integer = static_cast<int*>(ref_rw.get());
			} else if (PDI::Ref_r ref_r{ref}) {
				// the plugin can only read
				const int* some_integer = static_cast<const int*>(ref_r.get());
			} else if (PDI::Ref_w ref_w{ref}) {
				// the plugin can only write
				int* some_integer = static_cast<int*>(ref_w.get());
			} else {
				// the plugin can neither read nor write
			}
		});
		//! [rw_data]

		//! [events]
		ctx.on_event([this](const std::string& event_name) { this->handle_event(event_name); });
		ctx.on_event([this](const std::string& event_name) { this->handle_special_event(event_name); }, "special_event");
		//! [events]

		//! [conf_scalar_array]
		// scalar
		PC_tree_t scalar_tree = PC_get(spec_tree, ".scalar");
		std::string string_scalar = PDI::to_string(scalar_tree);

		// array
		PC_tree_t array_tree = PC_get(spec_tree, ".array");
		int array_size = PDI::len(array_tree);
		std::vector<long> array;
		for (int i = 0; i < array_size; i++) {
			PC_tree_t array_element = PC_get(array_tree, "[%d]", i);
			array.emplace_back(PDI::to_long(array_element));
		}
		//! [conf_scalar_array]

		//! [conf_map]
		PC_tree_t subtree = PC_get(spec_tree, ".custom_subtree");
		int subtree_size = PDI::len(subtree);
		std::unordered_map<std::string, long> custom_map;

		for (int i = 0; i < subtree_size; i++) {
			PC_tree_t key = PC_get(subtree, "{%d}", i);
			PC_tree_t value = PC_get(subtree, "<%d>", i);
			custom_map.emplace(PDI::to_string(key), PDI::to_long(value));
		}
		//! [conf_map]
	}

private:
	//! [event_handlers]
	void handle_event(const std::string& event_name) { context().logger().info("Event {} called.", event_name); }

	void handle_special_event(const std::string& event_name) { context().logger().info("Special event `{}' called.", event_name); }

	//! [event_handlers]
};

} // namespace

//! [loader]
PDI_PLUGIN(example)
//! [loader]
