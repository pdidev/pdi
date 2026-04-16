/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_GLOBAL_CONTEXT_H_
#define PDI_GLOBAL_CONTEXT_H_

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "pdi/pdi_fwd.h"
#include "pdi/callbacks.h"
#include "pdi/context.h"
#include "pdi/context_proxy.h"
#include "pdi/data_descriptor.h"
#include "pdi/logger.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"

#include "plugin_store.h"

namespace PDI {

class PDI_EXPORT Global_context: public Context
{
private:
	friend class Data_descriptor_impl;

	/// The singleton Context instance
	static std::unique_ptr<Global_context> s_context;

	/// Global logger of PDI, should be constructed first, destroyed last
	Logger m_logger;

	/// Datatype_template constructors available in PDI
	std::unordered_map<std::string, Datatype_template_parser> m_datatype_parsers;

	/// Descriptors of the data
	std::unordered_map<std::string, std::unique_ptr<Data_descriptor>> m_descriptors;

	/// The plugins, this should be late in the list to be destroyed early
	Plugin_store m_plugins;

	/// Callbacks of the context
	Callbacks m_callbacks;

	Global_context(const Global_context&) = delete;

	Global_context(Global_context&&) = delete;

	/// Private definition of collect_ordered_nodes to handle local no_path_counter
	void collect_ordered_nodes_impl(
		PC_tree_t conf,
		std::unordered_set<std::string>& globally_loaded,
		std::unordered_set<std::string>& include_chain,
		std::vector<std::pair<std::string, PC_tree_t>>& ordered_nodes,
		std::size_t& no_path_counter,
		const std::string& known_path = "",
		PC_tree_t include_directive = {},
		const std::string& parent_id = ""
	);

public:
	static void init(PC_tree_t conf);

	static bool initialized();

	static Global_context& context();

	static void finalize();

	Global_context(PC_tree_t conf);

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const std::string& name) override;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const char* name) override;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[] (const std::string& name) override;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[] (const char* name) override;

	/** Returns an iterator on the first descriptor
	 */
	Iterator begin() override;

	/** Returns an iterator past the last descriptor
	 */
	Iterator end() override;

	/** Returns the iterator that corresponds to the provided Data_descriptor name
	 */
	Iterator find(const std::string& name) override;

	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	void event(const char* name) override;

	Logger& logger() override;

	Datatype_template_sptr datatype(PC_tree_t node) override;

	void add_datatype(const std::string&, Datatype_template_parser) override;

	Callbacks& callbacks() override;

	/// Traverses the include tree once in post-order (deepest includes first),
	/// filling `ordered_nodes` with (canonical_id, PC_tree_t) pairs.
	/// Diamond/circular detection is handled here.
	void collect_ordered_nodes(
		PC_tree_t conf,
		std::unordered_set<std::string>& globally_loaded,
		std::unordered_set<std::string>& include_chain,
		std::vector<std::pair<std::string, PC_tree_t>>& ordered_nodes,
		const std::string& known_path = "",
		PC_tree_t include_directive = {},
		const std::string& parent_id = ""
	);

	void finalize_and_exit() override;

	void load_pdi_config(PC_tree_t conf);

	/// Creates a descriptor for `key_node`, throwing Config_error if already defined.
	Data_descriptor& make_and_check_descriptor(PC_tree_t key_node);

	~Global_context() override;
};

} // namespace PDI

#endif // PDI_GLOBAL_CONTEXT_H_
