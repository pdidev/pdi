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

#include "config.h"


#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <functional>
#include <map>
#include <memory>
#include <stdexcept>
#include <unordered_set>
#include <vector>

#include <dlfcn.h>
#include <unistd.h>

#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/version.h"

#include "pdi_instance.h"

namespace fs = std::filesystem;

using std::cref;
using std::exception;
using std::forward_as_tuple;
using std::function;
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

/** A class that represents a path to include a yaml subtree, i.e. both the path of the file itself and the subtree in
 * the file.
 */
class Include_path
{
	/// The path of the file
	fs::path m_file_path;

	/// The path of the subtree inside the file
	std::string m_ypath;

public:
	/** Builds an Include_path from a PC_tree_t
	 * \param include_directive either a scalar file path or a mapping with `file` and `subtree` keys
	 */
	Include_path(PC_tree_t include_directive)
	{
		if (is_map(include_directive)) {
			bool found_file = false;
			bool found_subtree = false;
			each(include_directive, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
				std::string key = PDI::to_string(key_tree);
				std::string value = PDI::to_string(value_tree);
				if (key == "file") {
					m_file_path = value;
					found_file = true;
				} else if (key == "subtree") {
					m_ypath = value;
					found_subtree = true;
				} else {
					throw Spectree_error(key_tree, "unexpected key in include directive: `{}', only `file' and `subtree' allowed", key);
				}
			});
			if (!found_file) {
				throw Spectree_error(include_directive, "missing `'file` key in include directive");
			}
			if (!found_subtree) {
				throw Spectree_error(include_directive, "missing `'subtree` key in include directive");
			}
		} else {
			m_file_path = PDI::to_string(include_directive);
		}
	}

	/** Returns the path of the file
	 * \returns the path of the file
	 */
	const fs::path& file_path() const { return m_file_path; }

	/** Returns the path of the subtree inside the file
	 * \returns the path of the subtree inside the file
	 */
	const std::string& ypath() const { return m_ypath; }

	// define the operator '<', '=' and '>'
	auto operator<=> (const Include_path&) const = default;

	/** Converts the path to a string representation
	 * \returns a string representation of the path
	 */
	std::string to_string() const { return fmt::format("yaml://{}{}{}", file_path().string(), ((ypath() != "") ? "#" : ""), ypath()); }

	/** Loads the subtree identified by this path
	*/
	PC_tree_t pc_tree() const
	{
		PC_tree_t result = PC_parse_path(file_path().string().c_str());
		if (PC_status(result)) {
			throw System_error("Unable to include file `{}': {}", file_path().string(), PC_errmsg());
		}
		result = PC_get(result, ypath().c_str());
		if (PC_status(result)) {
			throw System_error("Unable to include subtree `{}' from file `{}': {}", ypath(), file_path().string(), PC_errmsg());
		}
		return result;
	}
};


} // namespace
} // namespace PDI

namespace std {
template <>
struct hash<PDI::Include_path> {
	std::size_t operator() (const PDI::Include_path& path) const
	{
		// Computes the hash of an inc_path using boost strategy
		std::size_t result = std::hash<fs::path>()(path.file_path());
		result ^= std::hash<std::string>()(path.ypath()) + 0x9e3779b9 + (result << 6) + (result >> 2);
		return result;
	}
};
} // namespace std

namespace PDI {
namespace {

/** Gather the files included by the provided configuration.
 * 
 * The result is as an ordered list where elements at the end of the list can depend on those coming before.
 * 
 * \param logger a logger
 * \param conf the configuration where to look for included files
 * \param parents the set of subtree path that are in the include chain of conf (including conf)
 * \param result_path the path of all files already in result
 * \param result the ordered list of (transitively) included files to which conf and its requirements will be added
 */
void get_includes(
	Logger& logger,
	PC_tree_t conf,
	std::unordered_set<Include_path>& parents,
	std::unordered_set<Include_path>& result_path,
	std::vector<PC_tree_t>& result
)
{
	PC_tree_t inc_tree = PC_get(conf, ".include");
	if (!PC_status(inc_tree)) {
		opt_each(inc_tree, [&](PC_tree_t include_directive) {
			Include_path subconf_path{include_directive};
			if (parents.contains(subconf_path)) {
				// if we are in the include chain, this is a recursive include and an error
				throw Spectree_error(include_directive, "Circular include of `({}){}'", subconf_path.file_path().string(), subconf_path.ypath());
			}
			if (result_path.contains(subconf_path)) return; // if we were already included, nothing to do
			parents.emplace(subconf_path);
			try {
				logger.trace("Including {}", subconf_path.to_string());
				get_includes(logger, subconf_path.pc_tree(), parents, result_path, result);
			} catch (const Spectree_error& e) {
				rethrow_with_context(std::current_exception(), "included from ({}){}", subconf_path.file_path().string(), subconf_path.ypath());
			}
			parents.erase(subconf_path);
			result_path.emplace(subconf_path);
		});
	}
	result.emplace_back(conf);
}

/** Gather the files included by the provided configuration.
 * 
 * Returns the result as an ordered list where elements at the end of the list can depend on those coming before.
 * 
 * \param logger a logger
 * \param conf the configuration where to look for included files
 *
 * \returns the ordered list of (transitively) included confs, including `conf`
 */
std::vector<PC_tree_t> get_includes(Logger& logger, PC_tree_t conf)
{
	std::unordered_set<Include_path> parents;
	std::unordered_set<Include_path> result_path;
	std::vector<PC_tree_t> result;
	get_includes(logger, conf, parents, result_path, result);
	return result;
}

} // namespace

std::unique_ptr<Pdi_instance, void (*)(Pdi_instance*)> Pdi_instance::s_instance(nullptr, [](Pdi_instance* i) {
	if (i) delete i;
});

void Pdi_instance::init(PC_tree_t conf)
{
	s_instance.reset(new Pdi_instance(conf));
}

bool Pdi_instance::initialized()
{
	return static_cast<bool>(s_instance);
}

Pdi_instance& Pdi_instance::instance()
{
	if (!s_instance) throw State_error{"PDI not initialized"};
	return *s_instance;
}

void Pdi_instance::finalize()
{
	s_instance.reset();
}

Pdi_instance::Pdi_instance(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_data_store{m_logger}
	, m_plugins{m_logger}
{
	// Handle includes and gather all files
	std::vector<PC_tree_t> confs = get_includes(m_logger, conf);

	// load basic datatypes
	Datatype_template::load_basic_datatypes(m_data_store);
	// load user datatypes
	for (auto&& conf: confs) {
		Datatype_template::load_user_datatypes(m_data_store, PC_get(conf, ".types"));
	}

	m_plugins.load_plugins(m_data_store, confs);

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(m_data_store);
	
	m_data_store.configure(confs);

	m_logger.info("Initialization successful");
}

Pdi_instance::~Pdi_instance()
{
	m_logger.info("Finalization");
}

Global_context& Pdi_instance::data_store()
{
	return m_data_store;
}

Logger& Pdi_instance::logger()
{
	return m_logger;
}

} // namespace PDI
