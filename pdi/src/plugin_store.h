/*******************************************************************************
 * Copyright (C) 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

#ifndef PDI_PLUGIN_STORE_H_
#define PDI_PLUGIN_STORE_H_

#include <map>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "pdi/pdi_fwd.h"
#include "pdi/logger.h"
#include "pdi/plugin.h"

#include "pdi_logger.h"
#include "plugin_logger.h"

namespace PDI {

/** A store used to load a set of plugins
 */
class PDI_NO_EXPORT Plugin_store
{
	/// A factory function for a plugin
	using plugin_factory_f = std::unique_ptr<Plugin> (*)(Logger&, Context&, PC_tree_t);

	/// A function listing the dependencies of a plugin
	using plugin_deps_f = std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> (*)();

	/** The symbols a plugin exports
	 */
	struct Symbols {
		/// The factory function of the plugin
		plugin_factory_f ctr;

		/// The function listing the dependencies of the plugin
		plugin_deps_f deps;

		/// The name of the plugin, as shown in the logs: the one the plugin provides, or the name it is loaded by if it provides none
		std::string pretty_name;
	};

	/** Information about the load state of a plugin
	 */
	class Stored_plugin
	{
		/** A load status of a plugin
		 */
		enum Init_state {
			PRELOADED, ///< the plugin is not initialized yet
			LOADING, ///< the plugin has been visited and its pre-dependencies are being initialized
			LOADED ///< the plugin is already initialized
		};

		/// The config of the plugin
		PC_tree_t m_config;

		/// The factory function of the plugin
		plugin_factory_f m_ctr;

		/// The function listing the dependencies of the plugin
		plugin_deps_f m_deps;

		/// The plugins we depend on to ensure they remain alive as long as we exist
		std::vector<std::shared_ptr<Stored_plugin>> m_dependencies;

		/// The name of this plugin
		std::string m_name;

		/** The logger of the plugin
		 *
		 * The plugin refers to it, so it is declared before the plugin, and thus destroyed after it, to remain valid in the plugin destructor
		 */
		Plugin_logger m_plugin_logger;

		/// The actual plugin, declared after its logger to be destroyed before it
		std::unique_ptr<Plugin> m_loaded_plugin;

		/// Whether this plugin is loaded or not
		Init_state m_state = PRELOADED;

		/** Pre-load a plugin whose symbols are already looked up
		 * \param store the store in which the plugin is loaded
		 * \param name the name of the plugin
		 * \param conf the configuration subtree of the plugin
		 * \param symbols the symbols the plugin exports
		 * \throw Spectree_error if the `logging` subtree of the configuration is invalid
		 */
		Stored_plugin(Plugin_store& store, std::string name, PC_tree_t conf, Symbols symbols);

	public:
		/** Pre-load a plugin
		 * \param store the store in which the plugin is loaded
		 * \param name the name of the plugin
		 * \param conf the configuration subtree of the plugin
		 * \throw Plugin_error if the library of the plugin cannot be loaded or lacks one of its symbols
		 * \throw Spectree_error if the `logging` subtree of the configuration is invalid
		 */
		Stored_plugin(Plugin_store& store, std::string name, PC_tree_t conf);

		/** Load a plugin if not done yet and its pre-dependencies if required
		 * \param store the store in which the plugin is loaded
		 * \param ctx a context to pass to the plugin to load
		 */
		void ensure_loaded(Plugin_store& store, Context& ctx);
	};

	/// The PDI logger, that holds the infrastructure the logger of each plugin uses
	Pdi_logger& m_logger;

	/// The list of default path where to load the plugins
	std::vector<std::string> m_plugin_path;

	/// The list of plugins stored
	std::map<std::string, std::shared_ptr<Stored_plugin>> m_plugins;

	/** initialize m_plugin_path from all its sources
	 * \param confs the config nodes containing plugin_path
	 */
	void initialize_path(std::vector<PC_tree_t> const & confs);

	/** Look up the symbols a plugin exports, loading its library if its constructor or dependencies are not linked in: from m_plugin_path, from
	 * the default plugin path, or from the system path
	 * \param name the name of the plugin
	 * \return the symbols of the plugin
	 * \throw Plugin_error if the library cannot be loaded or lacks the constructor or the dependencies of the plugin
	 */
	Symbols plugin_dlopen(const std::string& name);

public:
	/** Builds a plugin store
	 * \param logger the PDI logger, that holds the infrastructure the logger of each plugin uses
	 */
	Plugin_store(Pdi_logger& logger);

	/** Actually load the plugins
	 *
	 * \param ctx the context to pass to the plugins
	 * \param confs the configurations specifying the list of plugin paths & plugins to load
	 */
	void load_plugins(Context& ctx, std::vector<PC_tree_t> const & confs);
};

} // namespace PDI

#endif // PDI_PLUGIN_STORE_H_
