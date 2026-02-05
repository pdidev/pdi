/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_PLUGIN_LOADER_H_
#define PDI_PLUGIN_LOADER_H_

#include <map>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <pdi/plugin.h>

namespace PDI {

/** A store used to load a set of plugins
 */
class Plugin_store
{
	/// A factory function for a plugin
	using plugin_factory_f = std::unique_ptr<Plugin> (*)(Context&, PC_tree_t);

	/// A function listing the dependencies of a plugin
	using plugin_deps_f = std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> (*)();

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

		/// A context for the plugin
		Context_proxy m_ctx;

		/// The function listing the dependencies of the plugin
		plugin_deps_f m_deps;

		/// The plugins we depend on to ensure they remain alive as long as we exist
		std::vector<std::shared_ptr<Stored_plugin>> m_dependencies;

		/// The actual plugin, this must remain after the context & handle to guarantee correct destruction order
		std::unique_ptr<Plugin> m_loaded_plugin;

		/// The name of this plugin
		std::string m_name;

		/// Whether this plugin is loaded or not
		Init_state m_state;

	public:
		/** Pre-loads a plugin
		 * \param ctx the context
		 * \param store the store in which this plugin is loaded
		 * \param name the name of the plugin
		 * \param conf the configuration subtree of the plugin
		 */
		Stored_plugin(Context& ctx, Plugin_store& store, std::string name, PC_tree_t conf);

		/** Loads a plugin if not done yet and its pre-dependencies if required
		 * \param plugins the list of all plugins (for dependencies)
		 */
		void ensure_loaded(std::map<std::string, std::shared_ptr<Stored_plugin>>& plugins);
	};

	/// The context for logging & co.
	Context& m_ctx;

	/// The list of default path where to load the plugins
	std::vector<std::string> m_plugin_path;

	/// The list of plugins stored
	std::map<std::string, std::shared_ptr<Stored_plugin>> m_plugins;

	/** initialize m_plugin_path from all its sources
	 * \param plugin_path_node the plugin_path node of the config
	 */
	void initialize_path(PC_tree_t plugin_path_node);

	/** dlopen's a plugin taking m_plugin_path into account
	 * \param name the name of the plugin
	 * \return a handle for the loaded object, same as dlopen
	 */
	void* plugin_dlopen(const std::string& name);

public:
	/** Builds a plugin store
	 * \param ctx the context in which to load the plugins
	 * \param conf the configuration specifying the plugin path & list of plugins to load
	 */
	Plugin_store(Context& ctx, PC_tree_t conf);

	/** Actually load the plugins
	 */
	void load_plugins();
};

} // namespace PDI

#endif // PDI_PLUGIN_LOADER_H_
