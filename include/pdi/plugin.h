/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

/** \file pdi/plugin.h
 * Main plugin API
 */

#ifndef PDI_PLUGIN_H_
#define PDI_PLUGIN_H_

#include <pdi/logger.h>
#include <pdi/pdi_fwd.h>


namespace PDI {

class PDI_EXPORT Plugin
{
	Context& m_context;

protected:
	/// Global logger of PDI
	Logger m_logger {spdlog::get("logger")};
	
public:
	Plugin(const Plugin&) = delete;
	
	Plugin(Plugin&&) = delete;
	
	/** Initialization of the plugin
	 * \param ctx the PDI context for this plugin instance
	 */
	Plugin(Context& ctx);
	
	virtual ~Plugin() noexcept(false);
	
	/** Notification for a named event
	 * \param[in] event the event name
	 */
	virtual void event(const char* event);
	
	/** Notification that some data becomes available
	 * \param name the name of the data made available
	 * \param ref a reference to the data value
	 */
	virtual void data(const char* name, Ref ref);
	
	/** Provides access to the PDI context for this plugin instance
	 */
	Context& context();
	
}; // class Plugin


#define PLUGIN_API_VERSION_MAJOR (0ul)

#define PLUGIN_API_VERSION_MINOR (0ul)

#define PLUGIN_API_VERSION_PATCH (1ul)

#define PLUGIN_API_VERSION ((PLUGIN_API_VERSION_MAJOR<<24) + (PLUGIN_API_VERSION_MINOR<<16) + (PLUGIN_API_VERSION_PATCH<<8))

/** Checks compatibility with a plugin API
 *
 * \param expected_version the expected version of the API or 0 for no check
 * \returns the version of the API provided by PDI
 * \throws PDI::Error if the provided version is incompatible with the expected
 * one
 */
unsigned long PDI_EXPORT plugin_api_version(unsigned long expected_version=0);

} // namespace PDI

/** Declares a plugin to be used with PDI
 *
 * This should be called after having implemented a class that inherits
 * PDI::Plugin with a constructor taking 3 parameters
 * - PDI::Context& ctx: the context for this plugin (forward it to PDI::Plugin)
 * - PC_tree_t conf: the configuration for this plugin
 * - MPI_Comm *world: a MPI communicator from which this plugin can reserve ranks
 *
 * The name of the class should be NAME_plugin where NAME is the plugin name
 *
 * \param name the name of the plugin
 */
#define PDI_PLUGIN(name)\
	_Pragma("clang diagnostic push")\
	_Pragma("clang diagnostic ignored \"-Wmissing-prototypes\"")\
	_Pragma("clang diagnostic ignored \"-Wreturn-type-c-linkage\"")\
	extern "C" ::std::unique_ptr<::PDI::Plugin> PDI_EXPORT PDI_plugin_##name##_loader(::PDI::Context& ctx, PC_tree_t conf, MPI_Comm *world) \
	{\
		auto plugin = ::std::unique_ptr<name##_plugin>{new name##_plugin{ctx, conf, world}};\
		::PDI::plugin_api_version(PLUGIN_API_VERSION);\
		return plugin;\
	}\
	_Pragma("clang diagnostic pop")

#endif // PDI_PLUGIN_H_
