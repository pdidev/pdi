/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_PLUGIN_H_
#define PDI_PLUGIN_H_

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>

#include <pdi/pdi_fwd.h>
#include <pdi/logger.h>
#include <pdi/version.h>

namespace PDI {


/** The class PDI plugins should implement
 */
class PDI_EXPORT Plugin
{
	Context& m_context;

	Logger& m_logger;

public:
	Plugin(const Plugin&) = delete;

	Plugin(Plugin&&) = delete;

	/** Initialization of the plugin
	 * \param logger the logger for this plugin instance
	 * \param ctx the PDI context for this plugin instance
	 */
	Plugin(Logger& logger, Context& ctx);

	virtual ~Plugin() noexcept(false);

protected:
	/** Provides access to the PDI context for this plugin instance
	 */
	Context& context();

	/** Provides access to the logger for this plugin instance
	 */
	Logger& logger();

}; // class Plugin

/** Major revision number of the plugin API
 *
 * The plugin API is not stable yet: it stays at 0, and every minor release of PDI may break it.
 */
#define PDI_PLUGIN_API_VERSION_MAJOR 0

/// Minor revision number of the plugin API: it changes with every major or minor release of PDI, and has to match exactly while the major is 0
#define PDI_PLUGIN_API_VERSION_MINOR (PDI_VERSION_MAJOR * 128 + PDI_VERSION_MINOR)

/// Patch revision number of the plugin API: the patch revision number of PDI
#define PDI_PLUGIN_API_VERSION_PATCH PDI_VERSION_PATCH

/** Computes a version of the plugin API as a uint64_t, comparable with PDI_PLUGIN_API_VERSION and with what plugin_api_version() returns
 *
 * The result is usable in a preprocessor `#if` as well as in code.
 * Bits 48 to 63 hold the major revision number, bits 32 to 47 the minor one and bits 16 to 31 the patch one.
 */
#define PDI_COMPUTE_PLUGIN_API_VERSION(MAJOR, MINOR, PATCH)                                                                                          \
	(((UINT64_C(0) + (MAJOR)) << 48) | ((UINT64_C(0) + (MINOR)) << 32) | ((UINT64_C(0) + (PATCH)) << 16))

/// The version of the plugin API of the PDI headers as a uint64_t, as PDI_COMPUTE_PLUGIN_API_VERSION computes it
#define PDI_PLUGIN_API_VERSION                                                                                                                       \
	PDI_COMPUTE_PLUGIN_API_VERSION(PDI_PLUGIN_API_VERSION_MAJOR, PDI_PLUGIN_API_VERSION_MINOR, PDI_PLUGIN_API_VERSION_PATCH)

/// \deprecated use PDI_PLUGIN_API_VERSION_MAJOR instead
#define PLUGIN_API_VERSION_MAJOR PDI_PLUGIN_API_VERSION_MAJOR

/// \deprecated use PDI_PLUGIN_API_VERSION_MINOR instead
#define PLUGIN_API_VERSION_MINOR PDI_PLUGIN_API_VERSION_MINOR

/// \deprecated use PDI_PLUGIN_API_VERSION_PATCH instead
#define PLUGIN_API_VERSION_PATCH PDI_PLUGIN_API_VERSION_PATCH

/// \deprecated use PDI_PLUGIN_API_VERSION instead
#define PLUGIN_API_VERSION PDI_PLUGIN_API_VERSION

// GCC has no way to deprecate a macro that works in `#if` too, Clang does
#if defined(__clang__) && __clang_major__ >= 14
#pragma clang deprecated(PLUGIN_API_VERSION_MAJOR, "use PDI_PLUGIN_API_VERSION_MAJOR instead")
#pragma clang deprecated(PLUGIN_API_VERSION_MINOR, "use PDI_PLUGIN_API_VERSION_MINOR instead")
#pragma clang deprecated(PLUGIN_API_VERSION_PATCH, "use PDI_PLUGIN_API_VERSION_PATCH instead")
#pragma clang deprecated(PLUGIN_API_VERSION, "use PDI_PLUGIN_API_VERSION instead")
#endif

/** Checks compatibility with a plugin API
 *
 * While the major revision number is 0, the expected version is compatible with the provided one if both have the same minor revision number.
 * From 1 on, it is compatible if both have the same major revision number and the expected minor revision number is not greater than the provided
 * one.
 *
 * \param expected_version the expected version of the API or 0 for no check
 * \returns the version of the API provided by PDI
 * \throws PDI::Error if the provided version is incompatible with the expected
 * one
 */
uint64_t PDI_EXPORT plugin_api_version(uint64_t expected_version = 0);

} // namespace PDI

#if defined(__clang__)
/** Silence the warnings Clang emits about the functions PDI_PLUGIN defines
 *
 * The loading functions are looked up by name, so they have no prototype, and they have C linkage while returning C++ types.
 * GCC emits neither warning, and it warns about the `clang diagnostic` pragmas, hence a definition for Clang only.
 */
#define PDI_PLUGIN_IGNORE_WARNINGS_BEGIN                                                                                                             \
	_Pragma("clang diagnostic push") _Pragma("clang diagnostic ignored \"-Wmissing-prototypes\"")                                                    \
		_Pragma("clang diagnostic ignored \"-Wreturn-type-c-linkage\"")

/// Restore the warnings PDI_PLUGIN_IGNORE_WARNINGS_BEGIN silenced
#define PDI_PLUGIN_IGNORE_WARNINGS_END _Pragma("clang diagnostic pop")
#else
#define PDI_PLUGIN_IGNORE_WARNINGS_BEGIN
#define PDI_PLUGIN_IGNORE_WARNINGS_END
#endif

/** Declares a plugin to be used with PDI and its dependencies
 *
 * This should be called after having implemented a class that inherits PDI::Plugin with a constructor taking 3 parameters
 * - PDI::Logger& logger: the logger for this plugin (forward it to PDI::Plugin)
 * - PDI::Context& ctx: the context for this plugin (forward it to PDI::Plugin)
 * - PC_tree_t conf: the configuration for this plugin
 *
 * The name of the class should be NAME_plugin where NAME is the plugin name
 *
 * If the plugin has any dependencies, they should be defined in a static method
 * named dependencies, which returns std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>>
 * and takes no parameters.
 * First unordered_set in a pair is required plugins (i.e plugins that must be loaded with the plugin).
 * Second unodrered_set is dependent plugins (i.e plugins that need to be loaded before the plugin, if they are both loaded).
 *
 * \param name the name of the plugin
 */
#define PDI_PLUGIN(name)                                                                                                                             \
	PDI_PLUGIN_IGNORE_WARNINGS_BEGIN extern "C" ::std::unique_ptr<::PDI::Plugin> PDI_EXPORT PDI_plugin_##name##_loader(                              \
		::PDI::Logger& logger,                                                                                                                       \
		::PDI::Context& ctx,                                                                                                                         \
		PC_tree_t conf                                                                                                                               \
	)                                                                                                                                                \
	{                                                                                                                                                \
		auto plugin = ::std::unique_ptr<name##_plugin>{new name##_plugin{logger, ctx, conf}};                                                        \
		::PDI::plugin_api_version(PDI_PLUGIN_API_VERSION);                                                                                           \
		return plugin;                                                                                                                               \
	}                                                                                                                                                \
	extern "C" ::std::pair<::std::unordered_set<::std::string>, ::std::unordered_set<::std::string>> PDI_EXPORT PDI_plugin_##name##_dependencies()   \
	{                                                                                                                                                \
		return []<class P>() {                                                                                                                       \
			if constexpr (requires { P::dependencies(); }) {                                                                                         \
				return P::dependencies();                                                                                                            \
			} else {                                                                                                                                 \
				return ::std::pair<::std::unordered_set<::std::string>, ::std::unordered_set<::std::string>>{};                                      \
			}                                                                                                                                        \
		}.template operator()<name##_plugin>();                                                                                                      \
	}                                                                                                                                                \
	extern "C" ::std::string PDI_EXPORT PDI_plugin_##name##_pretty_name()                                                                            \
	{                                                                                                                                                \
		return []<class P>(const char* fallback) -> ::std::string {                                                                                  \
			if constexpr (requires { P::pretty_name(); }) {                                                                                          \
				return P::pretty_name();                                                                                                             \
			} else {                                                                                                                                 \
				return fallback;                                                                                                                     \
			}                                                                                                                                        \
		}.template operator()<name##_plugin>(#name);                                                                                                 \
	}                                                                                                                                                \
	PDI_PLUGIN_IGNORE_WARNINGS_END

#endif // PDI_PLUGIN_H_
