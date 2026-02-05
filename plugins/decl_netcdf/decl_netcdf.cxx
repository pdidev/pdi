// SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <vector>

#include <netcdf_meta.h> // includes NC_HAS_PARALLEL4 define
#if NC_HAS_PARALLEL4
#include <mpi.h>
#endif

#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

#include "dnc_file_context.h"

namespace {

class decl_netcdf_plugin: public PDI::Plugin
{
	std::vector<decl_netcdf::Dnc_file_context> m_files;

public:
	decl_netcdf_plugin(PDI::Context& ctx, PC_tree_t config)
		: Plugin(ctx)
	{
		if (PDI::is_list(config)) {
			int len = PDI::len(config);
			// allocate memory for all elements, because Dnc_file has callbacks with their pointers
			m_files.reserve(len);
			for (int i = 0; i < len; i++) {
				m_files.emplace_back(context(), PC_get(config, "[%d]", i));
			}
		} else {
			m_files.emplace_back(context(), config);
		}
		context().logger().info("Plugin loaded successfully");
	}

	~decl_netcdf_plugin() { context().logger().info("Closing plugin"); }

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Decl'NetCDF"; }
};

} // namespace

PDI_PLUGIN(decl_netcdf)
