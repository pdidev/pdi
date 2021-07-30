/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <vector>

#include <netcdf_meta.h> // includes NC_HAS_PARALLEL4 define
#if NC_HAS_PARALLEL4
	#include <mpi.h>
#endif

#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

#include "dnc_file_context.h"

namespace {

class decl_netcdf_plugin : public PDI::Plugin
{
	std::vector<decl_netcdf::Dnc_file_context> m_files;
	
public:
	decl_netcdf_plugin(PDI::Context& ctx, PC_tree_t config):
		Plugin(ctx)
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
	
	~decl_netcdf_plugin()
	{
		context().logger().info("Closing plugin");
	}
	
	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name()
	{
		return "Decl'NetCDF";
	}
};

} // namespace decl_netcdf

PDI_PLUGIN(decl_netcdf)
