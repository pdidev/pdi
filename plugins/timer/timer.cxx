/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <filesystem>
#include <fstream>
#include <string>
#include <unordered_map>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

namespace {

using namespace PDI;

/** The timer plugin 
*/
class timer_plugin: public PDI::Plugin
{
	// Map between data variables and a pair between condition and the output filenames
	std::map<std::string, std::chrono::high_resolution_clock::time_point> start_times;
	std::map<std::string, double> accumulated_times;
	bool timer_enabled = false;

public:
	timer_plugin(Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		// initialize m_data_to_path_map from config.yml
		// read_config_tree(ctx.logger(), spec_tree);

		ctx.logger().info("Plugin loaded successfully");
	}

	~timer_plugin() { context().logger().info("Closing plugin"); }

	static std::string pretty_name() { return "TIMER"; }

private:
	/** Read the configuration file
	 *
	 * \param logger PDI's logger instance
	 * \param spec_tree the yaml tree
	 */
	// void read_config_tree(Logger& logger, PC_tree_t spec_tree)
	// {
	// }

};

} // namespace
PDI_PLUGIN(timer)
