/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
	#include <mpi.h>
#endif

#include <string>
#include <unordered_map>
#include <vector>

#include <paraconf.h>
#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

#include "file_op.h"
#include "hdf5_wrapper.h"

namespace {

using PDI::Context;
using PDI::each;
using PDI::opt_each;
using PDI::Plugin;
using PDI::Ref;
using PDI::to_long;
using std::string;
using std::unordered_map;
using std::vector;

using namespace decl_hdf5;

/** The decl'HDF5 plugin
 */
class decl_hdf5_plugin:
	public Plugin
{
	/// the file operations to execute on events, we use a map of vector vs. multimap to conserve order
	unordered_map<string, vector<File_op>> m_events;
	
	/// the file operations to execute on data, we use a map of vector vs. multimap to conserve order
	unordered_map<string, vector<File_op>> m_data;
	
	/** Set-up the plugin-specific logger
	 *
	 * \param logging_tree the logging specific config
	 */
	void set_up_logger(PC_tree_t logging_tree)
	{
		context().logger()->set_pattern("[PDI][Decl'HDF5][%T] *** %^%l%$: %v");
		
#ifdef H5_HAVE_PARALLEL
		int mpi_init = 0;
		MPI_Initialized(&mpi_init);
		if (mpi_init) {
			//set up format
			int world_rank;
			MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
			char format[64];
			snprintf(format, 64, "[PDI][Decl'HDF5][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
			context().logger()->set_pattern(string(format));
		}
#endif
	}
	
public:
	decl_hdf5_plugin(Context& ctx, PC_tree_t config):
		Plugin {ctx}
	{
		Hdf5_error_handler _;
		if ( 0>H5open() ) handle_hdf5_err("Cannot initialize HDF5 library");
		set_up_logger(PC_get(config, ".logging"));
		
		opt_each(config, [&](PC_tree_t elem) {
			for (auto&& op: File_op::parse(ctx, elem)) {
				auto&& events = op.event();
				if ( events.empty() ) {
					// if there are no event names, this is data triggered
					assert(op.dataset_ops().size() <= 1);
					for ( auto&& transfer: op.dataset_ops() ) {
						m_data[transfer.value()].emplace_back(op);
					}
				} else {
					// if there are event names, this is event triggered
					for ( auto&& evname: events ) {
						m_events[evname].emplace_back(op);
					}
				}
			}
		});
		
		ctx.add_data_callback([this](const std::string& name, Ref ref) {
			this->data(name, ref);
		});
		ctx.add_event_callback([this](const std::string& name) {
			this->event(name);
		});
		
		ctx.logger()->info("Plugin loaded successfully");
	}
	
	~decl_hdf5_plugin()
	{
		context().logger()->info("Closing plugin");
	}
	
	void data(const std::string& name, Ref ref)
	{
		Hdf5_error_handler _;
		for (auto&& op: m_data[name]) {
			op.execute(context());
		}
	}
	
	void event(const std::string& event)
	{
		Hdf5_error_handler _;
		for (auto&& op: m_events[event]) {
			op.execute(context());
		}
	}
	
}; // class decl_hdf5_plugin

} // namespace <anonymous>

PDI_PLUGIN(decl_hdf5)
