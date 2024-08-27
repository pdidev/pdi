/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
#include <mpi.h>
#endif

#include <string>
#include <unordered_map>
#include <vector>

#include <paraconf.h>

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
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;

using namespace decl_hdf5;

/** The decl'HDF5 plugin
 */
class decl_hdf5_plugin: public Plugin
{
	/// the file operations to execute on events, we use a map of vector vs. multimap to conserve order
	unordered_map<string, vector<File_op>> m_events;

	/// the file operations to execute on data, we use a map of vector vs. multimap to conserve order
	unordered_map<string, vector<File_op>> m_data;

public:
	decl_hdf5_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
	{
		Hdf5_error_handler _;
		if (0 > H5open()) handle_hdf5_err("Cannot initialize HDF5 library");
		opt_each(config, [&](PC_tree_t elem) {
			for (auto&& op: File_op::parse(ctx, elem)) {
				auto&& events = op.event();
				if (events.empty()) {
					// if there are no event names, this is data triggered
					assert(op.dataset_ops().size() <= 1);
					for (auto&& transfer: op.dataset_ops()) {
						m_data[transfer.value()].emplace_back(op);
					}
					for (auto&& transfer: op.attribute_ops()) {
						if (!transfer.desc().empty()) {
							m_data[transfer.desc()].emplace_back(op);
						}
					}
					for (auto&& transfer: op.dataset_size_ops()) {
						m_data[transfer.first].emplace_back(op);
					}
				} else {
					// if there are event names, this is event triggered
					for (auto&& evname: events) {
						m_events[evname].emplace_back(op);
					}
				}
			}
		});

		ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) { this->data(name, ref); });
		ctx.callbacks().add_event_callback([this](const std::string& name) { this->event(name); });

		ctx.logger().info("Plugin loaded successfully");
	}

	~decl_hdf5_plugin()
	{
		if (0 > H5close()) handle_hdf5_err("Cannot finalize HDF5 library");
		context().logger().info("Closing plugin");
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

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Decl'HDF5"; }

}; // class decl_hdf5_plugin

} // namespace

PDI_PLUGIN(decl_hdf5)
