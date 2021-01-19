/*******************************************************************************
 * Copyright (C) 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <mpi.h>

#include <string>
#include <unordered_set>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>

#include "mpi_comm_transtyper.h"

namespace {

using PDI::Context;
using PDI::Context_proxy;
using PDI::Data_descriptor;
using PDI::Datatype;
using PDI::Datatype_uptr;
using PDI::Error;
using PDI::Impl_error;
using PDI::len;
using PDI::Plugin;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_long;
using std::string;

struct mpi_plugin: Plugin {
	/// the MPI_Comm datatype
	Scalar_datatype m_mpi_comm_datatype{Scalar_kind::UNKNOWN, sizeof(MPI_Comm), alignof(MPI_Comm)};
	
	/// the MPI_Comm_f datatype
	Scalar_datatype m_mpi_comm_f_datatype{Scalar_kind::SIGNED, sizeof(MPI_Fint)};
	
	MPI_Comm_transtyper m_transtyper;
	
	void set_up_logger(Context& ctx, PC_tree_t)
	{
		//set up format
		int world_rank;
		MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
		char format[64];
		snprintf(format, 64, "[PDI][MPI][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
		ctx.logger()->set_pattern(string(format));
		
		//set up format for global logger
		try {
			Context_proxy& ctx_proxy = dynamic_cast<Context_proxy&>(ctx);
			snprintf(format, 64, "[PDI][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
			ctx_proxy.pdi_core_logger()->set_pattern(string(format));
		} catch (std::bad_cast&) {
			ctx.logger()->warn("Cannot cast Context to Context_proxy");
		}
	}
	
	void add_predefined(Context& ctx, const string& name, void* data, Datatype_uptr type)
	{
		Data_descriptor& predef_desc = ctx.desc(name);
		if (!predef_desc.empty()) {
			throw Impl_error{"Predefined descriptor already defined `{}'", name};
		}
		
		predef_desc.metadata(true);
		// share a RO reference on comm_self with no memory destruction function (local variable)
		predef_desc.share({data, nullptr, move(type), true, false}, true, false);
		predef_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
	}
	
	mpi_plugin(Context& ctx, PC_tree_t config):
		Plugin{ctx},
		m_transtyper{ctx, PC_get(config, ".transtype"), m_mpi_comm_datatype, m_mpi_comm_f_datatype}
	{
		set_up_logger(ctx, PC_get(config, ".logging"));
		
		// share the MPI_Comm datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm", [this](Context&, PC_tree_t) {
			return m_mpi_comm_datatype.clone();
		});
		
		//load MPI_COMM_WORLD
		MPI_Comm comm_world = MPI_COMM_WORLD;
		add_predefined(ctx, "MPI_COMM_WORLD", &comm_world, m_mpi_comm_datatype.clone_type());
		
		//load MPI_COMM_SELF
		MPI_Comm comm_self = MPI_COMM_SELF;
		add_predefined(ctx, "MPI_COMM_SELF", &comm_self, m_mpi_comm_datatype.clone_type());
		
		//load MPI_COMM_NULL
		MPI_Comm comm_null = MPI_COMM_NULL;
		add_predefined(ctx, "MPI_COMM_NULL", &comm_null, m_mpi_comm_datatype.clone_type());
		
		// share the MPI_Comm_f datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm_f", [this](Context&, PC_tree_t) {
			return m_mpi_comm_f_datatype.clone();
		});
		
		//load MPI_COMM_WORLD_F
		MPI_Fint comm_world_f = MPI_Comm_c2f(MPI_COMM_WORLD);
		add_predefined(ctx, "MPI_COMM_WORLD_F", &comm_world_f, m_mpi_comm_f_datatype.clone_type());
		
		//load MPI_COMM_SELF_F
		MPI_Fint comm_self_f = MPI_Comm_c2f(MPI_COMM_SELF);
		add_predefined(ctx, "MPI_COMM_SELF_F", &comm_self_f, m_mpi_comm_f_datatype.clone_type());
		
		//load MPI_COMM_NULL_F
		MPI_Fint comm_null_f = MPI_Comm_c2f(MPI_COMM_NULL);
		add_predefined(ctx, "MPI_COMM_NULL_F", &comm_null_f, m_mpi_comm_f_datatype.clone_type());
		
		ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->m_transtyper.data(name.c_str(), ref);
		});
		
		ctx.logger()->info("Plugin loaded successfully");
	}
	
	~mpi_plugin()
	{
		context().logger()->info("Closing plugin");
	}
	
};

} // namespace <anonymous>

PDI_PLUGIN(mpi)
