/*******************************************************************************
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

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>


namespace {

struct mpi_plugin: PDI::Plugin {

	mpi_plugin(PDI::Context& ctx, PC_tree_t, MPI_Comm*):
		Plugin{ctx}
	{
		// create the MPI_Comm datatype
		PDI::Scalar_datatype mpi_comm_datatype{PDI::Scalar_kind::UNKNOWN, sizeof(MPI_Comm), alignof(MPI_Comm)};
		
		// share the MPI_Comm datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm", [mpi_comm_datatype](const PC_tree_t&) {
			return mpi_comm_datatype.clone();
		});
		
		//load MPI_COMM_WORLD
		MPI_Comm comm_world = MPI_COMM_WORLD;
		PDI::Data_descriptor& comm_world_desc = ctx["MPI_COMM_WORLD"];
		comm_world_desc.metadata(true);
		// share a RO reference on comm_world with no memory destruction function (local variable)
		comm_world_desc.share({&comm_world, nullptr, mpi_comm_datatype.clone_type(), true, false}, true, false);
		comm_world_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
		
		//load MPI_COMM_SELF
		MPI_Comm comm_self = MPI_COMM_SELF;
		PDI::Data_descriptor& comm_self_desc = ctx["MPI_COMM_SELF"];
		comm_self_desc.metadata(true);
		// share a RO reference on comm_self with no memory destruction function (local variable)
		comm_self_desc.share({&comm_self, nullptr, mpi_comm_datatype.clone_type(), true, false}, true, false);
		comm_self_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
		
		//load MPI_COMM_NULL
		MPI_Comm comm_null = MPI_COMM_NULL;
		PDI::Data_descriptor& comm_null_desc = ctx["MPI_COMM_NULL"];
		comm_null_desc.metadata(true);
		// share a RO reference on comm_null with no memory destruction function (local variable)
		comm_null_desc.share({&comm_null, nullptr, mpi_comm_datatype.clone_type(), true, false}, true, false);
		comm_null_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
	}
	
};

} // namespace <anonymous>

PDI_PLUGIN(mpi)
