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
#include <pdi/pdi_fwd.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

struct mpi_plugin: PDI::Plugin {

	mpi_plugin(PDI::Context& ctx, PC_tree_t, MPI_Comm*):
		Plugin{ctx}
	{
		// deleter for MPI_Comm
		auto deleter = [](void* ptr) {
			auto comm_ptr = static_cast<MPI_Comm*>(ptr);
			MPI_Comm_free(comm_ptr);
			delete comm_ptr;
		};
		
		// load MPI_COMM_WORLD
		MPI_Comm* comm = new MPI_Comm;
		MPI_Comm_dup(MPI_COMM_WORLD, comm);
		PDI::Ref comm_world_ref{comm, deleter, PDI::Datatype_uptr{new PDI::Scalar_datatype(PDI::Scalar_kind::MPI_COMM, sizeof(MPI_Comm))}, true, false};
		context()["MPI_COMM_WORLD"].share(comm_world_ref, false, false);
		
		// load MPI_COMM_SELF
		comm = new MPI_Comm;
		MPI_Comm_dup(MPI_COMM_SELF, comm);
		PDI::Ref comm_self_ref {comm, deleter, PDI::Datatype_uptr{new PDI::Scalar_datatype(PDI::Scalar_kind::MPI_COMM, sizeof(MPI_Comm))}, true, false};
		context()["MPI_COMM_SELF"].share(comm_self_ref, false, false);
		
		// load MPI_COMM_NULL
		comm = new MPI_Comm;
		*comm = MPI_COMM_NULL;
		PDI::Ref comm_null_ref {comm, &free, PDI::Datatype_uptr{new PDI::Scalar_datatype(PDI::Scalar_kind::MPI_COMM, sizeof(MPI_Comm))}, true, false};
		context()["MPI_COMM_NULL"].share(comm_null_ref, false, false);
	}
	
	~mpi_plugin()
	{
		// release the communicators
		context()["MPI_COMM_WORLD"].release();
		context()["MPI_COMM_SELF"].release();
		context()["MPI_COMM_NULL"].release();
	}
};

PDI_PLUGIN(mpi)