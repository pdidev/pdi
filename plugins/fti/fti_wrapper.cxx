/*******************************************************************************
 * Copyright (C) 2018-2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/error.h>

#include <fti.h>
#include <spdlog/spdlog.h>

#include "fti_wrapper.h"

using PDI::Context;
using PDI::Data_descriptor;
using PDI::Datatype_uptr;
using PDI::Impl_error;
using PDI::Plugin_error;

using std::string;

namespace {

void add_predefined(Context& ctx, const std::string& name, void* data, Datatype_uptr type)
{
	Data_descriptor& predef_desc = ctx.desc(name);
	if (!predef_desc.empty()) {
		throw Impl_error{"Predefined descriptor already defined `%s'", name.c_str()};
	}
	
	predef_desc.metadata(true);
	// share a RO reference on comm_self with no memory destruction function (local variable)
	predef_desc.share({data, nullptr, move(type), true, false}, true, false);
	predef_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
}

} // namespace <anonymous>

namespace fti {

Fti_wrapper::Fti_wrapper(Context& ctx, const Fti_cfg& config, MPI_Comm comm):
	m_head{false}
{
	int status = FTI_Init(const_cast<char*>(config.config(ctx).c_str()), comm);
	if (status != FTI_SCES && status != FTI_HEAD) {
		throw Plugin_error{"Cannot initialize FTI library"};
	}
	if (status == FTI_HEAD) m_head = true;
	
	int fti_world_rank;
	MPI_Comm_rank(FTI_COMM_WORLD, &fti_world_rank);
	char format[64];
	
	//setup logger
	if (m_head) {
		snprintf(format, 64, "[PDI][FTI][%06d (HEAD)][%%T] *** %%^%%l%%$: %%v", fti_world_rank);
	} else {
		snprintf(format, 64, "[PDI][FTI][%06d][%%T] *** %%^%%l%%$: %%v", fti_world_rank);
	}
	ctx.logger()->set_pattern(format);
	
	if (m_head) {
		auto found_it = std::find_if(config.descs().begin(), config.descs().end(),
		[](const std::pair<std::string, fti::Desc_type>& element) {
			return element.second == Desc_type::HEAD;
		});
		if (found_it == config.descs().end()) {
			auto found_mpi_comm_it = std::find_if(config.descs().begin(), config.descs().end(),
			[](const std::pair<std::string, fti::Desc_type>& element) {
				return element.second == Desc_type::MPI_COMM;
			});
			if (found_mpi_comm_it != config.descs().end()) {
				// release mpi_comm desc if FTI was init by MPI_Comm share
				if (!ctx[found_mpi_comm_it->first].empty()) {
					PDI::Ref_r comm {ctx[found_mpi_comm_it->first].ref()};
					int result = 0;
					MPI_Comm_compare(MPI_COMM_WORLD, *static_cast<const MPI_Comm*>(comm.get()), &result);
					if (result != MPI_IDENT) {
						// if it's not MPI_COMM_WORLD, release it
						ctx[found_mpi_comm_it->first].release();
					}
				}
			}
			
			// no head flag defined -> do not return HEAD process
			ctx.logger()->info("Finalizing (this process won't return to the application)");
			MPI_Finalize();
			ctx.finalize_and_exit();
		} else {
			ctx.logger()->debug("Returning after FTI_Init");
		}
	}
	
	//load FTI_COMM_WORLD
	MPI_Comm fti_comm = FTI_COMM_WORLD;
	add_predefined(ctx, "FTI_COMM_WORLD", &fti_comm, ctx.datatype(PC_parse_string("MPI_Comm"))->evaluate(ctx));
	
	//load FTI_COMM_WORLD_F
	MPI_Fint fti_comm_f = MPI_Comm_c2f(FTI_COMM_WORLD);
	add_predefined(ctx, "FTI_COMM_WORLD_F", &fti_comm_f, ctx.datatype(PC_parse_string("MPI_Comm_f"))->evaluate(ctx));
}

bool Fti_wrapper::head()
{
	return m_head;
}

MPI_Comm Fti_wrapper::fti_comm_world()
{
	return FTI_COMM_WORLD;
}

int Fti_wrapper::bit_flip(int dataset_id)
{
	return FTI_BitFlip(dataset_id);
}

int Fti_wrapper::checkpoint(int id, int level)
{
	return FTI_Checkpoint(id, level);
}

int Fti_wrapper::init_type(fti_id_t* type, long size)
{
	return FTI_InitType(type, size);
}

int Fti_wrapper::protect(int id, void* ptr, long count, fti_id_t type)
{
	return FTI_Protect(id, ptr, count, type);
}

int Fti_wrapper::recover()
{
	return FTI_Recover();
}

void* Fti_wrapper::realloc(int id, void* ptr)
{
	return FTI_Realloc(id, ptr);
}

int Fti_wrapper::recover_var(int id)
{
	return FTI_RecoverVar(id);
}

int Fti_wrapper::send_file(char* src_path, char* dest_path)
{
	return FTI_SendFile(src_path, dest_path);
}

int Fti_wrapper::snapshot()
{
	return FTI_Snapshot();
}

int Fti_wrapper::stage_dir(char* buffer, int size)
{
	return FTI_GetStageDir(buffer, size);
}

int Fti_wrapper::stage_status(int id)
{
	return FTI_GetStageStatus(id);
}

long Fti_wrapper::stored_size(int id)
{
	return FTI_GetStoredSize(id);
}

int Fti_wrapper::status()
{
	return FTI_Status();
}

int Fti_wrapper::add_vector_field(fti_id_t composite_type_id, char* name, fti_id_t type_id, size_t offset, int ndims, int* dim_sizes)
{
	return FTI_AddVectorField(composite_type_id, name, type_id, offset, ndims, dim_sizes);
}

int Fti_wrapper::add_scalar_field(fti_id_t composite_type_id, char* name, fti_id_t type_id, size_t offset)
{
	return FTI_AddScalarField(composite_type_id, name, type_id, offset);
}

int Fti_wrapper::define_dataset(int id, int rank, int* dim_len, char* name, FTIT_H5Group* h5_group)
{
	return FTI_DefineDataset(id, rank, dim_len, name, h5_group);
}

fti_id_t Fti_wrapper::init_composite_type(char* name, size_t size, FTIT_H5Group* h5_group)
{
	return FTI_InitCompositeType(name, size, h5_group);
}

int Fti_wrapper::init_group(FTIT_H5Group* h5_group, char* name, FTIT_H5Group* parent)
{
	return FTI_InitGroup(h5_group, name, parent);
}

int Fti_wrapper::rename_group(FTIT_H5Group* h5_group, char* name)
{
	return FTI_RenameGroup(h5_group, name);
}

Fti_wrapper::~Fti_wrapper()
{
	//head processes call FTI_Finalize when returning from listening
	if (!m_head) FTI_Finalize();
}

} // namespace fti
