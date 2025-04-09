/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2024 National Institute for Research in Digital Science and Technology (Inria)
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


#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/error.h>

#include <Damaris.h>

#include "damaris_wrapper.h"

using PDI::Context;
using PDI::Context_proxy;
using PDI::Data_descriptor;
using PDI::Datatype_sptr;
using PDI::Impl_error;
using PDI::Plugin_error;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;

using std::string;

namespace {

void add_predefined(Context& ctx, const std::string& name, void* data, Datatype_sptr type)
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

namespace damaris_pdi {

// Constructer calls damaris_init() 
Damaris_wrapper::Damaris_wrapper(Context& ctx, const char* xmlConfigObject, MPI_Comm comm)
{
	ctx.logger().info("Damaris lib initialization starts...");
	int status = damaris_pdi_initialize(xmlConfigObject,  comm);
	if (status != DAMARIS_OK) {
		throw Plugin_error{"Cannot initialize Damaris library"};
	}
    else
	    ctx.logger().info("Damaris lib initialization Done!");
}

int Damaris_wrapper::damaris_pdi_initialize(const char* configfile, MPI_Comm comm)
{
	return damaris_initialize(configfile, comm);
}

int Damaris_wrapper::damaris_pdi_finalize( void )
{
	return damaris_finalize();
}

int Damaris_wrapper::damaris_pdi_start(int* is_client)
{
	return damaris_start(is_client);
}

int Damaris_wrapper::damaris_pdi_stop( void )
{
	return damaris_stop();
}

int Damaris_wrapper::damaris_pdi_write(const char* varname,  const void* data)
{
	return damaris_pdi_write_block(varname, 0, data);
}
int Damaris_wrapper::damaris_pdi_write(std::string varname,  const void* data)
{
	return damaris_pdi_write_block(varname, 0, data);
}

bool Damaris_wrapper::damaris_pdi_write_block(const char* varname, int32_t block, const void* data)
{
	return damaris_write_block(varname,block, data);
}

bool Damaris_wrapper::damaris_pdi_write_block(std::string varname, int32_t block, const void* data)
{
	return damaris_write_block(varname.c_str(),block, data);
}

int Damaris_wrapper::damaris_pdi_get_type(const char* variable_name, DAMARIS_TYPE_STR *vartype)
{
	return damaris_get_type(variable_name, vartype);
}

int Damaris_wrapper::damaris_pdi_has_plugin(DAMARIS_PLUGIN_TYPE plugin)
{
	return damaris_has_plugin( plugin);
}


int Damaris_wrapper::damaris_pdi_alloc(const char* varname, void** ptr)
{
	return damaris_alloc(varname, ptr);
}


int Damaris_wrapper::damaris_pdi_alloc_block(const char* varname, int32_t block, void** ptr)
{
	return damaris_alloc_block(varname, block, ptr);
}

int Damaris_wrapper::damaris_pdi_commit(const char* varname)
{
	return damaris_commit(varname);
}

int Damaris_wrapper::damaris_pdi_commit_iteration(const char* varname, int32_t iteration)
{
	return damaris_commit_iteration(varname, iteration);
}

int Damaris_wrapper::damaris_pdi_commit_block_iteration(const char* varname, 
	int32_t block, int32_t iteration)
{
	return damaris_commit_block_iteration( varname, block, iteration);
}

int Damaris_wrapper::damaris_pdi_clear(const char* varname)
{
	return damaris_clear(varname);
}

int Damaris_wrapper::damaris_pdi_clear_block(const char* varname, int32_t block)
{
	return damaris_clear_block(varname, block);
}

int Damaris_wrapper::damaris_pdi_clear_iteration(const char* varname, int32_t iteration)
{
	return damaris_clear_iteration(varname, iteration);
}

int Damaris_wrapper::damaris_pdi_clear_block_iteration(const char* varname, 
	int32_t block, int32_t iteration)
{
	return damaris_clear_block_iteration(varname, block, iteration);
}

int Damaris_wrapper::damaris_pdi_signal(const char* signal_name)
{
	return damaris_signal(signal_name);
}

int Damaris_wrapper::damaris_pdi_bind(const char* signal_name, signal_t sig)
{
	return damaris_bind(signal_name, sig);
}

int Damaris_wrapper::damaris_pdi_parameter_get(const char* param_name, 
	void* buffer, unsigned int size)
{
	return damaris_parameter_get(param_name, buffer, size);
}

int Damaris_wrapper::damaris_pdi_parameter_set(const char* param_name, 
	const void* buffer, unsigned int size)
{
	return damaris_parameter_set(param_name, buffer, size);
}

int Damaris_wrapper::damaris_pdi_set_position(const char* var_name, const int64_t* position)
{
	return damaris_set_position(var_name, position);
}

int Damaris_wrapper::damaris_pdi_set_block_position(const char* var_name, 
	int32_t block, const int64_t* position)
{
	return damaris_set_block_position(var_name, block, position);
}

int Damaris_wrapper::damaris_pdi_client_comm_get(MPI_Comm* comm)
{
	return damaris_client_comm_get(comm);
}

int Damaris_wrapper::damaris_pdi_end_iteration( void )
{
	return damaris_end_iteration( );
}

int Damaris_wrapper::damaris_pdi_get_iteration(int* iteration)
{
	return damaris_get_iteration(iteration);
}


Damaris_wrapper::~Damaris_wrapper()
{
	// Call before MPI_Finalize()
    damaris_finalize();
}

} // namespace damaris_pdi