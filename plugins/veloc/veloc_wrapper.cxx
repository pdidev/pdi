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

#include "veloc_wrapper.h"

using PDI::Invalid_action_error;
using PDI::System_error;

void init(PDI::Context& ctx, MPI_Comm comm, const std::string veloc_file)
{
	if (VELOC_Init(comm, veloc_file.c_str()) != VELOC_SUCCESS) {
		throw System_error{"Failure during VeloC initialization"};
	}
}

void protect_data(PDI::Context& ctx, int id, const void* ptr, size_t n_elements, size_t element_bytes, std::string name)
{
	if (VELOC_Mem_protect(id, const_cast<void*>(ptr), n_elements, element_bytes) != VELOC_SUCCESS) {
		throw System_error{"Memory protect failed for data {} with ptr = {} and size = {}", name, ptr, (n_elements * element_bytes)};
	}
}

void unprotect_data(PDI::Context& ctx, int id, std::string name)
{
	if (VELOC_Mem_unprotect(id) != VELOC_SUCCESS) {
		throw System_error{"Memory unprotect failed for data {}", name};
	}
}

void write_checkpoint(PDI::Context& ctx, const std::string label, int version)
{
	if (VELOC_Checkpoint(label.c_str(), version) != VELOC_SUCCESS) {
		throw System_error{"Error during checkpointing"};
	}
}

int read_checkpoint(PDI::Context& ctx, const std::string label, int version)
{
	int target = 0;

	if (version != 0) {
		target = VELOC_Restart_test(label.c_str(), version > 0 ? version : 0);
	}

	if (target < 0) {
		throw Invalid_action_error{"No previous checkpoint found for restarting"};
	} else {
		ctx.logger().info("Previous checkpoint found at iteration {}. Initiating restart...", target);
		if (VELOC_Restart(label.c_str(), target) != VELOC_SUCCESS) {
			throw System_error{"Error during restarting"};
		}
	}
	return target;
}

void init_restart(PDI::Context& ctx, const std::string label, int version)
{
	int target = 0;

	if (version != 0) {
		target = VELOC_Restart_test(label.c_str(), version > 0 ? version : 0);
	}

	if (target < 0) {
		throw Invalid_action_error{"No previous checkpoint found for restarting"};
	} else {
		ctx.logger().info("Previous checkpoint found at iteration {}. Initiating restart...", target);
		if (VELOC_Restart_begin(label.c_str(), target) != VELOC_SUCCESS) {
			throw System_error{"Error when initiating the restart phase"};
		}
	}
}

void end_restart(PDI::Context& ctx)
{
	if (VELOC_Restart_end(1) != VELOC_SUCCESS) {
		throw System_error{"Error when finalizing the restart phase"};
	}
}

void init_checkpoint(PDI::Context& ctx, const std::string label, int version)
{
	if (VELOC_Checkpoint_begin(label.c_str(), version) != VELOC_SUCCESS) {
		throw System_error{"Error when initiating the checkpoint phase"};
	}
}

void end_checkpoint(PDI::Context& ctx)
{
	if (VELOC_Checkpoint_end(1) != VELOC_SUCCESS) {
		throw System_error{"Error when finalizing the checkpoint phase"};
	}
}

void route_file(PDI::Context& ctx, const std::string& input_filename, char* output_filename)
{
	if (VELOC_Route_file(input_filename.c_str(), output_filename) != VELOC_SUCCESS) {
		throw System_error{"Error when routing file"};
	} else {
		ctx.logger().info("File routed successfully from {} to {}", input_filename, output_filename);
	}
}

void finalize(PDI::Context& ctx)
{
	if (VELOC_Finalize(1) != VELOC_SUCCESS) {
		throw System_error{"Error finalizing VeloC"};
	}
}
