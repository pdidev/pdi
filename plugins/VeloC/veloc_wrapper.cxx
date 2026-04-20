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

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/plugin.h>
#include <pdi/expression.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/ref_any.h>

#include <optional>
#include <iostream>
#include <cassert>


#include "veloc_wrapper.h"

using PDI::Context;
using PDI::Ref_r;

using std::string;

void init(PDI::Context& ctx, MPI_Comm comm, std::string veloc_file){
    if(VELOC_Init(comm, veloc_file.c_str()) != VELOC_SUCCESS) {
        ctx.logger().error("Error initializing VELOC. Aborting.");
        exit(2);
    }
}

void protect_data(PDI::Context& ctx, int id, void * ptr, size_t n, size_t sub_bytes){
    if (VELOC_Mem_protect(id, ptr, n, sub_bytes) != VELOC_SUCCESS){
        ctx.logger().error("Memory protect failed for id {} with ptr = {} and size = {}",
                     id, ptr, (n*sub_bytes));
    } else {
        ctx.logger().info("Memory protect succeeded for id {} with ptr = {} and size = {}",
                     id, ptr, (n*sub_bytes));
    }
}

void unprotect_data(PDI::Context& ctx, int id){
    if (VELOC_Mem_unprotect(id) != VELOC_SUCCESS){
        ctx.logger().error("Memory unprotect failed for id {}", id);
    } 
}

int write_checkpoint(PDI::Context& ctx, std::optional<const PDI::Expression> when, std::string label, int version){
    if(when.has_value() && !when->to_long(ctx)){
        return 0;
    }
    if (VELOC_Checkpoint(label.c_str(), version) != VELOC_SUCCESS) {
        ctx.logger().error("Error during checkpointing. Aborting.");
        exit(2);
    }
    return 1;
}

int read_checkpoint(PDI::Context& ctx, std::string label, int version){
    int target = (version >= 0) ? version : VELOC_Restart_test(label.c_str(), 0);
    if (target >= 0) {
        ctx.logger().info("Previous checkpoint found at iteration {}. Initiating restart...", target);
        if (VELOC_Restart(label.c_str(), target) != VELOC_SUCCESS) {
            ctx.logger().error("Error during restart. Aborting.");
            exit(2);
        }
    }
    return target;
}

void init_restart(PDI::Context& ctx, std::string label,  int version){
    int target = (version >= 0) ? version : VELOC_Restart_test(label.c_str(), 0);
    if (target > 0) {
        ctx.logger().info("Previous checkpoint found at iteration {}. Initiating restart...", target);
        if(VELOC_Restart_begin(label.c_str(), target) != VELOC_SUCCESS){
            ctx.logger().error("Error when initiating the restart phase.");
        }
    }
}


void end_restart(PDI::Context& ctx){
    if(VELOC_Restart_end(1) != VELOC_SUCCESS){
        ctx.logger().error("Error when ending restart phase. Restart failed.");
    }
}

void init_checkpoint(PDI::Context& ctx, std::string label, int version){
    if(VELOC_Checkpoint_begin(label.c_str(), version) != VELOC_SUCCESS){
        ctx.logger().error("Error when initiating the checkpoint phase.");
    }
}

void end_checkpoint(PDI::Context& ctx){
    if(VELOC_Checkpoint_end(1) != VELOC_SUCCESS){
        ctx.logger().error("Error when finalizing the checkpoint phase.");
    }
}

void route_file(PDI::Context& ctx, const std::string& input_filename, char* output_filename){
    if (VELOC_Route_file(input_filename.c_str(), output_filename) != VELOC_SUCCESS) {
        ctx.logger().error("Error when routing file.");
    } else {
        ctx.logger().info("File routed successfully from {} to {}", input_filename, output_filename);
    }
}

void finalize(PDI::Context& ctx){
    if(VELOC_Finalize(1) != VELOC_SUCCESS) {
        ctx.logger().error("Error finalizing VELOC. Aborting.");
        exit(2);
    }
}