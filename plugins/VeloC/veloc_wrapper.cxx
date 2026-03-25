/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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


#include "veloc_wrapper.h"

using PDI::Context;
using PDI::Ref_r;

using std::string;


void init(MPI_Comm comm, std::string veloc_file){

	if(VELOC_Init(comm, veloc_file.c_str()) != VELOC_SUCCESS) {
		printf("Error initializing VELOC! Aborting...\n");
		exit(2);
	}
}

void protect_data(PDI::Context& ctx, int id, void * ptr, size_t n, size_t sub_bytes,
			const string& name){

	VELOC_Mem_protect(id, ptr, n, sub_bytes);   
	ctx.logger().info("Registered {} at index {} with size = {} bytes\n", 
        name.c_str(), id, (n*sub_bytes));

}

void unprotect_data(PDI::Context& ctx, int id){

	VELOC_Mem_unprotect(id);   
    
	ctx.logger().info("Unregistered id {}\n", id);

}

int write_checkpoint(PDI::Context& ctx, std::optional<const PDI::Expression> when, std::string label, std::string iter_name){

	if(when.has_value() && !when->to_long(ctx)){
        return 0;
    }

	Ref_r ref_r_iter = ctx.desc(iter_name).ref();
	auto v = ref_r_iter.scalar_value<int>();

	std::cout << " value of iter_name is " << v << std::endl;
	
	if (VELOC_Checkpoint(label.c_str(), v) != VELOC_SUCCESS) {
		std::cout << "Error checkpointing! Aborting...\n" << std::endl;
		exit(2);
	}

	return 1;

}

int load_checkpoint(PDI::Context& ctx, std::string label){
	
	// restart from last checkpoint (because we pass 0)
	int version = VELOC_Restart_test(label.c_str(), 0);
	// while(1){ 
		
	//     if (v == VELOC_FAILURE) {
	//             printf("No valid checkpoint found.\n");
	//             exit(1);
	//     }

	if (version > 0) {
	std::cout << "Previous checkpoint found at iteration " << version << " initiating restart..." << std::endl; 
	// v can be any version, independent of what VELOC_Restart_test is returning
	
	if (VELOC_Restart(label.c_str(), version) != VELOC_SUCCESS) {
		std::cout << "Error restarting! Aborting ..." << std::endl;   
		exit(2);
	}
	// else {
	// 	break; 
	// }
	}
// }
	return version; 
}


void finalize(){
	VELOC_Finalize(1); // 1 because wait for pending checkpoints to flush if there are any 
}


