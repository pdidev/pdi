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
#include <iostream>
#include <cassert>


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

void protect_data(PDI::Context& ctx, int id, void * ptr, size_t n, size_t sub_bytes){

	std::cout << " id : " << id << " at ptr ="<< ptr << " of size = " << n*sub_bytes<< std::endl; 
	if (VELOC_Mem_protect(id, ptr, n, sub_bytes)!= VELOC_SUCCESS)
	{
		std::cout << "mem protect failed" << std::endl;
	}
	else{
		std::cout << "Protected id "  << id << std::endl; 
	} 

}

void unprotect_data(PDI::Context& ctx, int id){

	// VELOC_Mem_unprotect(id);  
	
	if (VELOC_Mem_unprotect(id) != VELOC_SUCCESS)
	{
		std::cout << "mem unprotect failed" << std::endl;
	}
	else{
		std::cout << "Unproteted id " << id << std::endl;
	}
    

}

int write_checkpoint(PDI::Context& ctx, std::optional<const PDI::Expression> when, std::string label, std::string iter_name){

	if(when.has_value() && !when->to_long(ctx)){
        return 0;
    }

	Ref_r ref_r_iter = ctx.desc(iter_name).ref();
	auto version = ref_r_iter.scalar_value<int>();
	
	if (VELOC_Checkpoint(label.c_str(), version) != VELOC_SUCCESS) {
		std::cout << "Error checkpointing! Aborting...\n" << std::endl;
		exit(2);
	}

	return version;

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
	std::cout << "Previous checkpoint found at iteration " << version << 
		" initiating restart..." << std::endl; 
	
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

void selective_load(std::string label, int * ids, int len){

	int version = VELOC_Restart_test(label.c_str(), 0);

	if (version > 0) {

		std::cout << "Previous checkpoint found at iteration " << version 
			<< " Initiating restart... \n" << std::endl;
	
		if(VELOC_Restart_begin(label.c_str(), version)!= VELOC_SUCCESS){
			std::cout << "Error at VELOC_Restart_begin " << std::endl;
		};

		std::cout << " ids = " << ids << std::endl; 
		
		if (VELOC_Recover_selective(VELOC_RECOVER_SOME, ids, len) != VELOC_SUCCESS) {
			std::cout << "Error restarting variable(s) ";
			for (int i = 0; i < len; ++i) {
				std::cout << ids[i] << ", ";
			}
			std::cout << " Aborting ..." << std::endl;
			VELOC_Restart_end(0);
			exit(2);	
		}

		if(VELOC_Restart_end(1) != VELOC_SUCCESS){
			std::cout << "Error at VELOC_Restart_end " << std::endl;
		};
	
	}

}

void restart_end(){

	if(VELOC_Restart_end(1) != VELOC_SUCCESS){
			std::cout << "Error at VELOC_Restart_end " << std::endl;
		};
}

void init_restart(PDI::Context& ctx, std::string label){
	
	int version = VELOC_Restart_test(label.c_str(), 0);

	if (version > 0) {

		std::cout << "Previous checkpoint found at iteration " << version 
			<< " Initiating restart... \n" << std::endl;
	
		if(VELOC_Restart_begin(label.c_str(), version)!= VELOC_SUCCESS){
			std::cout << "Error at VELOC_Restart_begin " << std::endl;
		};
	}

}


void init_checkpoint(PDI::Context& ctx, std::string label, std::string iter_name){

	Ref_r ref_r_iter = ctx.desc(iter_name).ref(); // this should be in veloc.xx
	auto version = ref_r_iter.scalar_value<int>(); // this should be in veloc.xx
	
	if(VELOC_Checkpoint_begin(label.c_str(), version)!= VELOC_SUCCESS){
		std::cout << "Error at VELOC_Checkpoint_begin " << std::endl;
	};

}

void end_checkpoint(){
	assert(VELOC_Checkpoint_end(1) == VELOC_SUCCESS);
}

void route_file(const std::string& input_filename, char* output_filename) {
    if (VELOC_Route_file(input_filename.c_str(), output_filename) != VELOC_SUCCESS) {
        std::cout << "Routing of file failed" << std::endl;
    } else {
        std::cout << "File routed successfully" << std::endl;
		std::cout << "input_filename = " << input_filename << std::endl;
		std::cout << "output_filename = " << output_filename << std::endl;
    }
}

void finalize(){
	VELOC_Finalize(1); // 1 because wait for pending checkpoints to flush if there are any 
}


