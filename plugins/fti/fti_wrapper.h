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

#ifndef FTI_WRAPPER_H_
#define FTI_WRAPPER_H_

#include <string>

#include <pdi/pdi_fwd.h>

#include "fti_cfg.h"

namespace fti {

class Fti_wrapper
{
	bool m_head;
	
	Fti_wrapper(const Fti_wrapper&) = delete;
	
	Fti_wrapper& operator=(const Fti_wrapper&) = delete;
	
public:
	Fti_wrapper(PDI::Context& ctx, const Fti_cfg& config, MPI_Comm comm);
	
	bool head();
	
	MPI_Comm fti_comm_world();
	
	int bit_flip(int dataset_id);
	
	int checkpoint(int id, int level);
	
	int init_type(FTIT_type* type, long size);
	
	int protect(int id, void* ptr, long count, FTIT_type type);
	
	int recover();
	
	void* realloc(int id, void* ptr);
	
	int recover_var(int id);
	
	int send_file(char* src_path, char* dest_path);
	
	int snapshot();
	
	int stage_dir(char* buffer, int size);
	
	int stage_status(int id);
	
	long stored_size(int id);
	
	int status();
	
	void add_complex_field(FTIT_complexType* definition, FTIT_type* type, size_t offset, int rank, int* dim_len, int id, char* name);
	
	void add_simple_field(FTIT_complexType* definition, FTIT_type* type, size_t offset, int id, char* name);
	
	int define_dataset(int id, int rank, int* dim_len, char* name, FTIT_H5Group* h5_group);
	
	int init_complex_type(FTIT_type* type, FTIT_complexType* definition, int len, size_t size, char* name, FTIT_H5Group* h5_group);
	
	int init_group(FTIT_H5Group* h5_group, char* name, FTIT_H5Group* parent);
	
	int rename_group(FTIT_H5Group* h5_group, char* name);
	
	~Fti_wrapper();
}; // class Fti

} // namespace fti
#endif // FTI_WRAPPER_H_
