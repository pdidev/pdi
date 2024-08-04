/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <pdi/pdi_fwd.h>

#include <pdi/paraconf_wrapper.h>

#include <pdi/array_datatype.h>
#include <pdi/error.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

#include <data_descriptor_impl.h>
#include <pdi/data_descriptor.h>

#include "global_context.h"
#include "mocks/datatype_mock.h"

using namespace PDI;
using namespace std;

namespace PDI {
// handler to private fields of Descriptor
struct Descriptor_test_handler {
	static unique_ptr<Data_descriptor> default_desc(Global_context& global_ctx)
	{
		return unique_ptr<Data_descriptor> {new Data_descriptor_impl{global_ctx, "default_desc"}};
	}
};
} // namespace PDI

/*
 * Struct prepared for gpu_support_intern
 */
struct gpu_support_intern: public ::testing::Test {
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("")};
	unique_ptr<Data_descriptor> m_desc_default = Descriptor_test_handler::default_desc(global_ctx);
};

/*
 * Name:                gpu_support_intern.share_from_cpu_to_all
 *
 * Tested functions:    The inner mechanism of sharing reference with new GPU support
 *
 * Description:         Checks the values are the same after sharing CPU references from CPU to CPU / GPU.
 */
TEST_F(gpu_support_intern, share_from_cpu_to_all)
{
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	
	// Share CPU var to CPU (read, write, share_from_gpu)
	int var_scalar_cpu_to_cpu = 11;
	Ref_r base_ref_cpu_to_cpu{&var_scalar_cpu_to_cpu, &free, scalar_type, true, false, false};
	this->m_desc_default->share(base_ref_cpu_to_cpu, true, false, false);
	
	/* Catch from CPU */
	Ref_r catched_ref_cpu = Ref_r{base_ref_cpu_to_cpu};
	EXPECT_FALSE(!catched_ref_cpu);
	
	const void* catched_ptr_cpu = const_cast<void*>(catched_ref_cpu.get());
	EXPECT_EQ(*(int*)catched_ptr_cpu, var_scalar_cpu_to_cpu);
	
	
	// Share CPU var to GPU (read, write, share_from_gpu)
	int var_scalar_cpu_to_gpu = 12;
	Ref_r base_ref_cpu_to_gpu{&var_scalar_cpu_to_gpu, &free, scalar_type, true, false, false};
	this->m_desc_default->share(base_ref_cpu_to_gpu, true, false, false);
	
	/* Catch from GPU */
	Ref_r_gpu catched_ref_gpu = Ref_r_gpu{base_ref_cpu_to_gpu};
	EXPECT_FALSE(!catched_ref_gpu);
	
	// Check that the reference is valid.
	// EXPECT_FALSE(!Reference_base::get_content(catched_ref_gpu));
	// EXPECT_FALSE(!Reference_base::get_content(catched_ref_gpu)->m_buffer);
	// EXPECT_EQ(0, Reference_base::get_content(catched_ref_gpu)->m_buffer->m_read_locks);
	// EXPECT_EQ(2, Reference_base::get_content(catched_ref_gpu)->m_buffer->m_write_locks);
	
	// Check value is the same
	const void* catched_ptr_gpu = const_cast<void*>(catched_ref_gpu.get());
	EXPECT_EQ(*(int*)catched_ptr_gpu, var_scalar_cpu_to_gpu);
	
	//
	base_ref_cpu_to_cpu.release();
	base_ref_cpu_to_gpu.release();
}

/*
 * Name:                gpu_support_intern.share_from_gpu_to_all
 *
 * Tested functions:    The inner mechanism of sharing reference with new GPU support
 *
 * Description:         Checks the values are the same after sharing GPU references from CPU to CPU / GPU.
 */
TEST_F(gpu_support_intern, share_from_gpu_to_all)
{
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	
	// Share GPU var to CPU (read, write, share_from_gpu)
	int var_scalar_gpu_to_cpu = 21;
	Ref_r_gpu base_ref_gpu_to_cpu{&var_scalar_gpu_to_cpu, &free, scalar_type, true, false, true};
	this->m_desc_default->share(base_ref_gpu_to_cpu, true, false, true);
	
	/* Catch from CPU */
	Ref_r catched_ref_cpu = Ref_r{base_ref_gpu_to_cpu};
	EXPECT_FALSE(!catched_ref_cpu);
	
	const void* catched_ptr_cpu = const_cast<void*>(catched_ref_cpu.get());
	EXPECT_EQ(*(int*)catched_ptr_cpu, var_scalar_gpu_to_cpu);
	
	
	// Share GPU var to GPU (read, write, share_from_gpu)
	int var_scalar_gpu_to_gpu = 22;
	Ref_r_gpu base_ref_gpu_to_gpu{&var_scalar_gpu_to_gpu, &free, scalar_type, true, false, true};
	this->m_desc_default->share(base_ref_gpu_to_gpu, true, false, true);
	
	/* Catch from GPU */
	Ref_r_gpu catched_ref_gpu = Ref_r_gpu{base_ref_gpu_to_gpu};
	EXPECT_FALSE(!catched_ref_gpu);
	
	const void* catched_ptr_gpu = const_cast<void*>(catched_ref_gpu.get());
	EXPECT_EQ(*(int*)catched_ptr_gpu, var_scalar_gpu_to_gpu);
	
	//
	base_ref_gpu_to_cpu.release();
	base_ref_gpu_to_gpu.release();
}
