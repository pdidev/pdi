/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <functional>
#include <iostream>
#include <string>
#include <unordered_set>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>


namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Error;
using PDI::Plugin;
using std::bind;
using std::cout;
using std::endl;
using std::reference_wrapper;
using std::string;
using std::unordered_set;

struct test_plugin: Plugin {

	MPI_Comm my_comm;
	
	unordered_set<Ref> m_refs;
	
	
	test_plugin(Context& ctx, PC_tree_t, MPI_Comm* world, PDI::Logger logger):
		Plugin {ctx}
	{
		if ( MPI_Comm_dup(*world, &my_comm) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		
		int rank;
		if ( MPI_Comm_rank(my_comm, &rank) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		
		if ( rank == 0 ) cout << "[PDI test plugin] Welcome to the test plugin!"<<endl;
	}
	
	~test_plugin()
	{
		int rank;
		if ( MPI_Comm_rank(my_comm, &rank) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		
		if ( rank == 0 ) cout << "[PDI test plugin] Goodbye from the test plugin!"<<endl;
		
		if ( MPI_Comm_free(&my_comm) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
	}
	
	void event(const char* event) override
	{
		int rank;
		if ( MPI_Comm_rank(my_comm, &rank) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		
		if ( rank == 0 ) cout << "[PDI test plugin] The test plugin received an event: "<<event<<endl;
	}
	
	void data(const char* name, Ref ref) override
	{
		// store a copy of the reference because we need to keep it for notification
		auto ref_it = m_refs.emplace(ref).first;
		// register to be notified when the reference becomes invalid (on the copy we keep)
		string sname = name; // store the name in a string to reuse it
		ref_it->on_nullify([=](Ref r) {
			this->data_end(sname.c_str(), r);
		});
		
		int rank;
		if ( MPI_Comm_rank(my_comm, &rank) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		
		if ( rank == 0 ) cout << "[PDI test plugin]  =>> data becoming available to the test plugin: "<<name<<endl;
	}
	
	void data_end(const char* name, Ref r)
	{
		int rank;
		if ( MPI_Comm_rank(my_comm, &rank) ) throw Error{PDI_ERR_SYSTEM, "MPI error"};
		m_refs.erase(r);
		
		if ( rank == 0 ) cout << "[PDI test plugin]  <<= data stop being available to the test plugin: "<<name<<endl;
	}
	
}; // struct test_plugin

} // namespace <anonymous>

PDI_PLUGIN(test)
