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


#ifndef DECL_HDF5_AST_H_
#define DECL_HDF5_AST_H_

#include <mpi.h>

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <paraconf.h>

#include <pdi/datatype.h>
#include <pdi/expression.h>


namespace decl_hdf5 {

using PDI::Datatype_template_uptr;
using PDI::Expression;
using std::reference_wrapper;
using std::string;
using std::unordered_map;
using std::unordered_multimap;
using std::unique_ptr;
using std::vector;

class Selection_cfg
{
	vector<Expression> m_size;
	
	vector<Expression> m_start;
	
public:
	Selection_cfg();
	
	Selection_cfg(PC_tree_t tree);
	
	const vector<Expression>& size() const { return m_size; }
	
	const vector<Expression>& start() const { return m_start; }
	
};

class File_cfg;

class Transfer_cfg
{
	const File_cfg* m_parent;
	
	Expression m_dataset;
	
	unique_ptr<Expression> m_when;
	
	MPI_Comm m_communicator;
	
	Selection_cfg m_memory_selection;
	
	Selection_cfg m_dataset_selection;
	
public:
	Transfer_cfg(const Transfer_cfg&)=delete;
	
	Transfer_cfg(Transfer_cfg&&)=default;
	
	Transfer_cfg(const File_cfg& parent, const string& name);
	
	Transfer_cfg(const File_cfg& parent, const string& name, PC_tree_t tree);
	
	const File_cfg& parent() const { return *m_parent; }
	
	void parent(const File_cfg&);
	
	const Expression& dataset() const { return m_dataset; }
	
	const Expression& when() const;
	
	MPI_Comm communicator() const;
	
	const Selection_cfg& memory_selection() const { return m_memory_selection; }
	
	const Selection_cfg& dataset_selection() const { return m_dataset_selection; }
	
};

class File_cfg
{
	Expression m_file;
	
	Expression m_when;
	
	MPI_Comm m_communicator;
	
	unordered_map<string, Datatype_template_uptr> m_datasets;
	
	unordered_multimap<string, Transfer_cfg> m_read;
	
	unordered_multimap<string, Transfer_cfg> m_write;
	
public:
	File_cfg(const File_cfg&)=delete;
	
	File_cfg(File_cfg&&);
	
	File_cfg(PC_tree_t tree, vector<string>& events);
	
	const Expression& file() const { return m_file; }
	
	const Expression& when() const { return m_when; }
	
	MPI_Comm communicator() const { return m_communicator; }
	
	const unordered_map<string, Datatype_template_uptr>& datasets() const { return m_datasets; }
	
	const unordered_multimap<string, Transfer_cfg>& read() const { return m_read; }
	
	unordered_multimap<string, Transfer_cfg>& read() { return m_read; }
	
	const unordered_multimap<string, Transfer_cfg>& write() const { return m_write; }
	
	unordered_multimap<string, Transfer_cfg>& write() { return m_write; }
	
};

class Decl_hdf5_cfg
{
	vector<File_cfg> m_files;
	
	unordered_multimap<string, reference_wrapper<const File_cfg>> m_events;
	
	unordered_multimap<string, reference_wrapper<const Transfer_cfg>> m_write;
	
	unordered_multimap<string, reference_wrapper<const Transfer_cfg>> m_read;
	
public:
	Decl_hdf5_cfg(const Decl_hdf5_cfg&)=delete;
	
	Decl_hdf5_cfg(PC_tree_t tree);
	
	const vector<File_cfg>& files() const { return m_files; }
	
	const unordered_multimap<string, reference_wrapper<const File_cfg>>& events() const { return m_events; }
	
	const unordered_multimap<string, reference_wrapper<const Transfer_cfg>>& write() const { return m_write; }
	
	const unordered_multimap<string, reference_wrapper<const Transfer_cfg>>& read() const { return m_read; }
	
private:
	void fill_rw(vector<string>& events);
	
};

} // namespace decl_hdf5

#endif // DECL_HDF5_AST_H_
