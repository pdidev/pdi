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


#ifndef DECL_HDF5_FILE_CFG_H_
#define DECL_HDF5_FILE_CFG_H_

#include <mpi.h>

#include <functional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <paraconf.h>

#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>

#include "transfer_cfg.h"


namespace {

class File_cfg
{
	PDI::Expression m_file;
	
	PDI::Expression m_when;
	
	MPI_Comm m_communicator;
	
	std::unordered_map<std::string, PDI::Datatype_template_uptr> m_datasets;
	
	std::unordered_multimap<std::string, Transfer_cfg> m_read;
	
	std::unordered_multimap<std::string, Transfer_cfg> m_write;
	
	
	File_cfg(const File_cfg&)=delete;
	
public:
	File_cfg(PC_tree_t tree, std::vector<std::string>& events, PDI::Logger_sptr logger):
		m_file{PDI::to_string(PC_get(tree, ".file"))},
		m_when{1},
		m_communicator{MPI_COMM_SELF}
	{
		using PDI::Datatype_template;
		using PDI::Error;
		using PDI::len;
		using PDI::to_string;
		using std::cref;
		using std::piecewise_construct;
		using std::string;
		int nb_key = len(tree);
		for (int key_id=0; key_id<nb_key; ++key_id) {
			string key = to_string(PC_get(tree, "{%d}", key_id));
			if ( key == "file" ) {
				string f = to_string(PC_get(tree, ".file"));
			} else if ( key == "on_event" ) {
				PC_tree_t event_tree = PC_get(tree, ".on_event");
				if ( !PC_status(PC_get(event_tree, "[0]")) ) {
					int nb_event = len(event_tree);
					for (int event_id=0; event_id<nb_event; ++event_id) {
						events.emplace_back(to_string(PC_get(event_tree, "[%d]", event_id)));
					}
				} else {
					events.emplace_back(to_string(event_tree));
				}
			} else if ( key == "when" ) {
				m_when = to_string(PC_get(tree, ".when"));
			} else if ( key == "communicator" ) {
				string comm = to_string(PC_get(tree, ".communicator"));
				if ( comm == "self" ) {
#ifdef H5_HAVE_PARALLEL
					m_communicator = MPI_COMM_SELF;
				} else if ( comm == "world" ) {
					m_communicator = MPI_COMM_WORLD;
#endif
				} else {
					throw Error{PDI_ERR_CONFIG, "Invalid communicator: %s", comm.c_str()};
				}
			} else if ( key == "datasets" ) {
				int nb_dataset = len(PC_get(tree, ".datasets"));
				for (int dataset_id=0; dataset_id<nb_dataset; ++dataset_id) {
					m_datasets.emplace(to_string(PC_get(tree, ".datasets{%d}", dataset_id)), Datatype_template::load(PC_get(tree, ".datasets<%d>", dataset_id), logger));
				}
			} else if ( key == "write" ) {
				PC_tree_t write_tree = PC_get(tree, ".write");
				int nb_data = len(write_tree);
				if ( !PC_status(PC_get(write_tree, "[0]")) ) {
					for (int data_id=0; data_id<nb_data; ++data_id) {
						string data = to_string(PC_get(write_tree, "[%d]", data_id));
						m_write.emplace(piecewise_construct,
						    forward_as_tuple(data),
						    forward_as_tuple(cref(*this), data)
						);
					}
				} else {
					for (int data_id=0; data_id<nb_data; ++data_id) {
						string data = to_string(PC_get(write_tree, "{%d}", data_id));
						PC_tree_t data_tree = PC_get(write_tree, "<%d>", data_id);
						if (!PC_status(PC_get(data_tree, "[0]", data_id))) {
							auto&& insert_iter = m_write.begin();
							int nb_write = len(data_tree);
							for (int write_id=0; write_id<nb_write; ++write_id) {
								insert_iter = m_write.emplace_hint(insert_iter, piecewise_construct,
								        forward_as_tuple(data),
								        forward_as_tuple(cref(*this), data, PC_get(data_tree, "[%d]", write_id))
								    );
							}
						} else {
							m_write.emplace(piecewise_construct,
							    forward_as_tuple(data),
							    forward_as_tuple(cref(*this), data, data_tree)
							);
						}
					}
				}
			} else if ( key == "read" ) {
				PC_tree_t read_tree = PC_get(tree, ".read");
				int nb_data = len(read_tree);
				if ( !PC_status(PC_get(read_tree, "[0]")) ) {
					for (int data_id=0; data_id<nb_data; ++data_id) {
						string data = to_string(PC_get(read_tree, "[%d]", data_id));
						m_read.emplace(piecewise_construct,
						    forward_as_tuple(data),
						    forward_as_tuple(cref(*this), data)
						);
					}
				} else {
					for (int data_id=0; data_id<nb_data; ++data_id) {
						string data = to_string(PC_get(read_tree, "{%d}", data_id));
						PC_tree_t data_tree = PC_get(read_tree, "<%d>", data_id);
						if (!PC_status(PC_get(data_tree, "[0]", data_id))) {
							auto&& insert_iter = m_read.begin();
							int nb_read = len(data_tree);
							for (int read_id=0; read_id<nb_read; ++read_id) {
								insert_iter = m_read.emplace_hint(insert_iter, piecewise_construct,
								        forward_as_tuple(data),
								        forward_as_tuple(cref(*this), data, PC_get(data_tree, "[%d]", read_id))
								    );
							}
						} else {
							m_read.emplace(piecewise_construct,
							    forward_as_tuple(data),
							    forward_as_tuple(cref(*this), data, data_tree)
							);
						}
					}
				}
			} else {
				throw Error{PDI_ERR_CONFIG, "Unknown key for HDF5 file configuration: `%s'", key.c_str()};
			}
		}
	}
	
	File_cfg(File_cfg&& moved):
		m_file{std::move(moved.m_file)},
		m_when{std::move(moved.m_when)},
		m_communicator{moved.m_communicator},
		m_datasets{std::move(moved.m_datasets)},
		m_read{std::move(moved.m_read)},
		m_write{std::move(moved.m_write)}
	{
		for (auto&& read: m_read) read.second.parent(*this);
		for (auto&& write: m_write) write.second.parent(*this);
	}
	
	const PDI::Expression& file() const
	{
		return m_file;
	}
	
	const PDI::Expression& when() const
	{
		return m_when;
	}
	
	MPI_Comm communicator() const
	{
		return m_communicator;
	}
	
	const std::unordered_map<std::string, PDI::Datatype_template_uptr>& datasets() const
	{
		return m_datasets;
	}
	
	const std::unordered_multimap<std::string, Transfer_cfg>& read() const
	{
		return m_read;
	}
	
	std::unordered_multimap<std::string, Transfer_cfg>& read()
	{
		return m_read;
	}
	
	const std::unordered_multimap<std::string, Transfer_cfg>& write() const
	{
		return m_write;
	}
	
	std::unordered_multimap<std::string, Transfer_cfg>& write()
	{
		return m_write;
	}
	
};


// defined here because File_cfg need to be defined
const PDI::Expression& Transfer_cfg::when() const
{
	if (!m_when) return parent().when();
	return *m_when;
}

// defined here because File_cfg need to be defined
MPI_Comm Transfer_cfg::communicator() const
{
	if ( m_communicator == MPI_COMM_NULL ) return parent().communicator();
	return m_communicator;
}

} // namespace <anonymous>

#endif // DECL_HDF5_FILE_CFG_H_
