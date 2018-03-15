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


#include <unordered_set>
#include <utility>

#include <hdf5.h>

#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>

#include "ast.h"


namespace decl_hdf5 {


using PDI::Datatype_template;
using PDI::Error;
using PDI::len;
using PDI::to_string;
using std::cref;
using std::forward_as_tuple;
using std::move;
using std::piecewise_construct;
using std::unordered_set;

Selection_cfg::Selection_cfg() = default;

Selection_cfg::Selection_cfg(PC_tree_t tree)
{
	int nb_key = len(tree);
	for (int key_id=0; key_id<nb_key; ++key_id) {
		string key = to_string(PC_get(tree, "{%d}", key_id));
		if ( key == "size" ) {
			PC_tree_t size_tree = PC_get(tree, ".size");
			if ( !PC_status(PC_get(size_tree, "[0]")) ) {
				int nb_size = len(size_tree);
				for (int size_id=0; size_id<nb_size; ++size_id) {
					m_size.emplace_back(to_string(PC_get(size_tree, "[%d]", size_id)));
				}
			} else {
				m_size.emplace_back(to_string(size_tree));
			}
		} else if ( key == "start" ) {
			PC_tree_t start_tree = PC_get(tree, ".start");
			if ( !PC_status(PC_get(start_tree, "[0]")) ) {
				int nb_start = len(start_tree);
				for (int size_id=0; size_id< nb_start; ++size_id) {
					m_start.emplace_back(to_string(PC_get(start_tree, "[%d]", size_id)));
				}
			} else {
				m_start.emplace_back(to_string(start_tree));
			}
		} else {
			throw Error{PDI_ERR_CONFIG, "Invalid configuration key in selection: `%s'", key.c_str()};
		}
	}
}

Transfer_cfg::Transfer_cfg(const File_cfg& parent, const string& name):
	m_parent{&parent},
	m_dataset{name},
	m_communicator{MPI_COMM_NULL}
{
}

Transfer_cfg::Transfer_cfg(const File_cfg& parent, const string& name, PC_tree_t tree):
	Transfer_cfg{parent, name}
{
	int nb_key = len(tree);
	for (int key_id=0; key_id<nb_key; ++key_id) {
		string key = to_string(PC_get(tree, "{%d}", key_id));
		if ( key == "dataset" ) {
			m_dataset= to_string(PC_get(tree, ".dataset"));
		} else if ( key == "when" ) {
			m_when.reset(new Expression{to_string(PC_get(tree, ".when"))});
		} else if ( key == "communicator" ) {
			string comm = to_string(PC_get(tree, ".communicator"));
			if ( comm == "self" ) {
#ifdef H5_HAVE_PARALLEL
				m_communicator = MPI_COMM_SELF;
			} else if ( comm == "world" ) {
				m_communicator = MPI_COMM_WORLD;
#endif
			} else {
				throw Error{PDI_ERR_CONFIG, "Invalid communicator: `%s'", comm.c_str()};
			}
		} else if ( key == "memory_selection" ) {
			m_memory_selection = PC_get(tree, ".memory_selection");
		} else if ( key == "dataset_selection" ) {
			m_dataset_selection = PC_get(tree, ".dataset_selection");
		} else {
			throw Error{PDI_ERR_CONFIG, "Unknown key for HDF5 transfer configuration: `%s'", key.c_str()};
		}
	}
}

void Transfer_cfg::parent(const File_cfg& parent)
{
	m_parent = &parent;
}

const Expression& Transfer_cfg::when() const
{
	if (m_when) return *m_when;
	return parent().when();
}

MPI_Comm Transfer_cfg::communicator() const
{
	if ( m_communicator == MPI_COMM_NULL ) return parent().communicator();
	return m_communicator;
}


File_cfg::File_cfg(decl_hdf5::File_cfg&& moved):
	m_file{move(moved.m_file)},
	m_when{move(moved.m_when)},
	m_communicator{moved.m_communicator},
	m_datasets{move(moved.m_datasets)},
	m_read{move(moved.m_read)},
	m_write{move(moved.m_write)}
{
	for (auto&& read: m_read) read.second.parent(*this);
	for (auto&& write: m_write) write.second.parent(*this);
}


File_cfg::File_cfg(PC_tree_t tree, vector<string>& events):
	m_file{to_string(PC_get(tree, ".file"))},
	m_when{1},
	m_communicator{MPI_COMM_SELF}
{
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
				m_datasets.emplace(to_string(PC_get(tree, ".datasets{%d}", dataset_id)), Datatype_template::load(PC_get(tree, ".datasets<%d>", dataset_id)));
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

void Decl_hdf5_cfg::fill_rw(vector<string>& events)
{
	if ( events.empty() ) {
		for ( auto&& read: m_files.back().read() ) {
			m_read.emplace(read.first, cref(read.second));
		}
		for ( auto&& write: m_files.back().write() ) {
			m_write.emplace(write.first, cref(write.second));
		}
	} else {
		for ( auto&& event: events) {
			m_events.emplace(move(event), m_files.back());
		}
	}
}


Decl_hdf5_cfg::Decl_hdf5_cfg(PC_tree_t tree)
{
	if (!PC_status(PC_get(tree, "[0]"))) {
		int nb_files = len(tree);
		for (int file_id=0; file_id<nb_files; ++file_id) {
			vector<string> events;
			m_files.emplace_back(PC_get(tree, "[%d]", file_id), events);
			fill_rw(events);
		}
	} else if (!PC_status(PC_get(tree, "{0}"))) {
		vector<string> events;
		m_files.emplace_back(tree, events);
		fill_rw(events);
	} else {
		throw Error{PDI_ERR_CONFIG, "Unexpected Decl'HDF5 configuration: `%s'", to_string(tree).c_str()};
	}
}

} // namespace decl_hdf5

