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

#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <paraconf.h>

#include <pdi/datatype.h>
#include <pdi/expression.h>

#include "file_cfg.h"


namespace {

class Decl_hdf5_cfg
{
	std::vector<File_cfg> m_files;
	
	std::unordered_multimap<std::string, std::reference_wrapper<const File_cfg>> m_events;
	
	std::unordered_multimap<std::string, std::reference_wrapper<const Transfer_cfg>> m_write;
	
	std::unordered_multimap<std::string, std::reference_wrapper<const Transfer_cfg>> m_read;
	
	Decl_hdf5_cfg(const Decl_hdf5_cfg&)=delete;
	
public:
	Decl_hdf5_cfg(PC_tree_t tree, PDI::Logger logger)
	{
		using PDI::Error;
		using PDI::len;
		using PDI::to_string;
		using std::string;
		using std::vector;
		if (!PC_status(PC_get(tree, "[0]"))) {
			int nb_files = len(tree);
			for (int file_id=0; file_id<nb_files; ++file_id) {
				vector<string> events;
				m_files.emplace_back(PC_get(tree, "[%d]", file_id), events, logger);
				fill_rw(events);
			}
		} else if (!PC_status(PC_get(tree, "{0}"))) {
			vector<string> events;
			m_files.emplace_back(tree, events, logger);
			fill_rw(events);
		} else {
			throw Error{PDI_ERR_CONFIG, "Unexpected Decl'HDF5 configuration: `%s'", to_string(tree).c_str()};
		}
	}
	
	const std::vector<File_cfg>& files() const
	{
		return m_files;
	}
	
	const std::unordered_multimap<std::string, std::reference_wrapper<const File_cfg>>& events() const
	{
		return m_events;
	}
	
	const std::unordered_multimap<std::string, std::reference_wrapper<const Transfer_cfg>>& write() const
	{
		return m_write;
	}
	
	const std::unordered_multimap<std::string, std::reference_wrapper<const Transfer_cfg>>& read() const
	{
		return m_read;
	}
	
private:
	void fill_rw(std::vector<std::string>& events)
	{
		using std::cref;
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
	
};

} // namespace <anonymous>

#endif // DECL_HDF5_AST_H_
