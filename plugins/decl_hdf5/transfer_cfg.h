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


#ifndef DECL_HDF5_TRANSFER_CFG_H_
#define DECL_HDF5_TRANSFER_CFG_H_

#include <mpi.h>

#include <memory>
#include <string>

#include <paraconf.h>

#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

#include "selection_cfg.h"


namespace {

class File_cfg;

class Transfer_cfg
{
	const File_cfg* m_parent;
	
	PDI::Expression m_dataset;
	
	std::unique_ptr<PDI::Expression> m_when;
	
	std::string m_communicator;
	
	Selection_cfg m_memory_selection;
	
	Selection_cfg m_dataset_selection;
	
public:
	Transfer_cfg(const Transfer_cfg&)=delete;
	
	Transfer_cfg(Transfer_cfg&&)=default;
	
	Transfer_cfg(const File_cfg& parent, const std::string& name):
		m_parent{&parent},
		m_dataset{name},
		m_communicator{"$MPI_COMM_NULL"}
	{
	}
	
	Transfer_cfg(const File_cfg& parent, const std::string& name, PC_tree_t tree):
		Transfer_cfg{parent, name}
	{
		using PDI::Error;
		using PDI::Expression;
		using PDI::len;
		using PDI::to_string;
		using std::string;
		int nb_key = len(tree);
		for (int key_id=0; key_id<nb_key; ++key_id) {
			string key = to_string(PC_get(tree, "{%d}", key_id));
			if ( key == "dataset" ) {
				m_dataset= to_string(PC_get(tree, ".dataset"));
			} else if ( key == "when" ) {
				m_when.reset(new Expression{to_string(PC_get(tree, ".when"))});
			} else if ( key == "communicator" ) {
				string m_communicator = to_string(PC_get(tree, ".communicator"));
#ifndef H5_HAVE_PARALLEL
				if (m_communicator != "$MPI_COMM_SELF") {
					throw Error{PDI_ERR_CONFIG, "Used HDF5 is not parallel. Invalid communicator: `%s'", comm_name.c_str()};
				}
#endif
			} else if ( key == "memory_selection" ) {
				m_memory_selection = PC_get(tree, ".memory_selection");
			} else if ( key == "dataset_selection" ) {
				m_dataset_selection = PC_get(tree, ".dataset_selection");
			} else {
				throw Error{PDI_ERR_CONFIG, "Unknown key for HDF5 transfer configuration: `%s'", key.c_str()};
			}
		}
	}
	
	const File_cfg& parent() const
	{
		return *m_parent;
	}
	
	void parent(const File_cfg& parent)
	{
		m_parent = &parent;
	}
	
	const PDI::Expression& dataset() const
	{
		return m_dataset;
	}
	
	// defined in File_cfg header because File_cfg need to be defined for implementation
	const PDI::Expression& when() const;
	
	// defined in File_cfg header because File_cfg need to be defined for implementation
	std::string communicator() const;
	MPI_Comm communicator(PDI::Context& ctx) const;
	
	const Selection_cfg& memory_selection() const
	{
		return m_memory_selection;
	}
	
	const Selection_cfg& dataset_selection() const
	{
		return m_dataset_selection;
	}
	
};

} // namespace <anonymous>

#endif // DECL_HDF5_TRANSFER_CFG_H_
