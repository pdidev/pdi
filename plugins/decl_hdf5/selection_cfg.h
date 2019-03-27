/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_SELECTION_CFG_H_
#define DECL_HDF5_SELECTION_CFG_H_

#include <string>
#include <vector>

#include <paraconf.h>

#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>


namespace {

class Selection_cfg
{
	std::vector<PDI::Expression> m_size;
	
	std::vector<PDI::Expression> m_start;
	
public:

	Selection_cfg() = default;
	
	Selection_cfg(PC_tree_t tree)
	{
		using PDI::Error;
		using PDI::len;
		using PDI::to_string;
		using std::string;
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
	
	const std::vector<PDI::Expression>& size() const
	{
		return m_size;
	}
	
	const std::vector<PDI::Expression>& start() const
	{
		return m_start;
	}
	
};

} // namespace <anonymous>

#endif // DECL_HDF5_SELECTION_CFG_H_
