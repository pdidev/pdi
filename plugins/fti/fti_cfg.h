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

#ifndef FTI_CFG_H_
#define FTI_CFG_H_

#include <map>
#include <set>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

#include <pdi/expression.h>
#include <pdi/pdi_fwd.h>

namespace fti {

enum class Event_type {
	CHECKPOINT_L1 = 1,
	CHECKPOINT_L2 = 2,
	CHECKPOINT_L3 = 3,
	CHECKPOINT_L4 = 4,
	RECOVER,
	RECOVER_VAR,
	SNAPSHOT,
	SEND_FILE,
	INIT
};

enum class Desc_type {
	MPI_COMM,
	STATUS,
	DATA_SIZE,
	HEAD,
	STAGE_DIR,
	STAGE_STATUS
};

class Fti_cfg
{
	PDI::Expression m_config_file;
	
	std::string m_communicator;
	
	std::unordered_map<int, std::string> m_dataset;
	
	std::unordered_map<std::string, int> m_dataset_sizes;
	
	std::unordered_map<std::string, Desc_type> m_descs;
	
	std::unordered_map<std::string, Event_type> m_events;
	
	std::unordered_map<std::string, std::unordered_set<int>> m_recover_var;
	
	std::unordered_map<std::string, std::set<std::tuple<PDI::Expression, PDI::Expression, std::string>>> m_send_file;
	
	bool m_init_on_event;
	
public:
	Fti_cfg(PDI::Context& ctx, PC_tree_t tree);
	
	std::string config(PDI::Context& ctx) const;
	
	const std::string& communicator() const;
	
	const std::unordered_map<int, std::string>& dataset() const;
	
	const std::unordered_map<std::string, int>& dataset_sizes() const;
	
	const std::unordered_map<std::string, Desc_type>& descs() const;
	
	const std::unordered_map<std::string, Event_type>& events() const;
	
	bool init_on_event() const;
	
	const std::unordered_map<std::string, std::unordered_set<int>>& recover_var() const;
	
	const std::unordered_map<std::string, std::set<std::tuple<PDI::Expression, PDI::Expression, std::string>>>& send_file() const;
}; // class Fti_cfg

} // namespace fti_plugin

#endif // FTI_CFG_H_
