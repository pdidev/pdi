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

#ifndef VELOC_CFG_H_
#define VELOC_CFG_H_

#include <map>
#include <set>
#include <tuple>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <pdi/expression.h>
#include <pdi/pdi_fwd.h>

#include <veloc.h>

enum class Event_type {
	RECOVER,
    CHECKPOINT,
	RECOVER_VAR,
	STATE_SYNC,
	START_CHECKPOINT,
	ROUTE_FILE_FOR_CP,
	ROUTE_FILE_FOR_REC,
	END_CHECKPOINT,
	START_RECOVERY,
	END_RECOVERY
};

enum class Desc_type {
	STATUS,
    COUNTER_CP
};

struct ManualCheckpoint{
	std::string original_file;
	std::string routed_file;
	Event_type start_cp_on;
	Event_type route_on;
	Event_type end_cp_on; 
	// PDI::Expression when;
};


struct ManualRecovery{
	std::string original_file;
	std::string routed_file;
	Event_type start_rec_on;
	Event_type route_on;
	Event_type end_rec_on; 
	// PDI::Expression when;
};


class Veloc_cfg
{
	std::string m_config_file;

    std::string m_cp_label; 

	ManualCheckpoint m_manual_cp;

	ManualRecovery m_manual_rec;

	bool manual_cp_defined;

	bool manual_rec_defined;

    int m_failure; 

	int m_status; 

	int m_requested_checkpoint; 

    std::string m_iter_name;
	
	std::unordered_map<int, std::string> m_protected_data;
	
	std::unordered_map<std::string, Desc_type> m_descs;
	
	std::unordered_map<std::string, Event_type> m_events;

	PDI::Expression m_when;
	
	std::unordered_map<std::string, std::set<int>> m_recover_var; // event a - 1,2,3 ( = recover variables with index 1,2, and 3 in m_protected_data on "event")

	void check_conformity(PDI::Context& ctx);
	
public:
	Veloc_cfg(PDI::Context& ctx, PC_tree_t tree);
	
	std::string config()  const { return m_config_file; }

	std::string label() const { return m_cp_label; }
	
	int failure() const { return m_failure; }

	int requested_checkpoint() const { return m_requested_checkpoint; }

	std::string iter_name() const { return m_iter_name; }

	const ManualCheckpoint manual_cp() const {return m_manual_cp;}

	const ManualRecovery manual_rec() const {return m_manual_rec;}

	const PDI::Expression& when() const { return m_when; }
	
	const std::unordered_map<int, std::string>& protected_data() const {return m_protected_data; }
	
	const std::unordered_map<std::string, Desc_type>& descs() const {return m_descs;}
	
	const std::unordered_map<std::string, Event_type>& events() const {return m_events;}
	
	const std::unordered_map<std::string, std::set<int>>& recover_var() const {return m_recover_var;}
}; // class Veloc_cfg

#endif // VELOC_CFG_H_
