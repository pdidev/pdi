/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

struct ManualCheckpoint {
	bool is_valid = false; 
	std::string original_file;
	Event_type start_cp_on;
	Event_type route_on;
	Event_type end_cp_on;
};

struct ManualRecovery {
	bool is_valid = false; 
	std::string original_file;
	Event_type start_rec_on;
	Event_type route_on;
	Event_type end_rec_on;
	int requested_checkpoint = -1; 
};

// Holds all configuration parsed from the "managed-checkpointing" block.
struct ManagedCheckpointingCfg {
	bool is_valid = false; 
	std::unordered_map<int, std::string> protected_data;
	PDI::Expression when = 1L;
	int requested_checkpoint = -1; 
};

// Holds all configuration parsed from the "custom-checkpointing" block.
struct CustomCheckpointingCfg {
	bool is_valid = false; 
	std::string routed_file;     
	ManualCheckpoint manual_cp;
	ManualRecovery manual_rec;
};


class Veloc_cfg
{
	std::string m_config_file;

	std::string m_cp_label;

	std::string m_iter_name;

	int m_failure = 0;            

	ManagedCheckpointingCfg m_managed;

	CustomCheckpointingCfg m_custom;

	std::unordered_map<std::string, Desc_type> m_descs;

	std::unordered_map<std::string, Event_type> m_events;

	void check_conformity(PDI::Context& ctx);

public:
	Veloc_cfg(PDI::Context& ctx, PC_tree_t tree);

	std::string config() { return m_config_file; }

	std::string label() { return m_cp_label; }

	std::string iter_name() { return m_iter_name; }

	int failure() { return m_failure; }

	ManagedCheckpointingCfg&  managed() { return m_managed; }

	bool managed_defined() { return m_managed.is_valid; }

	CustomCheckpointingCfg& custom() { return m_custom; }

	ManualCheckpoint& manual_cp() { return m_custom.manual_cp; }

	ManualRecovery&  manual_rec() { return m_custom.manual_rec; }

	std::unordered_map<std::string, Desc_type>&  descs() { return m_descs; }

	std::unordered_map<std::string, Event_type>&  events() { return m_events; }
}; // class Veloc_cfg

#endif // VELOC_CFG_H_
