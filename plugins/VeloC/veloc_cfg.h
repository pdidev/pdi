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
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

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

/** 
 * A ManualCheckpoint stores all information related to the definition 
 * of a checkpoint using VeloC file-based API 
 */
struct ManualCheckpoint {
	bool is_valid = false;
	std::string original_file;
	Event_type start_cp_on;
	Event_type route_on;
	Event_type end_cp_on;
};

/** 
 * A ManualRecovery stores all information related to the definition 
 * of a recovery using VeloC file-based API 
 */
struct ManualRecovery {
	bool is_valid = false;
	std::string original_file;
	Event_type start_rec_on;
	Event_type route_on;
	Event_type end_rec_on;
	int requested_checkpoint = -1;
};

/** 
 * A CustomCheckpointingCfg stores all information related to the 
 * definition of a checkpoint-restart process using VeloC file-based API 
 */
struct CustomCheckpointingCfg {
	bool is_valid = false;
	std::string routed_file;
	ManualCheckpoint manual_cp;
	ManualRecovery manual_rec;
};

/** 
 * A ManagedCheckpointingCfg stores all information related to the definition 
 * of a checkpoint using VeloC memory-based API 
 */
struct ManagedCheckpointingCfg {
	bool is_valid = false;
	std::unordered_map<int, std::string> protected_data;
	PDI::Expression when = 1L;
	int requested_checkpoint = -1;
};

/** 
 * A Veloc_cfg stores all information required for the correct functioning of VeloC 
 */
class Veloc_cfg
{
	/// The path of VeloC configuration file 
	std::string m_config_file;

	/// The subtree defining the behaviour of the plugin 
	PC_tree_t m_tree;

	/// The label common to all checkpoints' names of a simulation 
	std::string m_cp_label;

	/// The name of the iterator in the PDI data store   
	std::string m_iter_name;

	/// The configuration of a managed checkpoint
	ManagedCheckpointingCfg m_managed;

	/// The configuration of a custom checkpoint 
	CustomCheckpointingCfg m_custom;

	/// Collection of descriptors keyed by type
	std::unordered_map<std::string, Desc_type> m_descs;

	/// Collection of events keyed by type
	std::unordered_map<std::string, Event_type> m_events;

	/**
	 * @brief checks that all information stored is coherent 
	 * 
	 * @param ctx the context from which to access the logger
	 */
	void check_conformity(PDI::Context& ctx);

public:
	Veloc_cfg(PDI::Context& ctx, PC_tree_t tree);

	/**
	 * @brief returns the subtree defining the behaviour of the plugin 
	 * 
	 * @return PC_tree_t subtree defining the behaviour of the plugin 
	 */
	PC_tree_t tree(){ return m_tree; };

	/**
	 * @brief returns the path to VeloC's configuration file
	 * 
	 * @return std::string VeloC's configuration file path
	 */
	std::string config() { return m_config_file; }

 	/**
     * @brief returns the common checkpoint label
     *
     * @return std::string the commong checkpoint label
     */
    std::string label() { return m_cp_label; }

    /**
     * @brief returns the iterator name in the PDI data store
     *
     * @return std::string the iterator name in the PDI data store
     */
    std::string iter_name() { return m_iter_name; };

    /**
     * @brief returns the managed checkpoint configuration
     *
     * @return the managed checkpoint configuration
     */
    ManagedCheckpointingCfg& managed() { return m_managed; }

    /**
     * @brief returns whether a managed checkpointing behaviour is configured.
     *
     * @return true if managed checkpointing is defined, false otherwise.
     */
    bool managed_defined() { return m_managed.is_valid; }

    /**
     * @brief returns the custom checkpoint-recover configuration
     *
     * @return the custom checkpoint-recover configuration
     */
    CustomCheckpointingCfg& custom() { return m_custom; }

    /**
     * @brief returns the custom checkpoint configuration
     *
     * @return the manual checkpoint configuration
     */
    ManualCheckpoint& manual_cp() { return m_custom.manual_cp; }

    /**
     * @brief returns the custom recovery configuration
     *
     * @return the custom recovery configuration
     */
    ManualRecovery& manual_rec() { return m_custom.manual_rec; }

    /**
     * @brief returns the descriptor map
     *
     * @return the map of descriptors keyed by name
     */
    std::unordered_map<std::string, Desc_type>& descs() { return m_descs; }

    /**
     * @brief returns the event map
     *
     * @return the map of events keyed by name
     */
    std::unordered_map<std::string, Event_type>& events() { return m_events; }
};

// class Veloc_cfg

#endif // VELOC_CFG_H_
