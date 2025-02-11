/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2024 National Institute for Research in Digital Science and Technology (Inria)
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

#ifndef DAMARIS_CFG_H_
#define DAMARIS_CFG_H_

#include <string>
#include <map>
#include <set>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/pdi_fwd.h>

// Definitions of the Damaris XML tag generators
#include <Damaris.h>
#include <damaris/util/DamarisVar.hpp>
#include <damaris/model/ModifyModel.hpp>

#include "damaris_wrapper.h"

using PDI::Context;
using std::unique_ptr;
using std::list;
using std::string;

namespace damaris_pdi {

typedef struct placement {
    int start ;
    int stride ;
    int blocksize ;
    std::vector<int> mask {} ;
} placement ;

typedef struct dedicated {
    int core ;
    int node ;
} dedicated ;

struct Architecture_type {
    int domain ;
    placement arch_placement ;
    dedicated arch_cores_or_nodes ;
};

enum class Desc_type {
	 DATA_TO_WRITE
	,DATA_TO_WRITE_WITH_BLOCK
	//,DATA_TO_READ
	,PRM_REQUIRED_METADATA
	,PRM_TO_GET
	,PRM_TO_SET
};

struct Dataset_Write_Info {
    PDI::Expression when = "1";//By default, always write as long as there are iteration going on
    /*int64_t* */ PDI::Expression position[3] = {"0", "0", "0"};//Max Dim is 3
    /*int32_t */ PDI::Expression block = "0";//when domain = 1, which is the default behaviour
};


struct DamarisXMLGenerators {
    damaris::model::DamarisParameterXML params_ ;   // Layouts are typically descrived using parameters. Parameters can be shared between layouts.
    damaris::model::DamarisVarXML       variable_ ; // A variable 
    damaris::model::DamarisLayoutXML    layout_ ;   // each variable has one layout (layouts can be sahred)
    damaris::model::DamarisMeshXML      mesh_ ;     // Contains extra elements that need to be part of a Damaris <mesh> for visualization
    damaris::model::DamarisStoreXML     store_ ;    // Contains the extra elements that need to be part of a Damaris <store> (for HDF5 file w/r)
};

class Damaris_cfg
{
    std::string m_xml_config_object;      
    damaris::model::ModifyModel damarisXMLModifyModel;

    bool m_init_on_event = false;
    bool m_start_on_event = false;
    bool m_stop_on_event = false;
    
    int m_arch_domains ;
    int m_dc_cores_pernode ;
    int m_dc_nodes ;
    
    
    int m_placement_start ;
    int m_placement_stride ;
    int m_placement_blocksize ;
    std::string m_placement_mask_str ;
    
	PDI::Expression m_communicator;  
	
	std::unordered_map<std::string, damaris::model::DamarisVarXML> m_datasets;
    std::unordered_map<std::string, damaris::model::DamarisLayoutXML> m_layouts;
    std::unordered_map<std::string, damaris::model::DamarisParameterXML> m_parameters;
    std::unordered_map<std::string, damaris::model::DamarisStoreXML> m_storages;
    std::unordered_map<std::string, damaris::model::DamarisMeshXML> m_meshes;
    std::unordered_map<std::string, damaris::model::DamarisGroupXML> m_groups;
    damaris::model::DamarisParaviewXML *m_paraview = NULL;
    damaris::model::DamarisPyScriptXML *m_pyscript = NULL;

    std::unordered_map<std::string, Desc_type> m_descs ;  
    std::unordered_map<std::string, Dataset_Write_Info> m_datasets_to_write;
	list<string> m_after_write_events;  

    //In damaris parameter play similar role than metadata in PDI, in that other variable can be expressed by them
    //  Here we express parameter in terms of PDI metadata, and when metadata are expose, 
    //      we need to be awards in order to update parameter value for Damaris
    //   This variable is intended for that
    std::unordered_map<std::string, std::unordered_map<std::string, bool>> m_parameter_depends_on;
    std::unordered_map<std::string, PDI::Expression> m_parameter_expression;
    

const std::string XML_CONFIG_TEMPLATE = R"V0G0N(<?xml version="1.0"?>
<simulation name="_SIM_NAME_" language="c" xmlns="http://damaris.gforge.inria.fr/damaris/model">
    <architecture>
        <domains count="_DOMAINS_REGEX_"/>
        <dedicated cores="_DC_REGEX_" nodes="_DN_REGEX_"/>
        <buffer name="_SHMEM_NAME_" size="_SHMEM_BUFFER_BYTES_REGEX_" />
        <placement />
        <queue  name="queue" size="300" />
    </architecture>

    <data>
        _DATASET_ELEMENT_REGEX_
    </data>

    <storage>
        _STORAGE_ELEMENT_REGEX_        
    </storage>

    _PLUGINS_REGEX_

    <actions>
    </actions>

    <scripts>
    _SCRIPTS_REGEX_
    </scripts>

    <log FileName="./log/_SIM_LOG_NAME_" RotationSize="_LOG_ROTATION_SIZE_" LogFormat="[%TimeStamp%]: %Message%"  Flush="_LOG_FLUSH_"  LogLevel="_LOG_LEVEL_" />
    
</simulation>)V0G0N";


protected:
void parse_architecture_tree(Context& ctx, PC_tree_t arch_tree);
void parse_parameters_tree(Context& ctx, PC_tree_t parameters_tree_list);
void parse_datasets_tree(Context& ctx, PC_tree_t datasets_tree_list);
void parse_layouts_tree(Context& ctx, PC_tree_t layouts_tree_list);
void parse_storages_tree(Context& ctx, PC_tree_t storages_tree_list);
void parse_meshes_tree(Context& ctx, PC_tree_t meshes_tree_list);
void parse_paraview_tree(Context& ctx, PC_tree_t paraview_tree);
void parse_pyscript_tree(Context& ctx, PC_tree_t pyscript_tree);
void parse_write_tree(Context& ctx, PC_tree_t write_tree_list);
void parse_log_tree(Context& ctx, PC_tree_t config);

void init_xml_config_object(){
    m_xml_config_object = XML_CONFIG_TEMPLATE;
    damarisXMLModifyModel = damaris::model::ModifyModel(m_xml_config_object);
}

	
public:
	Damaris_cfg(PDI::Context& ctx, PC_tree_t tree);
    
	const std::string& xml_config_object( void );
	
	PDI::Expression communicator() const;
	
	const std::unordered_map<std::string, damaris::model::DamarisVarXML>& datasets() const;
    const std::unordered_map<std::string, damaris::model::DamarisLayoutXML>& layouts() const;
    const std::unordered_map<std::string, damaris::model::DamarisParameterXML>& parameters() const;
    const std::unordered_map<std::string, damaris::model::DamarisStoreXML>& storages() const;
    const std::unordered_map<std::string, damaris::model::DamarisMeshXML>& meshes() const;
    const std::unordered_map<std::string, damaris::model::DamarisGroupXML>& groups() const;

	const std::unordered_map<std::string, Desc_type>& descs() const;
	const std::unordered_map<std::string, Dataset_Write_Info>& datasets_to_write() const;

	Dataset_Write_Info get_dataset_write_info(std::string data_name) const;
	list<string> get_after_write_events() const
    {
        return m_after_write_events;
    }
	bool is_there_after_write_events() const
    {
        return m_after_write_events.size();
    }
		
	bool init_on_event() const;
	bool start_on_event() const;
	bool stop_on_event() const;
	
	const std::unordered_map<std::string, std::unordered_set<int>>& recover_var() const;
	
	const std::unordered_map<std::string, std::set<std::tuple<PDI::Expression, PDI::Expression, std::string>>>& send_file() const;


    bool is_dataset_to_write(std::string data_name);
    bool is_needed_metadata(std::string data_name);

    std::unordered_map<std::string, std::pair<std::string, std::string>> get_updatable_parameters(Context& ctx);

    void reset_parameter_depends_on(std::string prm_name);
    void reset_parameter_depends_on(std::vector<std::string> prm_list);

    void reset_all_parameters_depends_on();
}; // class Damaris_cfg

} // namespace damaris_pdi

#endif // DAMARIS_CFG_H_