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

 #include <iostream>
 #include <sstream>
 #include <assert.h>
 #include <list>
 #include <pdi/context.h>
 #include <pdi/error.h>
 #include <pdi/expression.h>
 #include <pdi/plugin.h>
 #include <pdi/paraconf_wrapper.h>
 #include <pdi/ref_any.h>
 #include <bits/stdc++.h>
 
 #include "damaris_cfg.h"
 
 using PDI::Context;
 using PDI::each;
 using PDI::opt_each;
 using PDI::Config_error;
 using PDI::Value_error;
 using PDI::Expression;
 using PDI::Impl_error;
 using PDI::Ref;
 using PDI::Ref_w;
 using PDI::Ref_r;
 using PDI::len;
 using PDI::to_long;
 using PDI::to_double;
 using PDI::to_string;
 using std::function;
 using std::list;
 using std::map;
 using std::string;
 using std::unique_ptr;
 using std::unordered_map;
 
 
 
 namespace damaris_pdi {
 
 namespace {
         
    bool load_desc(unordered_map<string, Desc_type>& descs, Context& ctx, const string& name, Desc_type desc_type)
    {        
        auto&& result = descs.emplace(name, desc_type);
        if (!result.second) {
            //ctx.logger().warn("Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')", name, desc_names.at(desc_type), desc_names.at(result.first->second));
        }
        return result.second;
    }
         
     bool load_event(unordered_map<string, Event_type>& events, Context& ctx, const string& name, Event_type event_type)
     {        
         auto&& result = events.emplace(name, event_type);
         if (!result.second) {
             //ctx.logger().warn("Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')", name, desc_names.at(desc_type), desc_names.at(result.first->second));
         }
         return result.second;
     }
 
 } // namespace <anonymous>
 
     /**
     *  This variable contains Damaris xml config nested groups, with dataset elements to display in the XML config 
     *       It is possible to have nested groups, but in most of the cases, 
     *       we just have root groups containing directly the dataset elements
     */
     std::vector<damaris::model::DamarisGroupXML> root_groups_xml;
 
     // Array to store the nested groups names
     const unsigned max_nested_groups = 5;
     std::string nested_groups_names[max_nested_groups];
     char ds_elt_full_name_delimiter = '/';
 
     void retrive_nested_groups(std::string& dataset_elt_full_name, char delimiter, std::string nested_groups_names[], unsigned& index);
     template <typename DS_TYPE>
     void insert_dataset_elts_to_group(DS_TYPE varxml, std::string nested_groups_names[], unsigned index);
     //void insert_dataset_elts_to_group(damaris::model::DamarisVarXML varxml, std::string nested_groups_names[], unsigned index);
     
     std::string numbers_types[] = {"short", "int", "integer", "float", "real", "double"};
     std::string int_numbers_types[3]  = {"short", "int", "integer"};
     std::string real_numbers_types[3] = {"float", "real", "double"};
 
 // This constructor is called on the construction of the damaris_plugin struct/object
 // This code is a mapping of the Damaris src/model/Model.xsd schema to PDI YAML
 // The mapping should allow us to recreate a Damaris XML object from what is present 
 // in the YML file
 Damaris_cfg::Damaris_cfg(Context& ctx, PC_tree_t tree)
     :m_communicator{"MPI_COMM_WORLD"}
 {
     init_xml_config_object();
     
     each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
         string key = to_string(key_tree);
         if (key == "architecture") {
             //default_when = to_string(value);
             parse_architecture_tree(ctx, value);
         } 
         else if (key == "communicator") {
 
             m_communicator = to_string(value);
             if (!m_communicator) {
                 throw Config_error{key_tree, "no MPI communicator set", key};
             }
         } 
         else if (key == "init_on_event" || key == "on_init") {
               m_init_on_event = to_string(value);
               load_event(m_events, ctx, m_init_on_event, Event_type::DAMARIS_INITIALIZE);
         } 
         else if (key == "finalize_on_event" || key == "on_finalize") {
            m_finalize_on_event = to_string(value);
               load_event(m_events, ctx, m_finalize_on_event, Event_type::DAMARIS_FINALIZE);
         } 
         else if (key == "start_on_event") {
             m_start_on_event = to_string(value);
             load_event(m_events, ctx, m_start_on_event, Event_type::DAMARIS_START);
         } 
         else if (key == "stop_on_event") {
             m_stop_on_event = to_string(value);
             load_event(m_events, ctx, m_stop_on_event, Event_type::DAMARIS_STOP);
         } 
         else if (key == "end_iteration_on_event") {
             m_end_iteration_on_event = to_string(value);
             load_event(m_events, ctx, m_end_iteration_on_event, Event_type::DAMARIS_END_ITERATION);
         } 
         else if (key == "parameters") {                
            parse_parameters_tree(ctx, value);
         } 
         else if (key == "datasets") {                
             parse_datasets_tree(ctx, value);
         } 
         else if (key == "layouts") {                
             parse_layouts_tree(ctx, value);
         } 
         else if (key == "meshes") {                
             parse_meshes_tree(ctx, value);
         } 
         else if (key == "storages") {                
             parse_storages_tree(ctx, value);
         }
         else if (key == "paraview") {                
             parse_paraview_tree(ctx, value);
         }
         else if (key == "pyscript") {                
             parse_pyscript_tree(ctx, value);
         }
 
         else if (key == "write") {                
             parse_write_tree(ctx, value);
         }
 
         else if (key == "parameter_get") {                
             parse_parameter_to_update_tree(ctx, value, Desc_type::PRM_TO_GET);
         }
 
         else if (key == "parameter_set") {                
             parse_parameter_to_update_tree(ctx, value, Desc_type::PRM_TO_SET);
         }
 
         else if (key == "after_write") {     
             if(!PC_status(value))
             {
                 if (!PC_status(PC_get(value, "[0]"))) {//Array [ev0,ev1,ev3]
                     each(value, [&](PC_tree_t event_name) {
                         m_after_write_events.emplace_back(to_string(event_name));
                     });
                 }                    
                 else {//ev0
                     m_after_write_events.emplace_back(to_string(value)); 
                 }
             }
         }
 
         else if (key == "start" || key == "get_is_client" || key == "is_client_get") {     
             if(!PC_status(value))
             {
                 m_is_client_dataset_name = to_string(value); 
                 load_desc(m_descs, ctx, m_is_client_dataset_name, Desc_type::IS_CLIENT_GET);
             }
         }
 
         else if (key == "client_comm_get") {     
             if(!PC_status(value))
             {
                 m_client_comm_get_dataset_name = to_string(value); 
                 load_desc(m_descs, ctx, m_client_comm_get_dataset_name, Desc_type::CLIENT_COMM_GET);
             }
         }
         /*else {
             throw Config_error{key_tree, "Unknown key in Damaris configuration: `{}'", key};
         }*/
     });

     std::string end_it_event_name = event_names.at(Event_type::DAMARIS_END_ITERATION);
     //Add only if it does not exist yet
     if(std::find(m_after_write_events.begin(), m_after_write_events.end(), end_it_event_name) == m_after_write_events.end()
            && m_end_iteration_on_event.empty()){
        m_after_write_events.emplace_back(end_it_event_name); 
     }
 
     parse_log_tree(ctx, tree);
 
     //Insert grouped dataset elements
     for (int root_group_id = 0; root_group_id < root_groups_xml.size(); root_group_id++) {
         damaris::model::DamarisGroupXML root_gp_xml = root_groups_xml[root_group_id];
 
         m_groups.emplace(root_gp_xml.get_name() , root_gp_xml) ;
         std::map<std::string, std::string> find_replace_map = 
         {
             {"_DATASET_ELEMENT_REGEX_", root_gp_xml.ReturnXMLForGroup() + "\n_DATASET_ELEMENT_REGEX_"}
         };
         damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
     }
 
     //CleanUp XML OBJECT
     std::map<std::string, std::string> find_replace_map =     
     {
              {"_DATASET_ELEMENT_REGEX_", ""}
             ,{"_STORAGE_ELEMENT_REGEX_", ""}
             ,{"_PLUGINS_REGEX_", ""}
             ,{"_SCRIPTS_REGEX_", ""}
     };
     damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
 
     m_xml_config_object = damarisXMLModifyModel.GetConfigString();
 
    printf("-------------------------------------------------------XML OBJECT MODIFIED----------------------------------------------\n%s", damarisXMLModifyModel.GetConfigString().c_str());
    //exit(0);
 }
 
 void Damaris_cfg::parse_architecture_tree(Context& ctx, PC_tree_t arch_tree){
 
     std::map<std::string, std::string> find_replace_map = {};
 
     //simulation name & Buffer
     std::string sim_name = "damaris_pdi_simu";
     PC_tree_t sim_name_tree = PC_get(arch_tree, ".sim_name");
     if(!PC_status(sim_name_tree))
     {
         sim_name = to_string(sim_name_tree);
     }
     //Buffer
     std::string buffer_name = sim_name + "_buffer";
     long buffer_size = 67108864;
     PC_tree_t buffer_size_tree = PC_get(arch_tree, ".buffer_size");
     if(!PC_status(buffer_size_tree))
     {
         buffer_size = to_long(buffer_size_tree);
     }
     find_replace_map.insert({
              {"_SIM_NAME_",sim_name}
             ,{"_SHMEM_BUFFER_BYTES_REGEX_", std::to_string(buffer_size)}
             ,{"_SHMEM_NAME_", buffer_name}
     });
 
     //domains
     int m_arch_domains = 1;
     PC_tree_t domains_tree = PC_get(arch_tree, ".domains");
     if(!PC_status(domains_tree))
     {
         m_arch_domains = to_long(domains_tree);
     }
     find_replace_map.insert({"_DOMAINS_REGEX_", std::to_string(m_arch_domains)});
 
     //dedicated
     int m_dc_cores_pernode = 0;
     int m_dc_nodes         = 0;
     PC_tree_t arch_dedicated_tree = PC_get(arch_tree, ".dedicated");
     if(!PC_status(arch_dedicated_tree))
     {
         int nb_subkey_dc = len(arch_dedicated_tree);
                     
         for (int subkey_dc_id = 0; subkey_dc_id < nb_subkey_dc; subkey_dc_id++) {
             string key_str = to_string(PC_get(arch_dedicated_tree, "{%d}", subkey_dc_id));
             
             if (key_str == "core") {
                 m_dc_cores_pernode = to_long(PC_get(arch_dedicated_tree, ".core"));
             } else if (key_str == "node") {
                 m_dc_nodes = to_long(PC_get(arch_dedicated_tree, ".node"));
             }
         } 
     }
     find_replace_map.insert(
         {
             {"_DC_REGEX_", std::to_string(m_dc_cores_pernode)}
             ,{"_DN_REGEX_", std::to_string(m_dc_nodes)}
         }
     );
 
     //placement not yet used
     PC_tree_t arch_placement_tree = PC_get(arch_tree, ".placement");
     if(!PC_status(arch_placement_tree))
     {
         int nb_subkey_dc = len(arch_placement_tree);
                     
         for (int subkey_pl_id = 0; subkey_pl_id < nb_subkey_dc; subkey_pl_id++) {
             string key_str = to_string(PC_get(arch_placement_tree, "{%d}", subkey_pl_id));
             
             if (key_str == "start") {
                 m_placement_start = to_long(PC_get(arch_placement_tree, ".start"));
             } 
             else if (key_str == "stride") {
                 m_placement_stride = to_long(PC_get(arch_placement_tree, ".stride"));
             } 
             else if (key_str == "blocksize") {
                 m_placement_blocksize = to_long(PC_get(arch_placement_tree, ".blocksize"));
             } 
             else if (key_str == "mask") {
                 m_placement_mask_str = to_string(PC_get(arch_placement_tree, ".mask"));
             }
         }
         //TODO: find_replace_map.insert(   );
     }
 
     //Update the xml config
     damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
 }
 
 void Damaris_cfg::parse_parameters_tree(Context& ctx, PC_tree_t parameters_tree_list)
 {   
     opt_each(parameters_tree_list, [&](PC_tree_t parameters_tree) {//each parameters (list of parameter)    
         each(parameters_tree, [&](PC_tree_t prm_tree_key, PC_tree_t parameter_tree) {//each parameter
             
             damaris::model::DamarisParameterXML prmxml{} ;
             std::map<std::string, std::string> find_replace_map = {};
             std::unordered_map<std::string, bool> depends_on_metadata;
             bool is_dependent = false;
             
             each(parameter_tree, [&](PC_tree_t prm_key, PC_tree_t value) {//parameter info
                 std::string key = to_string(prm_key);
                 
                 if (key == "name") {
                     prmxml.param_name_ = to_string(value);
                 } 
                 else if (key == "type") {
                     prmxml.param_datatype_ = to_string(value);
                 } 
                 else if (key == "value") {
                     prmxml.param_value_ = to_string(value);
                     m_parameter_expression.emplace(prmxml.param_name_, to_string(value));
                 } 
                 else if (key == "depends_on") {
                     is_dependent = true;
                     if (!PC_status(PC_get(value, "[0]"))) {//Array //[d1,d2,d3] for instance, each di an expreession of of Damaris Parameter
                         //int idx = 0;
                         each(value, [&](PC_tree_t metadata_name_tree) {
                             std::string metadata_name = to_string(metadata_name_tree);
                             
                             depends_on_metadata.insert(
                                 {metadata_name, false}
                             );
 
                             load_desc(m_descs, ctx, metadata_name, Desc_type::PRM_REQUIRED_METADATA);
                         });
                     }                    
                     else {//d1
                         std::string metadata_name = to_string(value);   
                         //depends_on_metadata[metadata_name] = false;
                         depends_on_metadata.insert(
                             {metadata_name, false}
                         );
                             
                         load_desc(m_descs, ctx, metadata_name, Desc_type::PRM_REQUIRED_METADATA);
                        ctx.logger().info("--------------------------------------------------------------------------------PARAMETER {} depends on {}", prmxml.param_name_, metadata_name);
                     }
 
 
                 }  
                 else {
                     std::cerr << "ERROR: damaris_cfg unrecogognized parameter map string: " << key << std::endl ;
                 }
             });
             //m_parameter_expression.emplace(prmxml.param_name_, prmxml.param_value_);
             m_parameter_depends_on.emplace(prmxml.param_name_, depends_on_metadata);
             //set default value if depend on metadata
             if(is_dependent && std::find(std::begin(numbers_types), std::end(numbers_types), prmxml.param_datatype_) != std::end(numbers_types)) {
                if(std::find(std::begin(int_numbers_types), std::end(int_numbers_types), prmxml.param_datatype_) != std::end(int_numbers_types))
                    prmxml.param_value_ = "1"; //"0"
                else
                    prmxml.param_value_ = "1.0"; //"0"
             }
             else {
                 ///???
             }
 
             m_parameters.emplace(prmxml.param_name_ , prmxml) ;
             find_replace_map.insert(
                     {"_DATASET_ELEMENT_REGEX_", prmxml.ReturnXMLForParameter() + "\n_DATASET_ELEMENT_REGEX_"}
             );
 
             //Update the xml config
             damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
         });
     });
 }
 
 void Damaris_cfg::parse_datasets_tree(Context& ctx, PC_tree_t datasets_tree_list)
 {    
     opt_each(datasets_tree_list, [&](PC_tree_t datasets_tree) {//each datasets (list of dataset)    
         each(datasets_tree, [&](PC_tree_t ds_tree_key, PC_tree_t dataset_tree) {//each dataset
 
             damaris::model::DamarisVarXML vxml{} ;
             std::map<std::string, std::string> find_replace_map = {};
             unsigned name_index = 0;
             std::string dataset_elt_full_name;
 
             each(dataset_tree, [&](PC_tree_t ds_key, PC_tree_t value) {//dataset info
                 std::string key = to_string(ds_key);
 
                 int tf; // 0 is false, anything else is true
                 if (key == "name") {
                     dataset_elt_full_name = to_string(value); 
                     retrive_nested_groups(dataset_elt_full_name
                     , ds_elt_full_name_delimiter
                     , nested_groups_names
                     , name_index);
 
                     vxml.var_name_ = nested_groups_names[name_index-1]; //to_string(value);
                 } 
                 else if (key == "layout") {
                     vxml.layout_name_ = to_string(value);
                 } 
                 else if (key == "mesh") {
                     vxml.mesh_ = to_string(value);
                 } 
                 else if (key == "centering") {
                     std::string t_centering = to_string(value);
                     vxml.set_centering(t_centering) ;
                 } 
                 else if (key == "storage") {
                     vxml.store_ = to_string(value);
                 } 
                 else if (key == "script") {
                     vxml.script_ = to_string(value);
                 } 
                 else if (key == "unit") {
                     vxml.unit_ = to_string(value);
                 } 
                 else if (key == "select_mem") {
                     vxml.select_mem_ = to_string(value);
                 } 
                 else if (key == "select_file") {
                     vxml.select_file_ = to_string(value);
                 } 
                 else if (key == "select_subset") {
                     vxml.select_subset_ = to_string(value);
                 } 
                 else if (key == "ref") {
                     vxml.ref_ = to_string(value);
                 } 
                 else if (key == "type") {
                     std::string in_type = to_string(value);
                     vxml.set_type( in_type );
                 } 
                 else if (key == "comment") {
                     vxml.comment_ = to_string(value);
                 } 
                 else if (key == "visualizable") {
                     PC_bool(value, &tf);
                     vxml.visualizable_ = (tf == 0) ?  false : true ;
                 } 
                 else if (key == "time_varying") {
                     PC_bool(value, &tf);
                     vxml.time_varying_ = (tf == 0) ?  false : true ;
                 } 
                 else if (key == "enabled") {
                     PC_bool(value, &tf);
                     vxml.enabled_ = (tf == 0) ?  false : true ;
                 } 
                 else {
                     std::cerr << "ERROR: damaris_cfg unrecogognized variable map string: " << key << std::endl ;
                 }
             });
 
             if(dataset_elt_full_name.empty())
                 throw Value_error{"ERROR: damaris_cfg variable name must not be empty"};
 
             //m_datasets.emplace(vxml.var_name_ , vxml) ;
             m_datasets.emplace(dataset_elt_full_name , vxml) ;
             ctx.logger().info("------------------- Parsing damaris Variable '{}' , name_index = {} ", dataset_elt_full_name, name_index);
 
             if(name_index == 1) {
                 find_replace_map.insert(
                         {"_DATASET_ELEMENT_REGEX_", vxml.ReturnXMLForVariable() + "\n_DATASET_ELEMENT_REGEX_"}
                 );
 
                 //Update the xml config
                 damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
             }
             else {//In nested groups
                 insert_dataset_elts_to_group(vxml, nested_groups_names, name_index);    
             }
         });
     });
 }
 
 void Damaris_cfg::parse_layouts_tree(Context& ctx, PC_tree_t layouts_tree_list)
 {
     opt_each(layouts_tree_list, [&](PC_tree_t layouts_tree) {//each layouts (list of layout)    
         each(layouts_tree, [&](PC_tree_t lts_key, PC_tree_t layout_tree) {//each layout
 
             damaris::model::DamarisLayoutXML layoutxml{} ;
             std::map<std::string, std::string> find_replace_map = {};
             unsigned name_index = 0;
             std::string dataset_elt_full_name;
             std::unordered_map<std::string, bool> depends_on_metadata;
             bool is_dependent = false;
             std::string depends_on_str = "";
             std::string in_datatype;
             int nb_dims;
 
             each(layout_tree, [&](PC_tree_t lt_key, PC_tree_t value) {//layout info
                 std::string key = to_string(lt_key);
 
                 int intb; // 0 is false, anything else is true
                 if (key == "name") {
                     dataset_elt_full_name = to_string(value); 
                     retrive_nested_groups(dataset_elt_full_name
                     , ds_elt_full_name_delimiter
                     , nested_groups_names
                     , name_index);
 
                     layoutxml.layout_name_ = nested_groups_names[name_index-1]; //to_string(value);
                 } 
                 else if (key == "type") {
                     in_datatype = to_string(value);
                     layoutxml.set_datatype( in_datatype );
                 } 
                 else if (key == "dimensions") {                   
 
                     //Is there a way to determine if an expression is ready to be evaluated? ei, all the conponent have a value
                     if (!PC_status(PC_get(value, "[0]"))) {//Array //[d1,d2,d3] for instance, each di an expreession of of Damaris Parameter
                         int nb_layout_dims2; PC_len(value, &nb_layout_dims2);
                         std::cout << "INFO: damaris_cfg nb_layout_dims has dims2: " << nb_layout_dims2 << std::endl ;
                         
                         std::string dims_list = "";
                         each(value, [&](PC_tree_t dim) {
                             dims_list += to_string(dim) + ",";
                         });
                         dims_list.pop_back();
                         layoutxml.layout_dimensions_ = dims_list;
                     }                    
                     else {//"d1,d2,d3" for instance, each di an expreession of of Damaris Parameter
                         layoutxml.layout_dimensions_ = to_string(value);   
                     }
                     
                     nb_dims = count(layoutxml.layout_dimensions_.begin(),layoutxml.layout_dimensions_.end(),',') + 1;
                 } 
                 else if (key == "global") {  
                    //Is there a way to determine if an expression is ready to be evaluated? ei, all the conponent have a value
                    if (!PC_status(PC_get(value, "[0]"))) {//Array //[dg1,dg2,dg3] for instance, each di an expreession of of Damaris Parameter
                        
                        std::string dims_global_list = "";
                        each(value, [&](PC_tree_t dim) {
                            dims_global_list += to_string(dim) + ",";
                        });
                        dims_global_list.pop_back();
                        layoutxml.layout_dims_global_ = dims_global_list;
                    }                    
                    else {//"dg1,dg2,dg3" for instance, each di an expreession of of Damaris Parameter
                        layoutxml.layout_dims_global_ = to_string(value);
                    }
                 } 
                 else if (key == "ghosts") {
                    //Is there a way to determine if an expression is ready to be evaluated? ei, all the conponent have a value
                    if (!PC_status(PC_get(value, "[0]"))) {//Array //['g11:g12','g21:g22','g31:g32'] for instance, each di an expreession of of Damaris Parameter
                        
                        std::string ghosts_list = "";
                        each(value, [&](PC_tree_t dim) {
                            ghosts_list += to_string(dim) + ",";
                        });
                        ghosts_list.pop_back();
                        layoutxml.layout_ghosts_ = ghosts_list;
                    }                    
                    else {//"g11:g12,g21:g22,g31:g32" for instance, each di an expreession of of Damaris Parameter
                        layoutxml.layout_ghosts_ = to_string(value);
                    }
                 } 
                 else if (key == "language") {
                     std::string in_language = to_string(value);
                     layoutxml.set_language( in_language );
                 } 
                 else if (key == "visualizable") {
                     PC_bool(value, &intb);
                     bool in_visualizable = (intb == 0) ?  false : true ;
                     layoutxml.layout_visualizable_ = in_visualizable;
                 } 
                 else if (key == "comment") {
                     layoutxml.layout_comment_ = to_string(value);
                 } 
                 else if (key == "depends_on") {
                    is_dependent = true;
                    if (!PC_status(PC_get(value, "[0]"))) {//Array //[d1,d2,d3] for instance, each di an expreession of of Damaris Parameter
                         //int idx = 0;
                         each(value, [&](PC_tree_t metadata_name_tree) {
                             std::string metadata_name = to_string(metadata_name_tree);
                             depends_on_str += metadata_name+", ";
                             
                             depends_on_metadata.insert(
                                 {metadata_name, false}
                             );
 
                             //load_desc(m_descs, ctx, metadata_name, Desc_type::PRM_REQUIRED_METADATA);
                         });
                         depends_on_str.pop_back();//' '
                         depends_on_str.pop_back();//','
                    }                    
                    else {//d1
                         std::string metadata_name = to_string(value);  
                         depends_on_str = metadata_name;
                         //depends_on_metadata[metadata_name] = false;
                         depends_on_metadata.insert(
                             {metadata_name, false}
                         );
                             
                         //load_desc(m_descs, ctx, metadata_name, Desc_type::PRM_REQUIRED_METADATA);
                    } 
                    depends_on_str = "["+depends_on_str+"]";
 
                 }
                 else {
                     std::cerr << "ERROR: damaris_cfg unrecogognized layout map string: " << key << std::endl ;
                 }
             });
             m_layout_depends_on.emplace(layoutxml.layout_name_, depends_on_metadata);
             //set default value if depend on metadata
            //if(is_dependent && std::find(std::begin(numbers_types), std::end(numbers_types), in_datatype)) { 
            if(is_dependent) {
                std::stringstream ss_dims(layoutxml.layout_dimensions_), ss_globals(layoutxml.layout_dims_global_);
                std::string tmp;
                std::vector<std::string> dim_list, global_list;

                while (std::getline(ss_dims, tmp, ',')) {
                    dim_list.push_back(tmp);
                }
                while (std::getline(ss_globals, tmp, ',')) {
                    global_list.push_back(tmp);
                }

                ctx.logger().info("------------------- OLD  layoutxml.layout_dimensions_ '{}' |  layoutxml.layout_dims_global_ '{}'", layoutxml.layout_dimensions_, layoutxml.layout_dims_global_);

                ctx.logger().info("------------------- dim_list[0] = '{}' |  global_list[0] = '{}'", dim_list[0], global_list[0]);

                std::string prm_config_yaml
                    = "";//"parameters:                                                         \n";

                layoutxml.layout_dimensions_ = "";
                std::string new_globals = "";
                //TODO: get the type of the metadata to which the layout depends, to apply it to the parameters
                std::string metadatatype = "int";

                for (int i = 0; i < nb_dims; i++)
                {                    
                    string dim_name = layoutxml.layout_name_+"_dim"+std::to_string(i);
                    prm_config_yaml += "- parameter:                                                    \n";
                    prm_config_yaml += "    name: "+dim_name+"                                          \n";
                    prm_config_yaml += "    type: "+metadatatype+"                                      \n";
                    prm_config_yaml += "    value: '"+dim_list[i]+"'                                    \n";
                    prm_config_yaml += "    depends_on: "+depends_on_str+"                              \n";

                    layoutxml.layout_dimensions_ += dim_name+","; 

                    if(layoutxml.layout_dims_global_.length() > 1) {      
                        string global_name = layoutxml.layout_name_+"_global"+std::to_string(i);                      
                        prm_config_yaml += "- parameter:                                                    \n";
                        prm_config_yaml += "    name: "+global_name+"                                       \n";
                        prm_config_yaml += "    type: "+metadatatype+"                                      \n";
                        prm_config_yaml += "    value: '"+global_list[i]+"'                                 \n";
                        prm_config_yaml += "    depends_on: "+depends_on_str+"                              \n";

                        new_globals += global_name+","; 
                    } 
                }
                layoutxml.layout_dimensions_.pop_back();
                //the layout_dims_global_ attribute being optional with default value '#', we need to ensure it has been set before modification.
                //if(layoutxml.layout_dims_global_ != '#') {  
                if(layoutxml.layout_dims_global_.length() > 1) {  
                    new_globals.pop_back();
                    layoutxml.layout_dims_global_ = new_globals;
                }

                //Background creation of parameter
	            PC_tree_t parameters_conf = PC_parse_string(prm_config_yaml.c_str());     
                ctx.logger().info("------------------- parameters_conf = \n '{}'", prm_config_yaml);                   
                parse_parameters_tree(ctx, parameters_conf);
             }
             else {
                 ///???
             }
 
             if(dataset_elt_full_name.empty())
                 throw Value_error{"ERROR: damaris_cfg layout name must not be empty"};
 
             //m_layouts.emplace(layoutxml.layout_name_ , layoutxml) ;
             m_layouts.emplace(dataset_elt_full_name , layoutxml) ;
             ctx.logger().info("------------------- Parsing damaris Layout '{}' , name_index = {} ", dataset_elt_full_name, name_index);
 
             if(name_index == 1) {
                 find_replace_map.insert(
                         {"_DATASET_ELEMENT_REGEX_", layoutxml.ReturnXMLForLayout() + "\n_DATASET_ELEMENT_REGEX_"}
                 );
 
                 //Update the xml config
                 damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
             }
             else {//In nested groups
                 insert_dataset_elts_to_group(layoutxml, nested_groups_names, name_index);    
             }
         });
     });
 }
 
 void Damaris_cfg::parse_meshes_tree(Context& ctx, PC_tree_t meshes_tree_list)
 {
     opt_each(meshes_tree_list, [&](PC_tree_t meshes_tree) {//each meshes (list of mesh)    
         each(meshes_tree, [&](PC_tree_t meshes_tree_key, PC_tree_t mesh_tree) {//each mesh
 
             damaris::model::DamarisMeshXML meshxml{} ;
             std::map<std::string, std::string> find_replace_map = {};
             unsigned name_index = 0;
             std::string dataset_elt_full_name;
 
             each(mesh_tree, [&](PC_tree_t mesh_tree_key, PC_tree_t value) {//mesh info
                 std::string key = to_string(mesh_tree_key);
 
                 int tf; // 0 is false, anything else is true
                 if (key == "name") {
                     dataset_elt_full_name = to_string(value); 
                     retrive_nested_groups(dataset_elt_full_name
                     , ds_elt_full_name_delimiter
                     , nested_groups_names
                     , name_index);
 
                     //meshxml.set_name(to_string(value));
                     meshxml.set_name(nested_groups_names[name_index-1]);
                 } 
                 else if (key == "type") {
                     meshxml.set_type(to_string(value));
                 } 
                 else if (key == "topology") {
                     meshxml.set_topology(to_long(value));
                 } 
                 else if (key == "coordinates") {//Liste of coordinates
                     opt_each(value, [&](PC_tree_t coords_tree) {//each meshes (list of mesh)    
                         each(coords_tree, [&](PC_tree_t coord_key, PC_tree_t coord_tree) {//each mesh
                             std::string coord_name, coord_unit = "", coord_label = "", coord_comment = "";
 
                             each(coord_tree, [&](PC_tree_t ct_tree_key, PC_tree_t value) {//mesh info
                                 std::string coord_info_key = to_string(ct_tree_key);
 
                                 int tf; // 0 is false, anything else is true
                                 if (coord_info_key == "name") {
                                     coord_name = to_string(value);
                                 } else if (coord_info_key == "unit") {
                                     coord_unit = to_string(value);
                                 } else if (coord_info_key == "label") {
                                     coord_label = to_string(value);
                                 } else if (coord_info_key == "comment") {
                                     coord_comment = to_string(value);
                                 } else {
                                     std::cerr << "ERROR: damaris_cfg unrecogognized coord map string: " << key << std::endl ;
                                 }
                             });
                             meshxml.add_coord(coord_name, coord_unit, coord_label, coord_comment);
                         });
                     });
                 } 
                 else {
                     std::cerr << "ERROR: damaris_cfg unrecogognized storage map string: " << key << std::endl ;
                 }
             });
 
             if(dataset_elt_full_name.empty())
                 throw Value_error{"ERROR: damaris_cfg mesh name must not be empty} groups"};
 
             //m_meshes.emplace(meshxml.get_name() , meshxml) ;
             m_meshes.emplace(meshxml.get_name() , meshxml) ;
             ctx.logger().info("------------------- Parsing damaris Mesh '{}' , name_index = {} ", dataset_elt_full_name, name_index);
 
             if(name_index == 1) {
                 find_replace_map.insert(
                         {"_DATASET_ELEMENT_REGEX_", meshxml.ReturnXMLForMesh() + "\n_DATASET_ELEMENT_REGEX_"}
                 );
 
                 //Update the xml config
                 damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
             }
             else {//In nested groups
                 insert_dataset_elts_to_group(meshxml, nested_groups_names, name_index);    
             }
         });
     });
 }
 
 void Damaris_cfg::parse_storages_tree(Context& ctx, PC_tree_t storages_tree_list)
 {
     opt_each(storages_tree_list, [&](PC_tree_t storages_tree) {//each storages (list of storage)    
         each(storages_tree, [&](PC_tree_t storagest_key, PC_tree_t storage_tree) {//each storage
                 
             damaris::model::DamarisStoreXML store{} ;
             std::map<std::string, std::string> find_replace_map = {};
 
             each(storage_tree, [&](PC_tree_t st_key, PC_tree_t value) {//storage info
                 std::string key = to_string(st_key);
 
                 int tf; // 0 is false, anything else is true
                 if (key == "name") {
                     store.store_name_ = to_string(value);
                 } 
                 else if (key == "type") {
                     store.store_type_ = to_string(value);
                 } 
                 else if (key == "file_mode") {
                     store.store_opt_FileMode_ = to_string(value);
                 } 
                 else if (key == "files_path") {
                     store.store_opt_FilesPath_ = to_string(value);
                 } 
                 else {
                     std::cerr << "ERROR: damaris_cfg unrecogognized storage map string: " << key << std::endl ;
                 }
             });
             m_storages.emplace(store.store_name_ , store) ;
 
             find_replace_map.insert(
                     {"_STORAGE_ELEMENT_REGEX_", store.ReturnXMLForStore() + "\n_STORAGE_ELEMENT_REGEX_"}
             );
 
             //Update the xml config
             damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
         });
     });
 }
 
 void Damaris_cfg::parse_paraview_tree(Context& ctx, PC_tree_t paraview_tree)
 {
     damaris::model::DamarisParaviewXML paraviewxml{} ;
     std::map<std::string, std::string> find_replace_map = {};
 
     each(paraview_tree, [&](PC_tree_t lt_key, PC_tree_t value) {//layout info
         std::string key = to_string(lt_key);
 
         int intb; // 0 is false, anything else is true
         if (key == "update_frequency") {
             int in_update_frequency = to_long(value);
             paraviewxml.set_update_frequency( in_update_frequency );
         } 
         else if (key == "realtime_timestep") {
             float in_realtime_timestep = (float) to_double(value);
             paraviewxml.set_realtime_timestep( in_realtime_timestep );
         } 
         else if (key == "end_iteration") {
             int in_end_iteration = to_long(value);
             paraviewxml.set_end_iteration( in_end_iteration );
         } 
         else if (key == "write_vtk") {
             int in_write_vtk = to_long(value);
             paraviewxml.set_write_vtk( in_write_vtk );
         } 
         else if (key == "write_vtk_binary") {
             PC_bool(value, &intb);
             bool in_write_vtk_binary = (intb == 0) ?  false : true ;
             paraviewxml.set_write_vtk_binary( in_write_vtk_binary );
         } 
         else if (key == "comment") {
             std::string in_comment = to_string(value);
             paraviewxml.set_comment( in_comment );
         } 
         else if (key == "scripts" || key == "script") {                   
 
             if (!PC_status(PC_get(value, "[0]"))) {//Array //[script1, script2, script3]
                 int nb_paraview_scripts; PC_len(value, &nb_paraview_scripts);
                 std::cout << "INFO: damaris_cfg nb paraview scripts: " << nb_paraview_scripts << std::endl ;
                 
                 each(value, [&](PC_tree_t script) {
                     paraviewxml.add_script(to_string(script));
                 });
             }                    
             else {
                 paraviewxml.add_script(to_string(value));
             }                    
         } 
         else {
             std::cerr << "ERROR: damaris_cfg unrecogognized paraview map string: " << key << std::endl ;
         }
     });
 
     m_paraview = &paraviewxml ;
 
     find_replace_map.insert(
             {"_PLUGINS_REGEX_", paraviewxml.ReturnXMLForParaview() + "\n_PLUGINS_REGEX_"}
     );
 
     //Update the xml config
     damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
 }
 
 void Damaris_cfg::parse_pyscript_tree(Context& ctx, PC_tree_t pyscript_tree)
 {
     damaris::model::DamarisPyScriptXML pyscriptxml{} ;
     std::map<std::string, std::string> find_replace_map = {};
 
     each(pyscript_tree, [&](PC_tree_t py_key, PC_tree_t value) {//layout info
         std::string key = to_string(py_key);
 
         int intb; // 0 is false, anything else is true
         if (key == "name") {
             pyscriptxml.pyscript_name_ = to_string(value);
         } 
         else if (key == "file") {
             pyscriptxml.pyscript_file_ = to_string(value);
         } 
         else if (key == "scheduler_file") {
             pyscriptxml.scheduler_file_ = to_string(value);
         } 
         else if (key == "execution") {
             pyscriptxml.execution_ = to_string(value);
         } 
         else if (key == "language") {
             pyscriptxml.language_ = to_string(value);
         } 
         else if (key == "scope") {
             pyscriptxml.scope_ = to_string(value);
         } 
         else if (key == "external") {
             PC_bool(value, &intb);
             bool in_external = (intb == 0) ?  false : true ;
             pyscriptxml.external_ = in_external;
         } 
         else if (key == "frequency") {
             pyscriptxml.frequency_ = to_long(value);
         } 
         else if (key == "nthreads") {
             pyscriptxml.nthreads_ = to_long(value);
         } 
         else if (key == "timeout") {
             pyscriptxml.timeout_ = to_long(value);
         } 
         else if (key == "keep_workers_") {
             pyscriptxml.keep_workers_  = to_string(value);//yes/no
         } 
         else if (key == "comment") {
             pyscriptxml.comment_ = to_string(value);
         } 
         else {
             std::cerr << "ERROR: damaris_cfg unrecogognized pyscript map string: " << key << std::endl ;
         }
     });
 
     m_pyscript = &pyscriptxml ;
 
     find_replace_map.insert(
             {"_SCRIPTS_REGEX_", pyscriptxml.ReturnXMLForPyScript() + "\n_SCRIPTS_REGEX_"}
     );
 
     //Update the xml config
     damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
 }
 
 void Damaris_cfg::parse_write_tree(Context& ctx, PC_tree_t write_tree_list)
 {   
    each(write_tree_list, [&](PC_tree_t writet_key, PC_tree_t write_ds_tree) {//each dataset to write
        
        std::string ds_name = to_string(writet_key);//the name of the data to write, if dataset not specified afterward!
        Dataset_Write_Info ds_write_info;

        //dataset
        PC_tree_t ds_name_tree = PC_get(write_ds_tree, ".dataset");
        if(!PC_status(ds_name_tree))
        {
            ds_name = to_string(ds_name_tree);
            std::cout << "INFO: damaris_cfg write_ds_tree :: ds_name = '" << ds_name  << "'"  << std::endl ;
        }
        //when
        PC_tree_t ds_when_tree = PC_get(write_ds_tree, ".when");
        if(!PC_status(ds_when_tree))
        {
            ds_write_info.when = to_string(ds_when_tree);
            std::cout << "INFO: damaris_cfg write_ds_tree :: when = '" << ds_write_info.when  << "'"  << std::endl ;
        }
        //position
        PC_tree_t ds_position_tree = PC_get(write_ds_tree, ".position");
        if(!PC_status(ds_position_tree))
        {
            if (!PC_status(PC_get(ds_position_tree, "[0]"))) {//Array [p0,p1,p3] (1 to 3 elements)
                int position_dim; PC_len(ds_position_tree, &position_dim);
                std::cout << "INFO: damaris_cfg write_ds_tree :: '"<< ds_name <<"' will be written in dims: " << position_dim << std::endl ;
                                        
                int pos_idx = 0;
                each(ds_position_tree, [&](PC_tree_t dim) {
                    ds_write_info.position[pos_idx] = to_string(dim);
                    pos_idx++;
                });
            }                    
            else {//p0
                ds_write_info.position[0] = to_string(ds_position_tree);   
                std::cout << "INFO: damaris_cfg write_ds_tree :: '"<< ds_name <<"' will be written in dims: 1" << std::endl ;
            }
        }
        //block
        PC_tree_t ds_block_tree = PC_get(write_ds_tree, ".block");
        if(!PC_status(ds_block_tree))
        {
            ds_write_info.block = to_string(ds_block_tree);
            std::cout << "INFO: damaris_cfg write_ds_tree :: block = '" << ds_write_info.block  << "'"  << std::endl ;
        }

        m_datasets_to_write.emplace(ds_name, ds_write_info);
        
        load_desc(m_descs, ctx, ds_name, Desc_type::DATA_TO_WRITE_WITH_BLOCK);
    });
 }
 
 
 
 void Damaris_cfg::parse_parameter_to_update_tree(Context& ctx, PC_tree_t ptu_tree, Desc_type op_type)
 {  
    if (!PC_status(PC_get(ptu_tree, "[0]"))) {//Array [prm0,prm1,prm3,...] / it's a list of names only
        std::cout << "INFO: damaris_cfg parameter_set or _get :: Array [prm0,prm1,prm3,...] / it's a list of names only " << std::endl ;
        each(ptu_tree, [&](PC_tree_t prm_name_t) {   
            std::pair<std::string, Desc_type> prm_to_update_info;
            std::string metadata = to_string(prm_name_t);
            std::string prm_name = to_string(prm_name_t);

            prm_to_update_info.first = prm_name;
            prm_to_update_info.second = op_type;
            m_parameter_to_update.emplace(metadata, prm_to_update_info);
            load_desc(m_descs, ctx, metadata, op_type);
            std::cout << "INFO: damaris_cfg parameter_set or _get :: metadata = "<< metadata <<" and prm_name = "<< prm_name <<" " << std::endl ;
        });
    } 
    else if (!PC_status(ptu_tree)) { // it's a name:{config...} mapping
        std::cout << "INFO: damaris_cfg parameter_set or _get :: it's a name:{config...} mapping " << std::endl ;
        each(ptu_tree, [&](PC_tree_t ptu_metadata, PC_tree_t ptu_prmname) {
            std::string metadata = to_string(ptu_metadata);
            std::string prm_name = to_string(ptu_metadata);   
            std::pair<std::string, Desc_type> prm_to_update_info;

            if(!PC_status(ptu_prmname))
            {
                if(to_string(ptu_prmname) != "")
                    prm_name = to_string(ptu_prmname);
            }

            prm_to_update_info.first = prm_name;
            prm_to_update_info.second = op_type;
            m_parameter_to_update.emplace(metadata, prm_to_update_info);
            load_desc(m_descs, ctx, metadata, op_type);
            std::cout << "INFO: damaris_cfg parameter_set or _get :: metadata = "<< metadata <<" and prm_name = "<< prm_name <<" " << std::endl ;
        }); 
    }    
 }
 
 void Damaris_cfg::parse_log_tree(Context& ctx, PC_tree_t config)
 {
     std::map<std::string, std::string> find_replace_map = {};
 
     //Log File Name
     std::string log_file_name = "damaris_pdi_simu";
     PC_tree_t log_file_name_tree = PC_get(config, ".log.file_name");
     if(!PC_status(log_file_name_tree))
     {
         log_file_name = to_string(log_file_name_tree);
     }
     else {        
         PC_tree_t sim_name_tree = PC_get(config, ".architecture.sim_name");
         if(!PC_status(sim_name_tree))
         {
             log_file_name = to_string(sim_name_tree);
         }
     }    
     
     std::string log_rotation_size = "5";
     PC_tree_t log_rotation_size_tree = PC_get(config, ".log.rotation_size");
     if(!PC_status(log_rotation_size_tree))
     {
         log_rotation_size = to_string(log_rotation_size_tree);
     }
     
     std::string log_level = "info";
     PC_tree_t log_level_tree = PC_get(config, ".log.log_level");
     if(!PC_status(log_level_tree))
     {
         log_level = to_string(log_level_tree);
     }
     
     std::string log_flush = "true";
     PC_tree_t log_flush_tree = PC_get(config, ".log.flush");
     if(!PC_status(log_flush_tree))
     {
         log_flush = to_string(log_flush_tree);
     }
 
     find_replace_map.insert({
              {"_SIM_LOG_NAME_",log_file_name}
             ,{"_LOG_ROTATION_SIZE_", log_rotation_size}
             ,{"_LOG_FLUSH_", log_flush}
             ,{"_LOG_LEVEL_", log_level}
     });
 
     //Update the xml config
     damarisXMLModifyModel.RepalceWithRegEx(find_replace_map);
 }
 
 
 
     bool Damaris_cfg::is_dataset_to_write(std::string data_name)
     {
         bool is_dataset_to_write = false;
         for(auto &datasets_to_write : m_datasets_to_write) {
             if(data_name == datasets_to_write.first)
             {
                 is_dataset_to_write = true;
                 break;
             }
         }
 
         return is_dataset_to_write;
     }
 
     bool Damaris_cfg::is_parameter_to_update(std::string data_name)
     {
         bool is_parameter_to_update = false;
         for(auto &parameter_to_update : m_parameter_to_update) {
             if(data_name == parameter_to_update.first)
             {
                 is_parameter_to_update = true;
                 break;
             }
         }
 
         return is_parameter_to_update;
     }
 
     bool Damaris_cfg::is_needed_metadata(std::string data_name)
     {
         bool is_needed_metadata = false;
         for(auto &prm_depends_on : m_parameter_depends_on) {
             auto &prm_name            = prm_depends_on.first;
             auto &prm_depends_on_data = prm_depends_on.second;
             for(auto &depends_on_data : prm_depends_on_data) {
                 //auto &depends_on_data_name   = depends_on_data.first;
                 //auto &depends_on_data_state  = depends_on_data.second;
 
                 if(data_name == depends_on_data.first && depends_on_data.second == false)
                 {
                     depends_on_data.second = true;
                     is_needed_metadata = true;
                 }
             }
         }
 
         return is_needed_metadata;
     }
 
     
     //std::vector<std::string> 
     //std::unordered_map<std::string, std::pair<PDI::Expression, std::string>> Damaris_cfg::get_updatable_parameters()
     std::unordered_map<std::string, std::pair<std::string, std::string>> Damaris_cfg::get_updatable_parameters(Context& ctx)
     {
         std::unordered_map<std::string, std::pair<std::string, std::string>> updatable_parameters;
         for(auto prm_depends_on : m_parameter_depends_on) {
             auto prm_name            = prm_depends_on.first;
             auto prm_depends_on_data = prm_depends_on.second;
             bool to_be_updated = (prm_depends_on_data.size() > 0);//true;
             for(auto depends_on_data : prm_depends_on_data) {
                 auto depends_on_data_name   = depends_on_data.first;
                 auto depends_on_data_state  = depends_on_data.second;
 
                 if(!depends_on_data_state)
                 {
                     to_be_updated = false;
                     break;
                 }    
             }
             if(to_be_updated) {//Update the parameter
                 PDI::Expression prm_value = m_parameter_expression.at(prm_name);
                 damaris::model::DamarisParameterXML prmxml = m_parameters.at(prm_name);
                 prmxml.param_value_ = prm_value.to_string(ctx);
                 //ctx.logger().info("------------------------------------------------------------------------------------In get_updatable_parameters `{}' prmxml.param_value_ = '{}'", prm_name, prmxml.param_value_);
                 std::pair<std::string, std::string> update_info;
                 update_info.first = prm_value.to_string(ctx);
                 update_info.second = prmxml.param_datatype_;
 
                 updatable_parameters.emplace(prm_name, update_info);
             }
         }
 
         return updatable_parameters;
     }
     
 
     void Damaris_cfg::reset_parameter_depends_on(std::string prm_name) {
         if (m_parameter_depends_on.find(prm_name) == m_parameter_depends_on.end()) {
             // not found
             //  handle the error
         } else {
             std::unordered_map<std::string, bool> prm_depends_on_data = m_parameter_depends_on.at(prm_name);
             for(auto depends_on_data : prm_depends_on_data) {
                 depends_on_data.second = false;
             }
         }
     }
 
     void Damaris_cfg::reset_parameter_depends_on(std::vector<std::string> prm_list) {
         for (auto prm_name : prm_list) {
             reset_parameter_depends_on(prm_name);
         }
     }
 
     void Damaris_cfg::reset_all_parameters_depends_on() {
         for(auto prm_depends_on : m_parameter_depends_on) {
             auto prm_name            = prm_depends_on.first;
             auto prm_depends_on_data = prm_depends_on.second;
 
             for(auto depends_on_data : prm_depends_on_data) {
                 depends_on_data.second = false;
             }
         }
     }
 
 
     // Retrive nested groups names from a dataset_elt_full_name (gp1/gp2/.../dataset_elt_name)
     void retrive_nested_groups(std::string& dataset_elt_full_name, char delimiter, std::string nested_groups_names[], unsigned& index)
     {
         // Creating an input string stream from the dataset_elt_full_name
         std::istringstream namestream(dataset_elt_full_name);
 
         // Tmp string
         string gp_name;
 
         // Retrive names from the string stream separated by the delimiter
         while (getline(namestream, gp_name, delimiter)) {
             if(index == max_nested_groups) {
                 throw Value_error{"Damaris variable/layout/mesh can be nested in more than {} groups", max_nested_groups};
             }
             // Add the gp_name to the array
             nested_groups_names[index++] = gp_name;
         }
         //If index==1, there is no group, ie: nested_groups_names[index-1] = dataset_elt_full_name
     }
 
     
     template <typename DS_TYPE>
     void insert_dataset_elts_to_group(DS_TYPE ds_elt_xml, std::string nested_groups_names[], unsigned index)
     {   
         std::string nearest_group_name = nested_groups_names[index -2];
         bool root_group_exists = false;
         damaris::model::DamarisGroupXML *nearest_parent_group = NULL;
         for (int root_group_id = 0; root_group_id < root_groups_xml.size(); root_group_id++) {
             damaris::model::DamarisGroupXML *root_gp_xml = &root_groups_xml[root_group_id];
 
             if(nested_groups_names[0] == root_gp_xml->get_name())
             {
                 if(nearest_group_name == root_gp_xml->get_name())
                 {
                     //printf("-------------------------------------------------------------------------------------In root_gp_xml.get_name() = %s, FOR Variable %s, nearest_group_name == root_gp_xml.get_name() = %d\n", root_gp_xml->get_name().c_str(), nested_groups_names[index -1].c_str(), (nearest_group_name == root_gp_xml->get_name()));
                     //root_gp_xml->add_variable(ds_elt_xml);
                     root_gp_xml->add_ds_element(ds_elt_xml);
 
                     //free(root_gp_xml);
                     return;
                 }
                 nearest_parent_group = root_gp_xml;
                 root_group_exists = true;
                 break;
             }
             
             //free(root_gp_xml);
         }
 
         //Insertion in an existing nested group
         bool insertion_group_found = false;
         int group_id = 1;
         if(root_group_exists)
         {   
             while (group_id <= (index - 2) && !insertion_group_found) {
                 std::string group_name = nested_groups_names[group_id];
                 
                 insertion_group_found = true;
                 std::vector<damaris::model::DamarisGroupXML> sub_groups = nearest_parent_group->get_sub_groups();
                 for (int group_id = 0; group_id < sub_groups.size(); group_id++) {
                     if(group_name == sub_groups[group_id].get_name())
                     {
                         if(nearest_group_name == sub_groups[group_id].get_name())
                         {
                             //(&sub_groups[group_id])->add_variable(ds_elt_xml);
                             (&sub_groups[group_id])->add_ds_element(ds_elt_xml);
 
                             return;
                         }
                         insertion_group_found = false;
                         nearest_parent_group = &sub_groups[group_id++];
                         break;
                     }
                 }
             }   
         }
         //The nested groups has to be created
         else {
             damaris::model::DamarisGroupXML root_gp_xml{nested_groups_names[0]};   
 
             if(nearest_group_name == root_gp_xml.get_name())
             {
                 //root_gp_xml.add_variable(ds_elt_xml);
                 root_gp_xml.add_ds_element(ds_elt_xml);
                 root_groups_xml.emplace_back(root_gp_xml);
 
                 return;
             }
 
             root_groups_xml.emplace_back(root_gp_xml);
            
             nearest_parent_group = &root_gp_xml;
             insertion_group_found = true;
         }
 
         //Insertion point found
         if(insertion_group_found){
             damaris::model::DamarisGroupXML linked_parent_group{nearest_group_name};
             for (unsigned insertion_group_id = (index - 2); insertion_group_id >= group_id; insertion_group_id--)
             {                    
                 std::string group_name = nested_groups_names[insertion_group_id];
                 damaris::model::DamarisGroupXML sub_gp_xml{group_name}; 
                 if(nearest_group_name == sub_gp_xml.get_name())
                 {
                     //sub_gp_xml.add_variable(ds_elt_xml);
                     sub_gp_xml.add_ds_element(ds_elt_xml);
                 }
                 else {
                     sub_gp_xml.add_sub_group(linked_parent_group);
                 }
                 linked_parent_group = sub_gp_xml;
             }
             nearest_parent_group->add_sub_group(linked_parent_group);            
         }
             
         //free(nearest_parent_group);
     }
 
     /*    
     void insert_dataset_elts_to_group(damaris::model::DamarisVarXML varxml, std::string nested_groups_names[], unsigned index)
     {                
         std::string nearest_group_name = nested_groups_names[index -2];
         bool group_exists = false;
         for (int root_group_id = 0; root_group_id < root_groups_xml.size(); root_group_id++) {
             damaris::model::DamarisGroupXML *root_gp_xml = &root_groups_xml[root_group_id];
 
             if(nearest_group_name == root_gp_xml->get_name())
             {
                 //printf("-------------------------------------------------------------------------------------In root_gp_xml.get_name() = %s, FOR Variable %s, nearest_group_name == root_gp_xml.get_name() = %d\n", root_gp_xml->get_name().c_str(), nested_groups_names[index -1].c_str(), (nearest_group_name == root_gp_xml->get_name()));
                 root_gp_xml->add_variable(varxml);
                 group_exists = true;
                 break;
             }
 
             std::vector<damaris::model::DamarisGroupXML> sub_groups = root_gp_xml->get_sub_groups();
             for (int group_id = 0; group_id < sub_groups.size(); group_id++) {
                 if(nearest_group_name == sub_groups[group_id].get_name())
                 {
                     (&sub_groups[group_id])->add_variable(varxml);
                     group_exists = true;
                     break;
                 }
             }
         }
 
         if(!group_exists)
         { 
             damaris::model::DamarisGroupXML root_gp_xml{nested_groups_names[0]};   
             int group_id = 1;
             while (group_id <= index - 2) {
                 std::string group_name = nested_groups_names[group_id];
                 damaris::model::DamarisGroupXML sub_gp_xml{group_name}; 
                 if(nearest_group_name == sub_gp_xml.get_name())
                 {
                     sub_gp_xml.add_variable(varxml);
                 }
                 root_gp_xml.add_sub_group(sub_gp_xml);
                 group_id++;
             }
 
             if(group_id == 1) {
                 root_gp_xml.add_variable(varxml);
             }
 
             root_groups_xml.emplace_back(root_gp_xml);
         }
     }
     */
 
 const string& Damaris_cfg::xml_config_object( void )
 {
     m_xml_config_object = damarisXMLModifyModel.GetConfigString();
     return m_xml_config_object;
 }
 
 //const PDI::Expression& Damaris_cfg::communicator() const
 PDI::Expression Damaris_cfg::communicator() const
 {
     return m_communicator;
 }
 
 const unordered_map<std::string, damaris::model::DamarisVarXML>& Damaris_cfg::datasets() const
 {
     return m_datasets;
 }
 const unordered_map<std::string, damaris::model::DamarisLayoutXML>& Damaris_cfg::layouts() const
 {
     return m_layouts;
 }
 
 const unordered_map<std::string, damaris::model::DamarisParameterXML>& Damaris_cfg::parameters() const
 {
     return m_parameters;
 }

const damaris::model::DamarisParameterXML Damaris_cfg::get_parameter_xml(string prm_name) const
{
    return m_parameters.at(prm_name);
}
 
 const std::unordered_map<std::string, damaris::model::DamarisStoreXML>& Damaris_cfg::storages() const
 {
     return m_storages;
 }
 
 const std::unordered_map<std::string, damaris::model::DamarisMeshXML>& Damaris_cfg::meshes() const
 {
     return m_meshes;
 }
 
 const std::unordered_map<std::string, damaris::model::DamarisGroupXML>& Damaris_cfg::groups() const
 {
     return m_groups;
 }
 
 const unordered_map<string, Desc_type>& Damaris_cfg::descs() const
 {
     return m_descs;
 }
 
 const unordered_map<string, Event_type>& Damaris_cfg::events() const
 {
     return m_events;
 }
 
 const std::unordered_map<std::string, Dataset_Write_Info>& Damaris_cfg::datasets_to_write() const
 {
     return m_datasets_to_write;
 }
 
 const std::unordered_map<std::string, std::pair<std::string, Desc_type>>& Damaris_cfg::parameter_to_update() const
 {
     return m_parameter_to_update;
 }
 
 Dataset_Write_Info Damaris_cfg::get_dataset_write_info(std::string data_name) const
 {
     try{
         return m_datasets_to_write.at(data_name);
     } catch (...) {
         assert(false && "Trying to get inexistant damaris awaited dataset!");
     }
 }
 
 std::pair<std::string, Desc_type> Damaris_cfg::get_parameter_to_update_info(std::string data_name) const
 {
     try{
         return m_parameter_to_update.at(data_name);
     } catch (...) {
         assert(false && "Trying to get inexistant damaris awaited dataset!");
     }
 }
 
 std::string Damaris_cfg::init_on_event() const
 {
    return m_init_on_event;
 }
 
 std::string Damaris_cfg::finalize_on_event() const
 {
    return m_finalize_on_event;
 }
 
 
 std::string Damaris_cfg::start_on_event() const
 {
     return m_start_on_event;
 }
 
 std::string Damaris_cfg::stop_on_event() const
 {
     return m_stop_on_event;
 }
 
 std::string Damaris_cfg::end_iteration_on_event() const
 {
     return m_end_iteration_on_event;
 }
 
 } // namespace damaris_pdi