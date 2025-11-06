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

#include <stdlib.h>
#include <Damaris.h>
#include <mpi.h>
#include <cassert>

#include <bits/stdc++.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <list>

#include <paraconf.h>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

#include "damaris_cfg.h"
#include "damaris_wrapper.h"
#include "damaris_api_call_handler.h"

namespace {

using PDI::Datatype_sptr;
using PDI::Context;
using PDI::Error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_w;
using PDI::Ref_r;
using PDI::to_string;

using std::get;
using std::pair;
using std::list;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::dynamic_pointer_cast;

using namespace PDI;
using namespace damaris_pdi;

class damaris_plugin: public Plugin {

	Damaris_cfg m_config;
	Damaris_api_call_handler m_event_handler;
	
	unique_ptr<Damaris_wrapper> m_damaris;
	
	static pair<unordered_set<string>, unordered_set<string>> dependencies()
	{
		return {{"mpi"}, {"mpi"}};
	}

	list<string> multi_expose_transaction_dataname;
	//list<Ref> multi_expose_transaction_dataref;

	std::string int_numbers_types[3]  = {"short", "int", "integer"};
	std::string real_numbers_types[3] = {"float", "real", "double"};
	
	int iteration = 0;//for debugging
    int datasets_to_write_count = 0;//The number of data already written in the current iteration

public:

	damaris_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
		, m_config{ctx, config}
		, m_event_handler{m_config.xml_config_object(), m_config.communicator(), m_config.init_on_event()
			, m_config.start_on_event(), m_config.stop_on_event()}
	{

		std::string data_cb_concat = "";
		for (auto&& desc: m_config.descs()) {//add data callback only for awaited data
			ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) { this->data(name, ref); },
				desc.first);
				data_cb_concat.append(desc.first + ", ");
		}
		context().logger().info("Data for callback : {}", data_cb_concat);
		
		//Sim configured event names
		for (auto&& event: m_config.events()) {
			ctx.callbacks().add_event_callback([this](const std::string& name) { this->event(name); },
			event.first);
		}
		
		//Default event names, maight be called internally
		for (auto&& ev_name: event_names) {
			//Only if the key if not yet used by an event
			if (m_config.events().find(ev_name.second) == m_config.events().end())
				ctx.callbacks().add_event_callback([this](const std::string& name) { this->event(name); },
					ev_name.second);
		}

		//ctx.callbacks().add_event_callback([this](const std::string& name) { this->event(name); });

		ctx.logger().info("Plugin loaded successfully");
	}

	void data(const std::string& name, Ref ref)
	{		
		ensure_damaris_is_initialized("");

		context().logger().info("data `{}' has been exposed", name);

		//Update damaris parameters
		if(m_config.is_needed_metadata(name)){
			std::unordered_map<std::string, std::pair<std::string, std::string>> updatable_parameters = m_config.get_updatable_parameters(context());

			std::string prm_name_concat = "";
			for(auto prm_update_info : updatable_parameters) {
				auto prm_name            = prm_update_info.first;
				auto update_info		 = prm_update_info.second;
				{
					std::string prm_value  = update_info.first;
					std::string prm_type   = update_info.second;

					void* prm_value_buffer;
					size_t prm_buffer_size;
					if(std::find(std::begin(int_numbers_types), std::end(int_numbers_types), prm_type) != std::end(int_numbers_types)) {
						std::cout << "int_numbers_types contains " << prm_type << '\n';
						int prm_long_value = std::atoi(prm_value.c_str());
						prm_value_buffer = &prm_long_value;
						prm_buffer_size = sizeof(int);
	
						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);	
												
						//checking if it works!
						/*int new_prm_val;
						m_damaris->damaris_pdi_parameter_get(prm_name.c_str(), &new_prm_val, prm_buffer_size);	
						context().logger().info("---------------------------------------------------------> Parameter '{}' updated successfully, new value '{}' vs Sent Value '{}'", prm_name, new_prm_val, prm_long_value);
						*/
					} 
					else if(std::find(std::begin(real_numbers_types), std::end(real_numbers_types), prm_type) != std::end(real_numbers_types)) {
						std::cout << "real_numbers_types contains " << prm_type << '\n';
						double prm_dbl_value = std::atof(prm_value.c_str());
						prm_value_buffer = &prm_dbl_value;
						prm_buffer_size = sizeof(double);

						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);  						
					} 
					else {
						std::cout << "String === " << prm_type << '\n';
						std::string prm_string_value = prm_value;
						prm_value_buffer = &prm_string_value;
						prm_buffer_size = sizeof(std::string);

						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);  						
					}
				}

				prm_name_concat.append(prm_name + ", ");
				m_config.reset_parameter_depends_on(prm_name);
			}
			if(updatable_parameters.size() > 0){
				prm_name_concat.pop_back();
				prm_name_concat.pop_back();
				context().logger().info("data `{}' Is a needed metadata for the evaluation of parameters {}", name, prm_name_concat);
			}
		}
		else if(m_config.is_dataset_to_write(name)){
			context().logger().info("is_dataset_to_write(`{}') = '{}'", name, m_config.is_dataset_to_write(name));
			if (Ref_r rref = ref) {
				Dataset_Write_Info ds_write_info = m_config.get_dataset_write_info(name);

				//Only write when autorized!
				if(ds_write_info.when.to_long(context())) {
					context().logger().info("data `{}' will be written when = '{}'", name, ds_write_info.when.to_long(context()));

					int32_t block = ds_write_info.block.to_long(context());
					context().logger().info("data `{}' will be written in block = '{}'", name, block);
					int64_t position[3] = {
						 ds_write_info.position[0].to_long(context())
						,ds_write_info.position[1].to_long(context())
						,ds_write_info.position[2].to_long(context())
					};
					
					const void* data = static_cast<const void*>(rref.get());

					if(block > 0) {
						context().logger().info("data `{}' will be written at: block '{}' and position '{}:{}:{}', when = '{}'", name, block, position[0], position[1], position[2], ds_write_info.when.to_long(context()));
					
						std::string set_block_pos_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_SET_BLOCK_POSITION);
						m_event_handler.damaris_api_call_event(context(), m_damaris, set_block_pos_event_name, multi_expose_transaction_dataname, name, block, position);
						
						std::string write_block_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_WRITE_BLOCK);
						m_event_handler.damaris_api_call_event(context(), m_damaris, write_block_event_name, multi_expose_transaction_dataname, name, block, data);	
					}
					else {
						std::string set_pos_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_SET_POSITION);
						m_event_handler.damaris_api_call_event(context(), m_damaris, set_pos_event_name, multi_expose_transaction_dataname, name, position);

						std::string write_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_WRITE);
						m_event_handler.damaris_api_call_event(context(), m_damaris, write_event_name, multi_expose_transaction_dataname, name, data);	
					}
					
					datasets_to_write_count++;
					//Wait until all datasets are written before launching end of iteration operations
					if(m_config.is_there_after_write_events() && datasets_to_write_count == m_config.datasets_to_write().size()) {
						list<string> after_write_events = m_config.get_after_write_events();
						for (auto it = after_write_events.begin(); it != after_write_events.end(); it++) {
							std::string aw_event = it->c_str();
							if(m_event_handler.is_damaris_api_call_event(aw_event)){
								context().logger().info("event `{}' has been triggered", aw_event);

								context().logger().info("is_damaris_api_call_event ( `{}' ) = TRUE", aw_event);
								m_event_handler.damaris_api_call_event(context(), m_damaris, aw_event, {});
								//m_event_handler.damaris_api_call_event(context(), m_damaris, aw_event, list<string> expose_dataname = {});
							}
							else {//Non Damaris call event
								
							}
						}
						datasets_to_write_count = 0;
					}
				}				
			}
			else{
				context().logger().error("The Damaris need write access over the data (`{}')", name);
			}

			//Debugging
			//iteration++;
			//if(iteration == 2)
			//	exit(0);
		}
		else if(m_config.is_parameter_to_update(name)){
			context().logger().info("m_config.is_parameter_to_update('{}') = `{}'", name, m_config.is_parameter_to_update(name));
			std::pair<std::string, Desc_type> prm_to_update_info = m_config.get_parameter_to_update_info(name);
			std::string prm_name = prm_to_update_info.first;
			size_t size;

			damaris::model::DamarisParameterXML prmxml = m_config.get_parameter_xml(prm_name);

			if(std::find(std::begin(int_numbers_types), std::end(int_numbers_types), prmxml.param_datatype_) != std::end(int_numbers_types)) {
				size = sizeof(int);					
			} 
			else if(std::find(std::begin(real_numbers_types), std::end(real_numbers_types), prmxml.param_datatype_) != std::end(real_numbers_types)) {
				size = sizeof(double);						
			} 
			else {
				size = sizeof(std::string);				
			}

			if (prm_to_update_info.second == Desc_type::PRM_TO_SET) {
				Ref_r rref = ref;

				int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), static_cast<const void*>(rref.get()), size);  	
			}
			else if (prm_to_update_info.second == Desc_type::PRM_TO_GET) {
				Ref_w wref = ref;

				int msg_err = m_damaris->damaris_pdi_parameter_get(prm_name.c_str(), static_cast<void*>(wref.get()), size);	
			}
			else {
				//Error handling!
			}
		}
		//is_client_get !?
		else if(name == m_config.is_client_dataset_name()) {	
			context().logger().info("'{}' == m_config.is_client_dataset_name() = '{}'", name, (name == m_config.is_client_dataset_name()));
					
			if (Ref_w wref = ref) {
				context().logger().info(":) D) '{}' == m_config.is_client_dataset_name() = '{}' | m_damaris->get_is_client() = '{}'", name, (name == m_config.is_client_dataset_name()), m_damaris->get_is_client());
				*static_cast<int*>(wref.get()) = m_damaris->get_is_client();
				context().logger().info("------------------- CALLED is_client_dataset_name Return is_client = '{}')", m_damaris->get_is_client());
			}
			else {
				//MayBe a PDI_multi_expose is under traitement
				multi_expose_transaction_dataname.emplace_back(name);
			}
		}
		//client_comm_get !?
		else if(name == m_config.client_comm_get_dataset_name()) {
			if (Ref_w wref = ref) {
				MPI_Comm client_comm;		
				int err = m_damaris->damaris_pdi_client_comm_get(&client_comm);

				*static_cast<MPI_Comm*>(wref.get()) = client_comm;
				context().logger().info("------------------- CALLED is_client_dataset_name Return client_comm SETED)");
			}
			else {
				//MayBe a PDI_multi_expose is under traitement
				multi_expose_transaction_dataname.emplace_back(name);
			}
		}
		else {//Handle other situations...
			multi_expose_transaction_dataname.emplace_back(name);
			//multi_expose_transaction_dataref.emplace_back(ref);
		}

		//In Situ
	}

	void event(const std::string& event_name)
	{
		ensure_damaris_is_initialized(event_name);

		//If it a sim configured event
		if (m_config.events().find(event_name) != m_config.events().end()) {
			if(event_name == m_config.init_on_event()) {
				std::string init_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_INITIALIZE);
				m_event_handler.damaris_api_call_event(context(), m_damaris, init_event_name, multi_expose_transaction_dataname);
			}
			else if(event_name == m_config.start_on_event()) {
				std::string start_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_START);
				m_event_handler.damaris_api_call_event(context(), m_damaris, start_event_name, multi_expose_transaction_dataname);
			}
			else if(event_name == m_config.end_iteration_on_event()) {
				std::string end_it_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_END_ITERATION);
				m_event_handler.damaris_api_call_event(context(), m_damaris, end_it_event_name, multi_expose_transaction_dataname);
			}
			else if(event_name == m_config.finalize_on_event()) {
				std::string finalize_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_FINALIZE);
				m_event_handler.damaris_api_call_event(context(), m_damaris, finalize_event_name, multi_expose_transaction_dataname);
			}
		}
		else if(m_event_handler.is_damaris_api_call_event(event_name)){
			context().logger().info("event `{}' has been triggered", event_name);

			context().logger().info("is_damaris_api_call_event ( `{}' ) = TRUE", event_name);
			m_event_handler.damaris_api_call_event(context(), m_damaris, event_name, multi_expose_transaction_dataname);

			multi_expose_transaction_dataname.clear();
			//multi_expose_transaction_dataref.clear();
		}
		else {//Non Damaris call event
			
		}
	}

	void ensure_damaris_is_initialized(const std::string& event_name)
	{
        if (!m_damaris) {
			if (m_config.events().find(event_name) != m_config.events().end()) {
				//Means the first action received by the plugin wasn't fir initialization...  
				if (!m_config.init_on_event().empty() && event_name != m_config.init_on_event()) {
					context().logger().error("Trying to use {} plugin before the initialization of the Damaris library!", pretty_name());
				}
				else if (event_name == m_config.init_on_event())
					return;//The init will follows
			}
			this->damaris_init();
        }
	}

	void damaris_init()
	{
		context().logger().info("In damaris_init()");
		std::string init_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_INITIALIZE);
		m_event_handler.damaris_api_call_event(context(), m_damaris, init_event_name, multi_expose_transaction_dataname);
	}


	~damaris_plugin()
	{

		if (m_config.finalize_on_event().empty() && m_damaris) {
			context().logger().info("Calling  DAMARIS_FINALIZE in ~damaris_plugin()");
			std::string finalize_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_FINALIZE);
			m_event_handler.damaris_api_call_event(context(), m_damaris, finalize_event_name, multi_expose_transaction_dataname);
		}

		context().logger().info("Closing plugin");
	}
	

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Damaris"; }

}; // class damaris_plugin

} // namespace <anonymous>

PDI_PLUGIN(damaris)