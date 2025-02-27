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

public:

	damaris_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
		, m_config{ctx, config}
		, m_event_handler{m_config.xml_config_object(), m_config.communicator(), m_config.init_on_event()
			, m_config.start_on_event(), m_config.stop_on_event()}
	{
		for (auto&& desc: m_config.descs()) {//add data callback only for awaited data
			ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) { this->data(name, ref); },
				desc.first);
		}
		//Trigger other data callback for multi_expose_transaction_dataname OR remove totally !!!?

		ctx.callbacks().add_event_callback([this](const std::string& name) { this->event(name); });

		ctx.logger().info("Plugin loaded successfully");
		
		if (!m_config.init_on_event() && m_config.communicator()) {
			//TODO: issue communicator to handle
			/* 
			MPI_Comm comm = *(static_cast<const MPI_Comm*>(Ref_r{m_config.communicator().to_ref(context())}.get()));	
			//context().logger().info("communicator `{}'", m_config.communicator().to_string(context()));
			
			m_damaris.reset(new Damaris_wrapper{context(), m_config.xml_config_object().c_str(), comm});
			context().logger().info("Plugin initialized successfully");
			*/

			std::string init_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_INITIALIZE);
			PDI_status_t status = PDI_event(init_event_name.c_str());
		}
	}

	void data(const std::string& name, Ref ref)
	{		
		//context().logger().info("data `{}' has been exposed", name);

		//Update damaris parameters
		if(m_config.is_needed_metadata(name)){
			std::unordered_map<std::string, std::pair<std::string, std::string>> updatable_parameters = m_config.get_updatable_parameters(context());

			std::string int_numbers_types[] = {"short", "int", "integer"};
			std::string real_numbers_types[] = {"float", "real", "double"};

			std::string prm_name_concat = "";
			for(auto prm_update_info : updatable_parameters) {
				auto prm_name            = prm_update_info.first;
				auto update_info		 = prm_update_info.second;
				{
					std::string prm_value  = update_info.first;
					std::string prm_type  	  = update_info.second;

					void* prm_value_buffer;
					size_t prm_buffer_size;
					if(std::find(std::begin(int_numbers_types), std::end(int_numbers_types), prm_type)) {
						int prm_long_value = std::atoi(prm_value.c_str());
						prm_value_buffer = &prm_long_value;
						prm_buffer_size = sizeof(int);
	
						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);						
					} 
					else if(std::find(std::begin(real_numbers_types), std::end(real_numbers_types), prm_type)) {
						double prm_dbl_value = std::atof(prm_value.c_str());
						prm_value_buffer = &prm_dbl_value;
						prm_buffer_size = sizeof(double);

						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);  						
					} 
					else {
						std::string prm_string_value = prm_value;
						prm_value_buffer = &prm_string_value;
						prm_buffer_size = sizeof(std::string);

						int msg_err = m_damaris->damaris_pdi_parameter_set(prm_name.c_str(), prm_value_buffer, prm_buffer_size);  						
					}
				}

				prm_name_concat.append(prm_name + ", ");
				m_config.reset_parameter_depends_on(prm_name);
			}
			prm_name_concat.pop_back();
			prm_name_concat.pop_back();
			context().logger().info("data `{}' Is a needed metadata for the evaluation of parameters {}", name, prm_name_concat);
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
					context().logger().info("data `{}' will be written at: block '{}' and position '{}:{}:{}', when = '{}'", name, block, position[0], position[1], position[2], ds_write_info.when.to_long(context()));

					const void* data = static_cast<const void*>(rref.get());

					std::string set_block_pos_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_SET_BLOCK_POSITION);
					m_event_handler.damaris_api_call_event(context(), m_damaris, set_block_pos_event_name, multi_expose_transaction_dataname, name, block, position);

					std::string write_block_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_WRITE_BLOCK);
					m_event_handler.damaris_api_call_event(context(), m_damaris, write_block_event_name, multi_expose_transaction_dataname, name, block, data);	
				
					if(m_config.is_there_after_write_events()) {
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
					}
				}				
			}
			else{
				context().logger().error("The Damaris need write access over the data (`{}')", name);
			}
		}
		else if(m_config.is_parameter_to_update(name)){
			std::pair<std::string, Desc_type> prm_to_update_info = m_config.get_parameter_to_update_info(name);
			std::string prm_name = prm_to_update_info.first;
			size_t size;

			Ref_r rref = ref;
			Ref_rw rwref = ref;

			if (rref && prm_to_update_info.second == Desc_type::PRM_TO_SET) {
				Datatype_sptr ref_type = rref.type();
				if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(ref_type)) {
					size = rref.scalar_value<size_t>();
				} else {
					throw Type_error{"Damaris paramegter must be a scalar"};
				}

				std::string prm_set_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_PARAMETER_SET);
				m_event_handler.damaris_api_call_event(context(), m_damaris, prm_set_event_name, multi_expose_transaction_dataname, prm_name, name, size);
			}
			else if (rwref && prm_to_update_info.second == Desc_type::PRM_TO_GET) {
				Datatype_sptr ref_type = rwref.type();
				if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(ref_type)) {
					size = rwref.scalar_value<size_t>();
				} else {
					throw Type_error{"Damaris paramegter must be a scalar"};
				}

				std::string prm_get_event_name = m_event_handler.get_event_name(Event_type::DAMARIS_PARAMETER_GET);
				m_event_handler.damaris_api_call_event(context(), m_damaris, prm_get_event_name, multi_expose_transaction_dataname, prm_name, name, size);
			}
			else {
				//Error handling!
			}
		}
		//is_client_get !?
		else if(name == m_config.is_client_dataset_name()) {	
			context().logger().info("'{}' == m_config.is_client_dataset_name() = '{}'", name, (name == m_config.is_client_dataset_name()));
					
			if (Ref_w wref = ref) {
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
		if(m_event_handler.is_damaris_api_call_event(event_name)){
			context().logger().info("event `{}' has been triggered", event_name);

			context().logger().info("is_damaris_api_call_event ( `{}' ) = TRUE", event_name);
			m_event_handler.damaris_api_call_event(context(), m_damaris, event_name, multi_expose_transaction_dataname);

			multi_expose_transaction_dataname.clear();
			//multi_expose_transaction_dataref.clear();
		}
		else {//Non Damaris call event
			
		}
	}

	~damaris_plugin()
	{
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