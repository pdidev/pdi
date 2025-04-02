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

#include <pdi/ref_any.h>
#include "damaris_api_call_handler.h"
#include "damaris_cfg.h"

using PDI::Context;
using PDI::Ref;
using PDI::Ref_w;
using PDI::Ref_r;
using std::unordered_map;
using std::map;
using std::unordered_set;
using std::list;
using std::string;
using std::unique_ptr;


namespace damaris_pdi {

Damaris_api_call_handler::Damaris_api_call_handler(std::string cfg_object)
{
    xml_config_object = cfg_object;
}

Damaris_api_call_handler::Damaris_api_call_handler(std::string cfg_object, PDI::Expression comm)
{
    xml_config_object = cfg_object;
    m_communicator = comm;
}

Damaris_api_call_handler::Damaris_api_call_handler(std::string cfg_object, PDI::Expression comm, std::string init_on_event, std::string start_on_event, std::string stop_on_event)
{
    xml_config_object = cfg_object;
    m_communicator = comm;
    m_init_on_event = init_on_event;
    m_start_on_event = start_on_event;
    m_stop_on_event = stop_on_event;
}

std::string Damaris_api_call_handler::get_event_name(Event_type event_type)
{
    return event_names.at(event_type);
}

bool Damaris_api_call_handler::is_damaris_api_call_event(std::string event_name)
{   
    for(auto event : event_names) {
        if(event_name == event.second)
            return true;
    }
    return false;
}


void Damaris_api_call_handler::damaris_api_call_event(Context& ctx, unique_ptr<Damaris_wrapper> &m_damaris, std::string event_name, list<string> expose_dataname, ...)
{
    //************************************************************ */
    //Events : Damaris Initialize and Damaris Start
    //************************************************************ */
    if(event_name == event_names.at(Event_type::DAMARIS_INITIALIZE))  {

        damaris_pdi_init(ctx, m_damaris, xml_config_object.c_str());

		if (m_start_on_event.empty()) {

            ctx.logger().info("Plugin sent damaris_start() to Damaris, in initialize");	

			std::string start_event_name = this->get_event_name(Event_type::DAMARIS_START);
		    PDI_status_t status = PDI_event(start_event_name.c_str());
		}
    }
    else if(event_name == event_names.at(Event_type::DAMARIS_START)) {
        // DAMARIS_START
        // The following call starts the servers. Servers will run inside this
        // function until they are asked to stop by clients. On clients,
        // is_client will be set to 1 (0 on servers).
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_start() before plugin initialization (`{}')", event_name);
            return;
        }
        std::string arg1_name;
        int is_client;
        int err = m_damaris->damaris_pdi_start(&is_client);
        m_damaris->set_is_client(is_client);
        ctx.logger().info("------------------- CALLED damaris_pdi_start Return IS_CLIENT = '{}')", is_client);

        int arg_pos = 0;
        int nb_awaited_args = 1;
        int transaction_data_size = expose_dataname.size();
        auto it = expose_dataname.begin();
        //Position to the first needed parameter
        advance(it, (transaction_data_size - nb_awaited_args));
        //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
        //while (arg_pos < nb_awaited_args)
        for (; it != expose_dataname.end(); it++) {
            ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
            
            if (arg_pos == 1 && strcmp("is_client", it->c_str()) == 0)
            {				
                ctx.logger().info("------------------- CALLED damaris_pdi_start {} = '{}')", arg1_name, is_client);
                
                int *inout_is_client;
                PDI_access(it->c_str(), (void**)&inout_is_client, PDI_INOUT);
                *inout_is_client = is_client;
                arg1_name = it->c_str();
            }
            else
                //if(nb_awaited_args <= arg_pos)
                break;
        }
    } 

    //************************************************************ */
    //Events that rely on Multi expose
    //************************************************************ */        
    // DAMARIS_PARAMETER_GET
    else if(event_name == event_names.at(Event_type::DAMARIS_PARAMETER_GET)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_parameter_get() before plugin initialization (`{}')", event_name);
            return;
        }
        ctx.logger().info("------------------- INNNNN DAMARIS_PARAMETER_GET Event...");

        char* var_name; std::string arg1_name = "prm_name";
        void* buffer;  std::string arg2_name = "prm_buffer";
        unsigned int* size; std::string arg3_name = "prm_size";

	    va_list extra_args;
	    va_start(extra_args, expose_dataname);
        string data_name = va_arg(extra_args, string);
        if(!data_name.empty()) {
            var_name  = (char*)data_name.c_str();
            buffer    = va_arg(extra_args, void*);
            ctx.logger().info("------------------- INNNNN DAMARIS_PARAMETER_GET Event.. EXTRA ARGS.... AFTER buffer");
            *size     = va_arg(extra_args, int);
            ctx.logger().info("------------------- INNNNN DAMARIS_PARAMETER_GET Event.. EXTRA ARGS.... AFTER size...");


            ctx.logger().info("------------------- CALLING damaris_pdi_parameter_get arg_pos({}==='{}', {}==='{}', {}==='{}')", arg1_name, var_name, arg2_name, *(int *)buffer, arg3_name, *(int*)size);
            int err = m_damaris->damaris_pdi_parameter_get((const char*)var_name, (void *) buffer, *(int*)size);
        }
    
    } 
    // DAMARIS_PARAMETER_SET
    else if(event_name == event_names.at(Event_type::DAMARIS_PARAMETER_SET)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_parameter_set() before plugin initialization (`{}')", event_name);
            return;
        }

        char* var_name; std::string arg1_name = "prm_name";
        void* buffer;  std::string arg2_name = "prm_buffer";
        unsigned int* size; std::string arg3_name = "prm_size";

	    va_list extra_args;
	    va_start(extra_args, expose_dataname);
        string data_name = va_arg(extra_args, string);
        if(!data_name.empty()) {
            var_name  = (char*)data_name.c_str();
            buffer    = va_arg(extra_args, void*);
            *size     = va_arg(extra_args, size_t);

            ctx.logger().info("------------------- CALLING damaris_pdi_parameter_set arg_pos({}==='{}', {}==='{}', {}==='{}')", arg1_name, var_name, arg2_name, *(int *)buffer, arg3_name, *(int*)size);
            int err = m_damaris->damaris_pdi_parameter_set((const char*)var_name, (const void *) buffer, *(int*)size);    
        }
    } 
    // DAMARIS_CLIENT_COMM_GET
    else if(event_name == event_names.at(Event_type::DAMARIS_CLIENT_COMM_GET))  {

        MPI_Comm client_comm; std::string arg1_name;

        int err = m_damaris->damaris_pdi_client_comm_get(&client_comm);

        int arg_pos = 0;
        int nb_awaited_args = 1;
        int transaction_data_size = expose_dataname.size();
        auto it = expose_dataname.begin();
        //Position to the first needed parameter
        advance(it, (transaction_data_size - nb_awaited_args));
        //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
        //while (arg_pos < nb_awaited_args)
        for (; it != expose_dataname.end(); it++) {
            ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
            
            if (arg_pos == 1)
            {				
                static MPI_Comm* comm;
                PDI_access(it->c_str(), (void**)&comm, PDI_INOUT);
                *comm = client_comm;//Update the value
                arg1_name = it->c_str();
            }
            else
                //if(nb_awaited_args <= arg_pos)
                break;
        }
        //ctx.logger().info("------------------- CALLING damaris_pdi_client_comm_get arg_pos({}==='{}')", arg1_name, client_comm);
    }
    // DAMARIS_SET_POSITION
    else if(event_name == event_names.at(Event_type::DAMARIS_SET_POSITION)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_set_position() before plugin initialization (`{}')", event_name);
            return;
        }

        //Retrive parameters! sent via Multi expose, the two last sent data
        char* var_name; std::string arg1_name;
        int64_t* position; std::string arg2_name;
        int arg_pos = 0;
        int nb_awaited_args = 2;
        int transaction_data_size = expose_dataname.size();
        auto it = expose_dataname.begin();
        //Position to the first needed parameter
        advance(it, (transaction_data_size - nb_awaited_args));
        //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
        //while (arg_pos < nb_awaited_args)
        for (; it != expose_dataname.end(); it++) {
            ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
            
            if (arg_pos == 1)
            {				
                PDI_access(it->c_str(), (void**)&var_name, PDI_IN);
                arg1_name = it->c_str();
            }
            else if (arg_pos == 2){
                PDI_access(it->c_str(), (void**)&position, PDI_IN);
                arg2_name = it->c_str();
            }
            else
                //if(nb_awaited_args <= arg_pos)
                break;
        }

        ctx.logger().info("------------------- CALLING damaris_pdi_set_position arg_pos({}==='{}', {}==={})", arg1_name, var_name, arg2_name, *(int64_t*)position);
        int err = m_damaris->damaris_pdi_set_position((const char*)var_name, (const int64_t*)position);
    } 
    // DAMARIS_WRITE
    else if(event_name == event_names.at(Event_type::DAMARIS_WRITE)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_write() before plugin initialization (`{}')", event_name);
            return;
        }

        //Retrive parameters! sent via Multi expose, the two last sent data
        char* var_name; std::string arg1_name;
        void* data; std::string arg2_name;
        int arg_pos = 0;
        int nb_awaited_args = 2;
        int transaction_data_size = expose_dataname.size();
        auto it = expose_dataname.begin();
        //Position to the first needed parameter
        advance(it, (transaction_data_size - nb_awaited_args));
        //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
        //while (arg_pos < nb_awaited_args)
        for (; it != expose_dataname.end(); it++) {
            ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
            
            if (arg_pos == 1)
            {				
                PDI_access(it->c_str(), (void**)&var_name, PDI_IN);
                arg1_name = it->c_str();
            }
            else if (arg_pos == 2){
                PDI_access(it->c_str(), (void**)&data, PDI_IN);
                arg2_name = it->c_str();
            }
            else
                //if(nb_awaited_args <= arg_pos)
                break;
        }

        ctx.logger().info("------------------- CALLING damaris_pdi_write arg_pos({}==='{}', {}==={})", arg1_name, var_name, arg2_name, *(int*)data);
        int err = m_damaris->damaris_pdi_write((const char*)var_name, (void*)data);
    } 
    // DAMARIS_SET_BLOCK_POSITION
    else if(event_name == event_names.at(Event_type::DAMARIS_SET_BLOCK_POSITION)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_set_position() before plugin initialization (`{}')", event_name);
            return;
        }

        char* var_name; std::string arg1_name = "bpos_var_name";
        int32_t* block;  std::string arg2_name = "dom";
        int64_t* position; std::string arg3_name = "position";

	    va_list extra_args;
	    va_start(extra_args, expose_dataname);
        string data_name = va_arg(extra_args, string);
        if(!data_name.empty()) {
            var_name  = (char*)data_name.c_str();
            *block    = va_arg(extra_args, int32_t);
            position = va_arg(extra_args, int64_t*);
        }
        else {
            //Retrive parameters! sent via Multi expose, the three last sent data
            int arg_pos = 0;
            int nb_awaited_args = 3;
            int transaction_data_size = expose_dataname.size();
            auto it = expose_dataname.begin();
            //Position to the first needed parameter
            advance(it, (transaction_data_size - nb_awaited_args));
            //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
            //while (arg_pos < nb_awaited_args)
            for (; it != expose_dataname.end(); it++) {
                ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
                
                if (arg_pos == 1)
                {				
                    PDI_access(it->c_str(), (void**)&var_name, PDI_IN);
                    arg1_name = it->c_str();
                }
                else if (arg_pos == 2){
                    PDI_access(it->c_str(), (void**)&block, PDI_IN);
                    arg2_name = it->c_str();
                }
                else if (arg_pos == 3){
                    PDI_access(it->c_str(), (void**)&position, PDI_IN);
                    arg3_name = it->c_str();
                }
                else
                    //if(nb_awaited_args <= arg_pos)
                    break;
            }
        }

        ctx.logger().info("------------------- CALLING damaris_pdi_set_block_position arg_pos({}==='{}', {}==='{}', {}==='{}')", arg1_name, var_name, arg2_name, (int32_t) *block, arg3_name, *(int64_t*)position);
        int err = m_damaris->damaris_pdi_set_block_position((const char*)var_name, (int32_t) *block, (const int64_t*)position);
    } 
    // DAMARIS_WRITE_BLOCK
    else if(event_name == event_names.at(Event_type::DAMARIS_WRITE_BLOCK)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_write() before plugin initialization (`{}')", event_name);
            return;
        }
        char* var_name; std::string arg1_name = "wb_var_name";
        int32_t* block;  std::string arg2_name = "dom";
        void* data; std::string arg3_name = "data";

	    va_list extra_args;
	    va_start(extra_args, expose_dataname);
        //if(string data_name = va_arg(extra_args, string)) {
        string data_name = va_arg(extra_args, string);
        if(!data_name.empty()) {
            var_name  = (char*)data_name.c_str();
            *block    = va_arg(extra_args, int32_t);
            data     = va_arg(extra_args, void*);//const void*
        }
        else {
            //Retrive parameters! sent via Multi expose, the three last sent data
            int arg_pos = 0;
            int nb_awaited_args = 3;
            int transaction_data_size = expose_dataname.size();
            auto it = expose_dataname.begin();
            //Position to the first needed parameter
            advance(it, (transaction_data_size - nb_awaited_args));
            //for (auto it = expose_dataname.rbegin(); it != expose_dataname.rend(); ++it) {
            //while (arg_pos < nb_awaited_args)
            for (; it != expose_dataname.end(); it++) {
                ctx.logger().info("Multi expose: Reclaiming `{}' ({}/{})", it->c_str(), ++arg_pos, expose_dataname.size());
                
                if (arg_pos == 1)
                {				
                    PDI_access(it->c_str(), (void**)&var_name, PDI_IN);
                    arg1_name = it->c_str();
                }
                else if (arg_pos == 2){
                    PDI_access(it->c_str(), (void**)&block, PDI_IN);
                    arg2_name = it->c_str();
                }
                else if (arg_pos == 3){
                    PDI_access(it->c_str(), (void**)&data, PDI_IN);
                    arg3_name = it->c_str();
                }
                else
                    //if(nb_awaited_args <= arg_pos)
                    break;
            }
        }

        ctx.logger().info("------------------- CALLING damaris_pdi_write_block arg_pos({}==='{}', {}==='{}', {}==='{}')", arg1_name, var_name, arg2_name, (int32_t) *block, arg3_name, *(int*)data);
        int err = m_damaris->damaris_pdi_write_block((const char*)var_name, (int32_t) *block, (void*)data);
    } 

    //************************************************************ */
    //Events : End Iteration / Damaris Stop and Damaris Finalize
    //************************************************************ */
    else if (event_name == event_names.at(Event_type::DAMARIS_END_ITERATION)) {
        if (m_damaris) {

            ctx.logger().info("Plugin called damaris_end_iteration()");

            int err = m_damaris->damaris_pdi_end_iteration() ;	
            //iteration++;

            ctx.logger().info("Plugin sent damaris_end_iteration() to Damaris");					
        } else {
            ctx.logger().warn("Trying to call damaris_end_iteration() before plugin initialization (`{}')", event_name);
        }
    } 
    else if(event_name == event_names.at(Event_type::DAMARIS_STOP)) {
        // DAMARIS_STOP is called and the Daamris server processes will return from the damaris_start() call 
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_strop() before plugin initialization (`{}')", event_name);
            return;
        }
        
        int err = m_damaris->damaris_pdi_stop();
    } 		
    else if(event_name == event_names.at(Event_type::DAMARIS_FINALIZE)) {
        if (!m_damaris) {
            ctx.logger().warn("Trying to call damaris_strop() before plugin initialization (`{}')", event_name);
            return;
        }

        if (m_stop_on_event.empty()) {
            ctx.logger().info("Plugin sent damaris_stop() to Damaris, in finalization");	
            
            //std::string stop_event_name = this->get_event_name(Event_type::DAMARIS_STOP);
            //PDI_status_t status = PDI_event(stop_event_name.c_str());
            
            int stop_err = m_damaris->damaris_pdi_stop();
        }
        
        int err = m_damaris->damaris_pdi_finalize();
    }
    else {
        assert(false && "Unexpected damaris event type");
    }
}


void Damaris_api_call_handler::damaris_pdi_init(Context& ctx, unique_ptr<Damaris_wrapper> &m_damaris, const char* damaris_xml_object) 
{
    if (!m_damaris) {
        MPI_Comm comm = MPI_COMM_WORLD;
        if (m_communicator) {
			//comm = static_cast<const MPI_Comm>(m_communicator.to_string(ctx));
			//comm = *(static_cast<const MPI_Comm*>(Ref_r{m_communicator.to_ref(ctx)}.get()));	
        }

        // This creator method calls damaris_initialize(), passin in an XML file, so if we want to 
        // pre-fill our xml file, we need to do this somehow before:
        // PDI_expose("mpi_comm", &main_comm, PDI_INOUT);
        
        m_damaris.reset(new Damaris_wrapper{ctx, damaris_xml_object, comm});						
        
        ctx.logger().info("Plugin initialized successfully");
    }
}

} // namespace damaris_pdi
