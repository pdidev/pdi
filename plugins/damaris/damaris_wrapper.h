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


#ifndef DAMARIS_WRAPPER_H_
#define DAMARIS_WRAPPER_H_

#include <mpi.h>
#include <string>

#include <pdi/pdi_fwd.h>

//#include "damaris_cfg.h"

namespace damaris_pdi {

class Damaris_wrapper
{
     
    Damaris_wrapper(const Damaris_wrapper&) = delete;
    
    Damaris_wrapper& operator=(const Damaris_wrapper&) = delete;
    
    int m_is_client ;  // if != 0 (In this case == 1) then the process is a Damaris client (i.e. a simulation rank)
    
public:
    Damaris_wrapper(PDI::Context& ctx, const char* xmlConfigObject, MPI_Comm comm);
    // Damaris_wrapper(PDI::Context& ctx, const Damaris_cfg& config, MPI_Comm comm, PC_tree_t logging_tree);
    //Damaris_wrapper(PDI::Context& ctx, Damaris_cfg::Damaris_cfg& config, MPI_Comm comm);
    
//    void set_is_client(int is_client) ;
//    int  get_is_client( void ) ;
    

    void set_is_client(int is_client)
    {
        m_is_client = is_client ;
    }

    int get_is_client( void )
    {
       return m_is_client ;
    }

private:
    /**
     * Initializes Damaris, should be called after MPI_Init.
     * 
     * \param[in] configfile : name of the XML configuration file.
     * \param[in] comm : MPI communicator gathering all the nodes.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_initialize(const char* configfile, MPI_Comm comm);

public:
    /**
     * Finalize Damaris. Should be called before MPI_Finalize. If Damaris was 
     * started (damaris_start()), it should be stopped (using damaris_stop()) before this call.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_finalize( void );

    /**
     * Starts the server(s). Sets is_client to 1 if this core is a client.
     * Otherwise, this function starts the server and blocks until clients call 
     * damaris_stop, and is_client is set to 0.
     *
     * \param[out] is_client : indicates if this core is a client.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_start(int* is_client);

    /**
     * Stops the server. This function should only be called by client processes
     * and these processes must have called damaris_start to start the servers
     * before. When all client processes have called damaris_stop, server processes
     * blocked on damaris_start will return (with is_client set to 0).
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_stop( void );


    /** 
     * Writes a variable (similar to damaris_write_block(varnale,0,data)).
     *
     * \param[in] varname : name of the variable to write.
     * \param[in] data : pointer to the data to write.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     */
    int damaris_pdi_write(const char* varname,  const void* data);
    /** 
     * Writes a variable (similar to damaris_write_block(varnale,0,data)).
     *
     * \param[in] varname : name of the variable to write.
     * \param[in] data : pointer to the data to write.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     */
    int damaris_pdi_write(std::string varname,  const void* data);


    /**
     * Writes a block of a variable. The variable name should be the full name
     * of a variable defined in the configuration file. The block id should be
     * between 0 and the number of domains per client - 1, as defined in the
     * configuration file.
     *
     * \param[in] varname : name of the variable to write.
     * \param[in] block : id of the block to write.
     * \param[in] data : pointer to the data to write.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     */
    bool damaris_pdi_write_block(const char* varname, int32_t block, const void* data);
    /**
     * Writes a block of a variable. The variable name should be the full name
     * of a variable defined in the configuration file. The block id should be
     * between 0 and the number of domains per client - 1, as defined in the
     * configuration file.
     *
     * \param[in] varname : name of the variable to write.
     * \param[in] block : id of the block to write.
     * \param[in] data : pointer to the data to write.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     */
    bool damaris_pdi_write_block(std::string varname, int32_t block, const void* data);
    
    /**
     * Checks what a variables layout type is and returns an enumerated constant
     * indicating the type
     *
     * \param[in] varname : name of the variable to check the type of.
     * \param[out] vartype : enumerated constant indicating what data type the 
     *                       Variable data is defined with.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     */
    int damaris_pdi_get_type(const char* variable_name, DAMARIS_TYPE_STR *vartype);


    /**
     * Checks if a particular Damaris plugin is available.
     *
     * \param[out] vartype : enumerated constant indicating what data type the 
     *                       Variable data is defined with.
     *
     * \return 1 on plugin being available, 0 otherwise
     */
    int damaris_pdi_has_plugin(DAMARIS_PLUGIN_TYPE plugin);

    /**
     * Allocates the data required for a variable to be entirely written in memory.
     * Similar to damaris_alloc_block(varname,0,ptr).
     *
     * \param[in] varname : name of the variable to write.
     * \param[out] ptr : pointer to a pointer to the allocated memory.
     * 
     * \return DAMARIS_OK on success, other error codes on failure.
     */
    int damaris_pdi_alloc(const char* varname, void** ptr);

    /**
     * Allocates the data required for a block of a variable to be written 
     * in memory.
     *
     * \param[in] varname : name of the variable to write.
     * \param[in] block : block id for which to allocate memory.
     * \param[out] ptr : pointer to a pointer to the allocated memory.
     */
    int damaris_pdi_alloc_block(const char* varname, int32_t block, void** ptr);

    /**
     * Commits data associated to the variable on current iteration. This function
     * is equivalent to calling damaris_commit_block(varname,0).
     *
     * \param[in] varname : name of the variable to commit.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_commit(const char* varname);

    /**
     * Commits variable from the given iteration. This call is equivalent to
     * damaris_commit_block_iteration(varname,0,iteration).
     *
     * \param[in] varname : name of the variable to commit.
     * \param[in] iteration : iteration to commit.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_commit_iteration(const char* varname, int32_t iteration);

    /**
     * Commits a specific block of variable from the given iteration. The variable
     * should be defined in the configuration file and the block id should be
     * within the range defined by the number of domains per client.
     *
     * \param[in] varname : name of the variable to commit.
     * \param[in] block : block id.
     * \param[in] iteration : iteration to commit.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_commit_block_iteration(const char* varname, 
        int32_t block, int32_t iteration);

    /**
     * Clears the specified variable. This call is equivalent to 
     * damaris_clear_block(varname,0). Will transfer the responsibility of the data
     * to the dedicated cores.
     *
     * \param[in] varname : name of the variable to clear.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_clear(const char* varname);

    /**
     * Clears the block for the specified variable.
     *
     * \param[in] varname : name of the variable to clear.
     * \param[in] block : block id.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_clear_block(const char* varname, int32_t block);

    /**
     * Clears the specified variable at a specified iteration.
     *
     * \param[in] varname : name of the variable to clear.
     * \param[in] iteration : iteration to clear.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_clear_iteration(const char* varname, int32_t iteration);

    /**
     * Clears the block for the specified variable at a specified iteration.
     *
     * \param[in] varname : name of the variable to clear.
     * \param[in] iteration : iteration number.
     * \param[in] block : block id.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_clear_block_iteration(const char* varname, 
        int32_t block, int32_t iteration);

    /**
     * Sends a signal to the closest dedicated core.
     *
     * \param[in] signal_name : name of the signal to send, must correspond to an
     * event or a script in the configuration file.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_signal(const char* signal_name);

    /**
     * Associates a signal name to a function from the executable.
     *
     * \param[in] signal_name : name of the signal.
     * \param[in] sig : function to bind.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_bind(const char* signal_name, signal_t sig);

    /**
     * Get the current value of a parameter.
     *
     * \param[in] param_name : name of the parameter to read.
     * \param[out] buffer : buffer in which to put the value.
     * \param[in] size : maximum size (in bytes) in the buffer.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_parameter_get(const char* param_name, 
        void* buffer, unsigned int size);

    /**
     * Set the value of a parameter.
     *
     * \param[in] param_name : name of the parameter.
     * \param[in] buffer : buffer from which to take the value.
     * \param[in] size : size of the buffer.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_parameter_set(const char* param_name, 
        const void* buffer, unsigned int size);

    /**
     * Changes the position of the variable. Sets the meta-data that represents
     * the start of position within the variable of which the block stores the data
     * of.
     * Equivalent to a call to
     * damaris_set_block_position(var_name,0,position).
     * 
     * \param[in] var_name : name of the variable to move.
     * \param[in] position : array of new coordinates.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_set_position(const char* var_name, const int64_t* position);

    /**
     * Changes the position of a block of the variable.
     * 
     * \param[in] var_name : name of the variable to move.
     * \param[in] block : block id.
     * \param[in] position : array of new coordinates.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_set_block_position(const char* var_name, 
        int32_t block, const int64_t* position);

    /**
     * Gets the communicator that the clients must use.
     *
     * \param[out] comm : communicator gathering clients.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_client_comm_get(MPI_Comm* comm);

    /**
     * Ends the current iteration.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_end_iteration( void );

    /**
     * Gets the current iteration number.
     *
     * \param[out] iteration : current iteration number.
     *
     * \return DAMARIS_OK on success, other error codes on failures.
     *
     */
    int damaris_pdi_get_iteration(int* iteration);


    ~Damaris_wrapper() ;
}; // class Damaris_wrapper

} // namespace damaris_pdi
#endif // DAMARIS_WRAPPER_H_