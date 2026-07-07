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

#ifndef VELOC_WRAPPER_H
#define VELOC_WRAPPER_H

#include <veloc.h>
#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>

#include <algorithm>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

/**
 * @brief initializes VeloC 
 * 
 * @param ctx the context from which to access the logger 
 * @param comm the MPI communicator to inizialize VeloC with  
 * @param veloc_file path to VeloC configuration file  
 */
void init(PDI::Context& ctx, MPI_Comm comm, std::string veloc_file);

/**
 * @brief registers a data structure as a region to be checkpointed/recovered  
 * 
 * @param ctx the context from which to access the logger 
 * @param id the identifier of the data to register 
 * @param ptr pointer to the data to register.
 * @param n_elements number of elements in the data structure 
 * @param element_bytes size in bytes of each element 
 */
void protect_data(PDI::Context& ctx, int id, const void* ptr, size_t n_elements, size_t element_bytes);

/**
 * @brief de-registers a data structure to stop tracking it for checkpoint/restart. 
 * 
 * @param ctx the context from which to access the logger 
 * @param id the identifier of the data to de-register 
 */
void unprotect_data(PDI::Context& ctx, int id);

/**
 * @brief writes a checkpoint using VeloC memory based API
 * 
 * @param ctx the context from which to access the logger  
 * @param label name of the checkpoint  
 * @param version version of the checkpoint (iteration number)
 */
void write_checkpoint(PDI::Context& ctx, std::string label, int version);

/**
 * @brief reads a checkpoint using VeloC memory based API
 * 
 * @param ctx the context from which to access the logger 
 * @param label name of the checkpoint  
 * @param version version of the checkpoint (iteration number) 
 * @return int version of the checkpoint read 
 */
int read_checkpoint(PDI::Context& ctx, std::string label, int version);

/**
 * @brief starts a checkpoint phase using VeloC file-based API 
 * 
 * @param ctx the context from which to access the logger 
 * @param label name of the checkpoint  
 * @param version version version of the checkpoint (iteration number)
 */
void init_checkpoint(PDI::Context& ctx, std::string label, int version);


/**
 * @brief routes a user-defined file path to the VeloC-managed checkpoint file path 
 *
 * @param ctx the context from which to access the logger.
 * @param input_filename the user-defined file path 
 * @param output_filename output buffer receiving the routed file path
 */
void route_file(PDI::Context& ctx, const std::string& input_filename, char* output_filename);

/**
 * @brief ends a checkpoint phase using VeloC file-based API 
 * 
 * @param ctx the context from which to access the logger 
 */
void end_checkpoint(PDI::Context& ctx);

/**
 * @brief starts a restart phase using VeloC file-based API 
 * 
 * @param ctx the context from which to access the logger 
 * @param label name of the checkpoint 
 * @param version version version of the checkpoint (iteration number)
 */
void init_restart(PDI::Context& ctx, std::string label, int version);

/**
 * @brief ends a restart phase using VeloC file-based API 
 * 
 * @param ctx the context from which to access the logger 
 */
void end_restart(PDI::Context& ctx);

/**
 * @brief finalizes VeloC
 * 
 * @param ctx the context from which to access the logger 
 */
void finalize(PDI::Context& ctx);

#endif
