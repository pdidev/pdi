/*******************************************************************************
 * Copyright (C) 2026 Centre National de la Recherche Scientifique (CNRS)
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
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>

#include <string>

/**
 * @brief initializes VeloC
 *
 * @param comm the MPI communicator to initialize VeloC with
 * @param veloc_file path to VeloC configuration file
 */
void init(MPI_Comm comm, std::string veloc_file);

/**
 * @brief registers a data structure as a region to be checkpointed/recovered
 *
 * @param id the identifier of the data to register
 * @param ptr pointer to the data to register
 * @param n_elements number of elements in the data structure
 * @param element_bytes size in bytes of each element
 * @param name name of the data (used in error messages)
 */
void protect_data(int id, const void* ptr, size_t n_elements, size_t element_bytes, std::string name);

/**
 * @brief de-registers a data structure to stop tracking it for checkpoint/restart
 *
 * @param id the identifier of the data to de-register
 * @param name name of the data (used in error messages)
 */
void unprotect_data(int id, std::string name);

/**
 * @brief writes a checkpoint using VeloC memory based API
 *
 * @param label name of the checkpoint
 * @param version version of the checkpoint (iteration number)
 */
void write_checkpoint(std::string label, int version);

/**
 * @brief reads a checkpoint using VeloC memory based API
 *
 * @param logger the plugin logger
 * @param label name of the checkpoint
 * @param version version of the checkpoint (iteration number)
 * @return int version of the checkpoint read
 */
int read_checkpoint(PDI::Logger& logger, std::string label, int version);

/**
 * @brief starts a checkpoint phase using VeloC file-based API
 *
 * @param label name of the checkpoint
 * @param version version of the checkpoint (iteration number)
 */
void init_checkpoint(std::string label, int version);

/**
 * @brief routes a user-defined file path to the VeloC-managed checkpoint file path
 *
 * @param logger the plugin logger
 * @param input_filename the user-defined file path
 * @param output_filename output buffer receiving the routed file path
 */
void route_file(PDI::Logger& logger, const std::string& input_filename, char* output_filename);

/**
 * @brief ends a checkpoint phase using VeloC file-based API
 *
 */
void end_checkpoint();

/**
 * @brief starts a restart phase using VeloC file-based API
 *
 * @param logger the plugin logger
 * @param label name of the checkpoint
 * @param version version of the checkpoint (iteration number)
 */
void init_restart(PDI::Logger& logger, std::string label, int version);

/**
 * @brief ends a restart phase using VeloC file-based API
 *
 */
void end_restart();

/**
 * @brief finalizes VeloC
 *
 */
void finalize();

#endif
