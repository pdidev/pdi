/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/expression.h>
#include <veloc.h>

#include <iostream>
#include <vector>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <optional>

void init(MPI_Comm comm, std::string veloc_file);

void protect_data(PDI::Context& ctx,int id, void * ptr, size_t n, size_t sub_bytes);

void unprotect_data(PDI::Context& ctx, int id);

int write_checkpoint(PDI::Context& ctx, std::optional<const PDI::Expression> when, 
	std::string label, std::string iter_name);

int load_checkpoint(PDI::Context& ctx, std::string label);

void finalize();

#endif 
