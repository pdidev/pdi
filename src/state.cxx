/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

/**
 * \file api.c
 * \brief PDI internal data
 * \author Julien Bigot (CEA) <julien-bigot@cea.fr>
 */

#include "config.h"

#include <iostream>

#include "pdi/plugin.h"
#include "pdi/data_reference.h"
#include "pdi/status.h"

#include "pdi/state.h"


using PDI::Data_ref;
using std::cerr;
using std::endl;
using std::stack;
using std::string;


PDI_state_t PDI_state;


PDI::Data_descriptor &PDI_state_t::desc(const char *name)
{
	return m_descriptors.emplace(name, name).first->second;
}

PDI::Data_descriptor &PDI_state_t::desc(const std::string &name)
{
	return m_descriptors.emplace(name, name.c_str()).first->second;
}
