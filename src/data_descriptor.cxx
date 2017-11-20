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

#include "config.h"

#include "pdi/data_descriptor.h"
#include "pdi/datatype.h"

#include "status.h"

namespace PDI {

using std::string;

Data_descriptor::Data_descriptor(const char* name):
		m_config(PC_parse_string(const_cast<char*>(""))),
		m_metadata(false),
		m_name(name)
{
	PDI_datatype_init_scalar(&m_type, PDI_T_UNDEF);
}

Data_descriptor::Data_descriptor(const string& name):
		m_config(PC_parse_string(const_cast<char*>(""))),
		m_metadata(false),
		m_name(name)
{
	PDI_datatype_init_scalar(&m_type, PDI_T_UNDEF);
}

Data_descriptor::~Data_descriptor()
{
	PDI_datatype_destroy(&m_type);
}

PDI_status_t Data_descriptor::init(PC_tree_t config, bool is_metadata, const PDI_datatype_t& type)
{
	m_config = config;
	m_metadata = is_metadata;
	PDI_datatype_destroy(&m_type);
	return PDI_datatype_copy(&m_type, &type);
}

const PDI_datatype_t &Data_descriptor::get_type() const
{
	return m_type;
}

bool Data_descriptor::is_metadata() const
{
	return m_metadata;
}

PC_tree_t Data_descriptor::get_config() const
{
	return m_config;
}

} // namespace PDI
