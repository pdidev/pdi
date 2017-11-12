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
/* The following is used for doxygen documentation */
/**
 * \file data_descriptor.cxx
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
 */
// Created:  08/09/2017 08:21:55


#include "config.h"

#include "pdi/data_descriptor.h"
#include "pdi/datatype.h"

#include "status.h"

namespace PDI
{

/// Copy constructor
Data_descriptor::Data_descriptor(const Data_descriptor &from):
	m_config(from.m_config),
	m_metadata(from.m_metadata)
{
	PDI_datatype_copy(&m_type, &from.m_type);
}


/// init data descriptor
PDI_status_t Data_descriptor::init(PC_tree_t config, bool is_metadata, const PDI_datatype_t &type)
{
	PDI_status_t status(PDI_OK);
	handle_PC_err(PC_status(config), err0);
	m_config = config;
	m_metadata = is_metadata;
	PDI_handle_err(PDI_datatype_copy(&m_type, &type), err0);
	
err0:
	return status;
}

/// Destructor
Data_descriptor::~Data_descriptor()
{
	PDI_datatype_destroy(&m_type);
}

/// Copy operator
Data_descriptor &Data_descriptor::operator= (const Data_descriptor &from)
{
	m_config = from.m_config;
	m_metadata = from.m_metadata;
	PDI_datatype_copy(&m_type, &from.m_type);
	return *this;
}


/* ****** ACCESSORS  ****** */
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
