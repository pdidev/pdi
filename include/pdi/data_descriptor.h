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
 * \file Data_descriptor.h
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
 */
// Created:  11/09/2017 14:22:31


#ifndef DATA_DESCRIPTOR_H__
#define DATA_DESCRIPTOR_H__

#include <memory>

#include <paraconf.h>
#include <pdi.h>

#include "pdi/datatype.h"


/** the possible kind of data
 */
typedef enum PDI_datakind_e {
	PDI_DK_DATA = 0,
	PDI_DK_METADATA
} PDI_datakind_t;


namespace PDI
{

/** \class  Data_descriptor
 *  \brief  Describe the content of a buffer.
 */
class Data_descriptor
{
public:
	/* ****** LIFECYCLE  ****** */
	Data_descriptor() = default;  ///< Create empty descriptor
	Data_descriptor(const Data_descriptor &); ///< copy constructor
	~Data_descriptor(); ///< Destructor
	
	PDI_status_t init(const std::string &name, PC_tree_t config, bool is_metadata, const PDI_datatype_t &type);  ///< initialized descriptor
	
	/* ****** OPERATORS ****** */
	Data_descriptor &operator= (const Data_descriptor &);  ///< Copy operator
	
	/* ****** ACCESSORS  ****** */
	const std::string &get_name() const; ///< return the data name
	const PDI_datatype_t &get_type() const;   ///< Return the datatype
	PC_tree_t get_config() const;       ///< Return the PC_tree_t config
	bool is_metadata() const;           ///< Return true if the data is a metadata
	
private:
	/* ****** DATA MEMBERS  ****** */
	std::string m_name;
	PC_tree_t m_config;
	bool m_metadata;
	PDI_datatype_t m_type;
	
}; // *****  end of class Data_descriptor  *****

} // ***** end of PDI namespace

#endif // DATA_DESCRIPTOR_H__
