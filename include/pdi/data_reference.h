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
 * \file Data_ref.h
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
 */
// Created:  08/09/2017 17:45:15

/** \class  Data_ref
 *
 *  \brief  Reference and access a shared Data_content with restricted right.
 *
 *  This class allows to manipulate a shared Data_content with restricted right access.
 *  When the content stop being available it sends a terminating function and loose access and ownership.
 */

#ifndef DATA_REF_H__
#define DATA_REF_H__

#include <memory>

#include <pdi.h>

#include <pdi/data_descriptor_fwd.h>
#include <pdi/data_content_fwd.h>
#include <pdi/plugin.h>

namespace PDI
{

class Data_ref
{
public:
	/* ****** LIFECYCLE  ****** */
	Data_ref();  ///< default constructor, empty object
	Data_ref(const Data_ref &); ///< Copy contructor, THE DUPLICATED REFERENCE HAS NO RIGHT OF ACCESS
	Data_ref(Data_ref &&); ///< Move constructor
	~Data_ref();  ///< Destructor
	
	/// Inialized a Data_ref with or without a data_end function
	PDI_status_t init(const std::string name, const std::shared_ptr< Data_content > content, const PDI_data_end_f data_end, const PDI_inout_t inout);
	PDI_status_t init(const std::string name, const std::shared_ptr< Data_content > content, const PDI_inout_t inout);
	
	/* ****** OPERATORS  ****** */
	Data_ref &operator = (Data_ref &&ref); ///< Move operator
	Data_ref &operator = (const Data_ref &ref); ///< Copy operator -- Right of access are removed in the process
	operator bool () const;
	
	/* ****** METHODS  ****** */
	PDI_status_t reclaim(); ///< Inform the PDI::Data_content that the buffer is reclaimed. All others references and pending operation are stopped.
	
	/* ****** ACCESSORS  ****** */
	std::shared_ptr< Data_content> get_content() const;  ///< shared the contained data_content
	const Data_descriptor &get_desc() const;  ///< return a pointer to the descriptor
	const std::string &get_name() const; ///< Get the data name
	bool is_metadata() const; ///< return true if this is a metadata
	
	bool try_grant(PDI_inout_t access); ///< Check if (additional) priviledge can be granted.
	bool grant(PDI_inout_t access); ///< Ask for (additional) priviledge
	bool revoke(PDI_inout_t access); ///< Release current priviledge on the content
	PDI_inout_t  priviledge() const; ///< Return the current priviledge
	bool priviledge(PDI_inout_t access) const; ///< Return true if (access & m_access)
	
	
	friend class Data_content;
	
private:
	/* ****** LIFECYCLE  ****** */
	void clear(); /// Reset reference
	bool add_priviledge(const PDI_inout_t inout); /// increase priviledge
	bool rm_priviledge(const PDI_inout_t inout); /// decrease priviledge
	
	/* ****** DATA MEMBERS  ****** */
	std::shared_ptr< Data_content >  m_content; ///< shared pointer on the data content
	PDI_inout_t m_access;      ///< Authorized access using this reference
	PDI_data_end_f m_data_end;  ///< function to use before releasing the data. Wrapper below.
	Data_descriptor *m_desc;   ///< metadata that defines the name, PDI_datatype and ...etc.
	
	/* ****** METHODS  ****** */
	PDI_status_t data_end(); ///< wrapp the plugin function above, call this to release the data.
	
}; // *****  end of class Data_ref  *****

} // ***** end of PDI namespace

#endif //  DATA_REF_H__
