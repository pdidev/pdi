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
 * \file Data_ref.h
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
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

/** A dynamically typed reference to data with automatic memory management and
 * read/write locking semantic.
 * 
 * Data_ref is a smart pointer that features:
 * - cycle-free garbage collecting similar to std::shared_ptr 
 * - a dynamic type system,
 * - a read/write locking mechanism,
 * - a release system that nullifies all existing references to the raw data
 *   before returning it,
 * - a notification system to be notified when the raw data is to be deleted
 * 
 * The lock system can not be relied upon in a multithreaded environment.
 * 
 * \author Corentin Roussel (CEA) <corentin.roussel@cea.fr>
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 */
class Data_ref
{
public:
	/** Constructs a null ref
	 */
	Data_ref ();
	
	/** Creates a reference to currently unreferenced data
	 * \param desc a descriptor of the data to reference
	 * \param data the raw data to reference
	 * \param access the maximum allowed access to the underlying content
	 */
	Data_ref(const Data_descriptor& desc, void* data, PDI_inout_t access, PDI_inout_t lock=PDI_NONE);
	
	/** Copies an existing reference
	 * \param other the ref to copyv
	 */
	Data_ref(const Data_ref &other, PDI_inout_t lock=PDI_NONE);
	
	/** The move constructor
	 * \param other the ref to move
	 */
	Data_ref(Data_ref &&other, PDI_inout_t lock=PDI_NONE);
	
	
	/** Copies an existing reference into this one
	 * \param other the ref to copy
	 * \return *this
	 */
	Data_ref &operator= (const Data_ref &other);
	
	/** Moves an existing reference into this one
	 * \param other the ref to move
	 * \return *this
	 */
	Data_ref &operator= (Data_ref &&other);
	
	/** Destructor
	 */
	~Data_ref();
	
	operator bool () const;
	
	//TODO: add a function to register a deletion callback
	
	/* ****** METHODS  ****** */
	PDI_status_t reclaim(); ///< Inform the PDI::Data_content that the buffer is reclaimed. All others references and pending operation are stopped.
	
	/* ****** ACCESSORS  ****** */
	std::shared_ptr< Data_content> get_content() const;  ///< shared the contained data_content
	const std::string& get_name() const;
	const Data_descriptor& get_desc() const;
	
	bool try_grant(PDI_inout_t access); ///< Check if (additional) priviledge can be granted.
	bool grant(PDI_inout_t access); ///< Ask for (additional) priviledge
	bool revoke(PDI_inout_t access); ///< Release current priviledge on the content
	PDI_inout_t  priviledge() const; ///< Return the current priviledge
	bool priviledge(PDI_inout_t access) const; ///< Return true if (access & m_access)
	
	
	friend class Data_content;
	
private:
	/// Reset reference
	void clear();
	
	/// increase priviledge
	bool add_priviledge(const PDI_inout_t inout);
	
	/// decrease priviledge
	bool rm_priviledge(const PDI_inout_t inout);
	
	/// wrap the plugin function above, call this to release the data.
	PDI_status_t data_end();
	
	/** shared pointer on the data content
	 * \todo make Data_content private to this class
	 * \todo replace by a raw pointer we manage ourselves
	 */
	std::shared_ptr< Data_content >  m_content;
	
	/// Authorized access using this reference
	PDI_inout_t m_access;
	
	/** function to use before releasing the data. Wrapper below.
	 * \todo replace by a set in Data_content
	 */
	PDI_data_end_f m_data_end;
	
	/** The descriptor that lead to the creation of this data (might be null)
	 * \todo move to Data_content
	 */
	const Data_descriptor *m_desc;
	
}; // class Data_ref

} // namespace PDI

#endif //  DATA_REF_H__
