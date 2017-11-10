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
* \file Data_content.h
* \brief .
* \author C. Roussel, corentin.roussel@cea.fr
*/
// Created:  08/09/2017 17:45:52

#ifndef DATA_CONTENT_H__
#define DATA_CONTENT_H__

#include <iostream>
#include <memory>
#include <unordered_set>

#include <pdi.h>

#include <pdi/data_content_fwd.h>
#include <pdi/data_reference_fwd.h>
#include <pdi/datatype.h>
#include <pdi/plugin.h>


/** the possible memory mode for a content **/
typedef enum PDI_memmode_e {
	PDI_MM_NONE = 0,
	/// PDI is responsible for freeing the memory
	PDI_MM_FREE = 0x08, // start at 8 so as to be ORable w. PDI_inout_t
} PDI_memmode_t;

/**  Binary OR and AND operator **/
PDI_memmode_t operator|(PDI_memmode_t a, PDI_memmode_t b);
PDI_memmode_t &operator|=(PDI_memmode_t &lhs, PDI_memmode_t rhs);
PDI_memmode_t operator&(PDI_memmode_t a, PDI_memmode_t b);
PDI_memmode_t &operator&=(PDI_memmode_t &lhs, PDI_memmode_t rhs);


namespace PDI
{

/// Wrapper of free
void PDI_EXPORT destroyer_free(void *buffer, void *context);

/// Wrapper of delete
template <typename T>
void PDI_EXPORT destroyer_delete(T *t, void *context);

/** \class  Data_content
*   \brief  Manipulate and grant access to a buffer depending on the remaining right access (read/write).
*/
class Data_content
{
public:
	/* ****** LIFECYCLE  ****** */
	Data_content();   ///< constructor
	~Data_content();  ///< destructor
	Data_content(const Data_content &) = delete ; ///< unused
	Data_content(Data_content &&) = delete;       ///< unused
	
	PDI_status_t init(void *buffer, Destroyer func, PDI_inout_t permission, const PDI_datatype_t &type); ///< Initialized a Data_content
	
	/* ****** ACCESSORS  ****** */
	bool is_writable(); ///< True if no references has writing nor reading access right.
	bool is_readable(); ///< True if is initialized and no reference can write.
	bool has_reader();  ///< True if the number of reader > 0
	
	void *get_buffer() const; ///< return the buffer address
	const PDI_datatype_t &get_type() const;///< return a copy of the PDI_datatype_t or the a pointer on the current PDI_datatype_t
	
	/* ****** METHODS ****** */
	void add_memory_mode(const PDI_memmode_t ownership); ///< Set memory mode (who is the owner of the buffer).
	PDI_status_t copy_metadata(); ///< duplicate the buffer content
	
	friend class Data_ref; //< Data_ref when created/destroyed update the unordered set "m_refs"
private:
	/*  ****** METHODS ******* */
	bool try_lock(PDI_inout_t access);  ///< retain right access
	bool lock(PDI_inout_t access);  ///< retain right access
	bool unlock(PDI_inout_t access); ///< return right access
	PDI_status_t reclaim(Data_ref *ignore); ///< The buffer is reclaimed and data_end is sent
	
	
	/* ****** DATA MEMBERS  ****** */
	void *m_buffer;       ///< buffer that contains data
	bool m_initialized;   ///< m_buffer has been initialized
	int m_nb_reader;      ///< number of concurrent reader (if >1 , is_writable = False)
	bool m_writable;      ///< m_buffer is writable (if True, m_nb_reader=0)
	bool m_writer;      ///< m_buffer has writer
	Destroyer m_delete;   ///< free memory allocated for the buffer
	void *m_context;      ///< a context that is consumed by the destroyer
	PDI_datatype_t m_type;     ///< type of the data inside the buffer
	PDI_memmode_t m_memory_mode;     ///< type of the data inside the buffer
	std::unordered_set< Data_ref *> m_refs; ///< Class that reference this instance
	
}; // *****  end of class Data_content  *****

} // *****  end of PDI namespace  *****

#endif //  DATA_CONTENT_H__
