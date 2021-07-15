/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_DATATYPE_H_
#define PDI_DATATYPE_H_

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>


namespace PDI {

/** A Datatype is a Datatype_template that accepts no argument.
 * It represents the memory layout of data and supports some simple operations
 * on it:
 * * accessing its content
 * * cloning and destruction
 */
class PDI_EXPORT Datatype:
	public Datatype_template
{
public:
	/** Base class for datatype accesssors, that allow to get pointer to subtype
	 */
	struct Accessor_base {
		/** Access function for array datatype
		 * \param type a datatype to get access
		 * \param from pointer to data of type datatype
		 * \param remaining_begin iterator to the beginning of remaining accessors
		 * \param remaining_end iterator to the end of remaining accessors
		 * \return string that inform what access is made
		 */
		virtual std::pair<void*, Datatype_uptr> access(const Array_datatype& type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const;
		    
		/** Access function for pointer datatype
		 * \param type a datatype to get access
		 * \param from pointer to data of type datatype
		 * \param remaining_begin iterator to the beginning of remaining accessors
		 * \param remaining_end iterator to the end of remaining accessors
		 * \return string that inform what access is made
		 */
		virtual std::pair<void*, Datatype_uptr> access(const Pointer_datatype& type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const;
		    
		/** Access function for record datatype
		 * \param type a datatype to get access
		 * \param from pointer to data of type datatype
		 * \param remaining_begin iterator to the beginning of remaining accessors
		 * \param remaining_end iterator to the end of remaining accessors
		 * \return string that inform what access is made
		 */
		virtual std::pair<void*, Datatype_uptr> access(const Record_datatype& type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const;
		    
		/** Returns access kind as string
		 * \return string that inform what access is made
		 */
		virtual std::string access_kind() const = 0;
		
		/** Creates and returns clone of accessor
		 * \return clone of accessor
		 */
		virtual std::unique_ptr<Accessor_base> clone() const = 0;
		
		/** Destroys the accessor
		*/
		virtual ~Accessor_base() = default;
	};
	
	/** Creates a new datatype
	 *
	 * \param[in] attributes attributes of the datatype
	 */
	Datatype(const Attributes_map& attributes = {});
	
	~Datatype() override;
	
	/** Creates a new datatype as an exact copy of this one
	 *
	 * \return the dense type that is produced
	 */
	virtual Datatype_uptr clone_type() const = 0;
	
	/** Test for equality
	 *
	 * \param other the Datatype to compare
	 * \return true if the Datatype's are equal
	 */
	virtual bool operator== (const Datatype& other) const = 0;
	
	/** Test for inequality
	 *
	 * \param other the Datatype to compare
	 * \return true if the Datatype's are different
	 */
	bool operator!=(const Datatype& other) const;
	
	/** Creates a new datatype as the dense copy of this one
	 *
	 * \return the type that is produced
	 */
	virtual Datatype_uptr densify() const = 0;
	
	/** Indicate if the datatype is dense or not
	 *
	 * \return whether the datatype is dense
	 */
	virtual bool dense() const = 0;
	
	/** Computes the data size of a type, excluding potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t datasize() const = 0;
	
	/** Computes the data size of a type, including potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t buffersize() const = 0;
	
	/** Returns the required alignment for a type
	 *
	 * \return the size in bytes
	 */
	virtual size_t alignment() const = 0;
	
	/**
	 * Tells if data can be copied as bytes (if type is dense) and doesn't need a destroyer
	 *
	 * \return true if data has trivial copier and destroyer, false otherwise
	 */
	virtual bool simple() const = 0;
	
	/**
	 * Creates a dense deep copy of data
	 *
	 * \param[in] to the pointer to the allocated memory to fill (dense data)
	 * \param[in] from the pointer to the copied data (size of buffersize)
	 * \return updated `to' pointer
	 */
	virtual void* data_to_dense_copy(void* to, const void* from) const = 0;
	
	/**
	 * Creates a sparse deep copy of dense data
	 *
	 * \param[in] to the pointer to the allocated memory to fill (size of buffersize)
	 * \param[in] from the pointer to the copied data (dense data)
	 * \return updated `to' pointer
	 */
	virtual void* data_from_dense_copy(void* to, const void* from) const = 0;
	
	/**
	 * Creates datatype of subtype and returns it with a moved pointer
	 *
	 * \param[in] from the pointer to the data
	 * \param[in] accessor accessor to get subtype of datatype
	 * \return pointer with offset and new datatype
	 */
	std::pair<void*, Datatype_uptr> subaccess(void* from, const Accessor_base& accessor) const;
	
	/**
	 * Creates datatype of subtype and returns it with a moved pointer
	 *
	 * \param[in] from the pointer to the data
	 * \param[in] accessors accessors to get nested subtype of datatype
	 * \return pointer with offset and new datatype
	 */
	std::pair<void*, Datatype_uptr> subaccess(void* from, const std::vector<std::unique_ptr<Accessor_base>>& accessors) const;
	
	/**
	 * Creates datatype of subtype and returns it with a moved pointer
	 *
	 * \param[in] from the pointer to the data
	 * \param[in] remaining_begin iterator to the begin of remaining accessors
	 * \param[in] remaining_end iterator to the end of remaining accessors
	 * \return pointer moved by offset and new datatype
	 */
	virtual std::pair<void*, Datatype_uptr> subaccess_by_iterators(void* from,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const;
	    
	/**
	 * Function used to delete the data behind the datatype. This should not deallocate the memory.
	 *
	 * \param[in] ptr to the data to free
	 */
	virtual void destroy_data(void* ptr) const = 0;
	
	/** Returns the datatype yaml representation as a string
	 *
	 * \return the datatype yaml representation as a string
	 */
	virtual std::string debug_string() const = 0;
	
};

} // namespace PDI

#endif // PDI_DATATYPE_H_
