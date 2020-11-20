/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_RECORD_DATATYPE_H_
#define PDI_RECORD_DATATYPE_H_

#include <string>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>


namespace PDI {

/** A Record_datatype is a Datatype that represents a fixed number of
 * elements of potentially different types layed out in a specific way in
 * memory. Each element is given a name to access it.
 */
class PDI_EXPORT Record_datatype:
	public Datatype
{
public:
	/** Member accessor for record datatype
	 */
	class Member_accessor : public Accessor_base
	{
		/// Name of the member that will be returned
		std::string m_member_name;
		
		std::string access_kind() const override;
	public:
		/** Construct a new member accessor
		 *
		 * \param member_name name of the member that will be returned
		 */
		Member_accessor(const std::string& member_name);
		
		std::pair<void*, Datatype_uptr> access(const Record_datatype& record_type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	};
	
	/** A Member is one of the elements inside a Record_datatype
	 */
	class Member
	{
		/// Offset or distance in byte from the Record_datatype start
		size_t m_displacement;
		
		/// Type of the contained member
		Datatype_uptr m_type;
		
		/// Name of this specific member
		std::string m_name;
		
	public:
		/** Construct a new member
		 *
		 * \param displacement offset or distance in byte from the Record_datatype start
		 * \param type type of the contained member
		 * \param name name of this specific member
		 */
		Member(size_t displacement, Datatype_uptr type, const std::string& name);
		
		/** Construct a new member by copy
		 *
		 * \param o the Member to copy
		 */
		Member(const Member& o);
		
		/** Access the offset or distance in byte from the Record_datatype start
		 *
		 * \return the offset or distance in byte from the Record_datatype start
		 */
		size_t displacement() const;
		
		/** Access the type of the contained member
		 *
		 * \return the type of the contained member
		 */
		const Datatype& type() const;
		
		/** Access the name of this specific member
		 *
		 * \return the name of this specific member
		 */
		const std::string& name() const;
		
		/** Tests another member for equality
		 *
		 * \param rhs the other member to compare
		 * \return true if the members are equal
		 */
		bool operator==(const Member& rhs) const;
		
		/** Tests another member for inequality
		 *
		 * \param rhs the other member to compare
		 * \return true if the members are different
		 */
		bool operator!=(const Member& rhs) const;
		
	};
	
private:
	/// All members in increasing displacement order
	std::vector<Member> m_members;
	
	/// The total size of the buffer containing all members
	size_t m_buffersize;
	
public:
	/** Constructs a new Record_datatype
	 *
	 * \param members the members for the newly created Record_datatype in
	 *        increasing displacement order
	 * \param size the total size of the buffer containing all members
	 */
	Record_datatype(std::vector<Member>&& members, size_t size);
	
	/** Accesses the members in increasing displacement order
	 */
	const std::vector<Member>& members() const;
	
	
	Datatype_template_uptr clone() const override;
	
	Datatype_uptr clone_type() const override;
	
	Datatype_uptr densify() const override;
	
	Datatype_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
	bool simple() const override;
	
	void* data_to_dense_copy(void* to, const void*) const override;
	
	void* data_from_dense_copy(void* to, const void*) const override;
	
	std::pair<void*, Datatype_uptr> subaccess_by_iterators(void* from,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	    
	void destroy_data(void*) const override;
	
	std::string debug_string() const override;
	
	bool operator== (const Datatype&) const override;
	
};

} // namespace PDI

#endif // PDI_RECORD_DATATYPE_H_
