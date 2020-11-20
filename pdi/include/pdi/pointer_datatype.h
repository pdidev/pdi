/*******************************************************************************
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

#ifndef PDI_POINTER_DATATYPE_H_
#define PDI_POINTER_DATATYPE_H_

#include <functional>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>


namespace PDI {

class PDI_EXPORT Pointer_datatype:
	public Datatype
{
	/// Type of data that pointer is pointing
	Datatype_uptr m_subtype;
	
	/// Copy function or null for memcpy
	std::function<void* (void*, const void*)> m_copy;
	
	/// Destroy function or null for memcpy
	std::function<void(void*)> m_destroy;
	
public:
	/** Accessor for pointer datatype
	 */
	class Accessor: public Accessor_base
	{
		std::string access_kind() const override;
		
	public:
		std::pair<void*, Datatype_uptr> access(const Pointer_datatype& pointer_type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	};
	
	Pointer_datatype(Datatype_uptr subtype);
	
	Pointer_datatype(Datatype_uptr subtype, std::function<void* (void*, const void*)> copy, std::function<void(void*)> destroy);
	
	/** Type of the pointed element
	 *
	 * \return the type of the pointed element
	 */
	const Datatype& subtype() const;
	
	Datatype_template_uptr clone() const override;
	
	Datatype_uptr clone_type() const override;
	
	Datatype_uptr densify() const override;
	
	Datatype_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
	bool simple() const override;
	
	void* data_to_dense_copy(void* to, const void* from) const override;
	
	void* data_from_dense_copy(void* to, const void* from) const override;
	
	std::pair<void*, Datatype_uptr> subaccess_by_iterators(void* from,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	    
	void destroy_data(void* ptr) const override;
	
	std::string debug_string() const override;
	
	bool operator== (const Datatype&) const override;
	
	Datatype_uptr dereference() const;
	
};

} // namespace PDI

#endif // PDI_POINTER_DATATYPE_H_
