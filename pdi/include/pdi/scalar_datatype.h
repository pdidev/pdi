/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_SCALAR_DATATYPE_H_
#define PDI_SCALAR_DATATYPE_H_

#include <functional>
#include <string>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>


namespace PDI {

class PDI_EXPORT Scalar_datatype:
	public Datatype
{
	/// Size of the content in bytes or 0 if unknown
	size_t m_size;
	
	/// Size of the densified content in bytes
	size_t m_dense_size;
	
	/// Size of the alignment in bytes
	size_t m_align;
	
	/// Interpretation of the content
	Scalar_kind m_kind;
	
	/// copy function or null for memcpy
	std::function<void* (void*, const void*)> m_copy;
	
	/// destroy function or null for memcpy
	std::function<void(void*)> m_destroy;
	
public:
	Scalar_datatype(Scalar_kind kind, size_t size);
	
	Scalar_datatype(Scalar_kind kind, size_t size, size_t align);
	
	Scalar_datatype(Scalar_kind kind, size_t size, size_t align, size_t dense_size, std::function<void* (void*, const void*)> copy, std::function<void(void*)> destroy);
	
	/** Interpretation of the content
	 */
	Scalar_kind kind() const;
	
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
	
	void destroy_data(void* ptr) const override;
	
	std::string debug_string() const override;
	
	bool operator== (const Datatype&) const override;
	
};

const Scalar_datatype UNDEF_TYPE = {Scalar_kind::UNKNOWN, 0};

} // namespace PDI

#endif // PDI_SCALAR_DATATYPE_H_