/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>


namespace PDI {

class PDI_EXPORT Scalar_datatype:
	public Datatype
{
	/// Size of the content in bytes or 0 if unknown
	size_t m_size;
	
	/// Size of the alignment in bytes
	size_t m_align;
	
	/// Interpretation of the content
	Scalar_kind m_kind;
	
public:
	Scalar_datatype(Scalar_kind kind, size_t size);
	
	Scalar_datatype(Scalar_kind kind, size_t size, size_t align);
	
	/** Interpretation of the content
	 */
	Scalar_kind kind() const;
	
	Type_template_uptr clone() const override;
	
	Data_type_uptr clone_type() const override;
	
	Data_type_uptr densify() const override;
	
	Data_type_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
};

const Scalar_datatype UNDEF_TYPE = {Scalar_kind::UNKNOWN, 0};

} // namespace PDI

#endif // PDI_SCALAR_DATATYPE_H_
