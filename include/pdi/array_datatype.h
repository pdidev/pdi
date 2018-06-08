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

#ifndef PDI_ARRAY_DATATYPE_H_
#define PDI_ARRAY_DATATYPE_H_

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>
#include <pdi/logger.h>

namespace PDI {

class PDI_EXPORT Array_datatype:
	public Datatype
{
	/// Type of the elements contained in the array.
	Datatype_uptr m_subtype;
	
	/// Number of elements the array can store
	size_t m_size;
	
	/// id of the first actual element of the array
	size_t m_start;
	
	/// Number of actual elements in the array
	size_t m_subsize;

	/// Global logger of PDI
	Logger logger {spdlog::get("logger")};
	
	/// Global logger of PDI
	Logger logger {spdlog::get("logger")};
	
public:
	Array_datatype(Datatype_uptr subtype, size_t size, size_t start, size_t subsize);
	
	Array_datatype(Datatype_uptr subtype, size_t size);
	
	/** Type of the elements contained in the array.
	 */
	const Datatype& subtype() const;
	
	/** Number of elements the array can store
	 */
	size_t size() const;
	
	/** id of the first actual element of the array
	 */
	size_t start() const;
	
	/** Number of actual elements in the array
	 */
	size_t subsize() const;
	
	Datatype_template_uptr clone() const override;
	
	Datatype_uptr clone_type() const override;
	
	Datatype_uptr densify() const override;
	
	Datatype_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
};

} // namespace PDI

#endif // PDI_ARRAY_DATATYPE_H_
