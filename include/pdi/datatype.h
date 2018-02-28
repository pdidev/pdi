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

#ifndef PDI_DATATYPE_H_
#define PDI_DATATYPE_H_

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>


namespace PDI {

class PDI_EXPORT Datatype:
	public Datatype_template
{
public:
	~Datatype();
	
	/** Creates a new datatype as an exact copy of this one
	 *
	 * \return the dense type that is produced
	 */
	virtual Data_type_uptr clone_type() const = 0;
	
	/** Creates a new datatype as the dense copy of this one
	 *
	 * \return the type that is produced
	 */
	virtual Data_type_uptr densify() const = 0;
	
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
	
	/** Return the required alignment for a type
	 *
	 * \return the size in bytes
	 */
	virtual size_t alignment() const = 0;
	
};

} // namespace PDI

#endif // PDI_DATATYPE_H_
