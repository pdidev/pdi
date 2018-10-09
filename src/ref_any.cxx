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

#include "config.h"

#include <cassert>
#include <cstring>
#include <cstdint>
#include <map>
#include <memory>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"

#include "pdi/ref_any.h"


namespace PDI {

Ref Ref_base::do_copy(Ref_r ref)
{
	Datatype_uptr densified_type {ref.type().densify()};
	// no std::aligned_alloc or std::align_val_t in C++14, hand-written version
	// size + (densified_type->alignment() - 1) <- we want to make sure that we fit the data even though the worst alignment occur
	size_t size = densified_type->buffersize() + (densified_type->alignment() - 1);
	void* buffer = operator new (size);
	void* data = std::align(densified_type->alignment(), densified_type->buffersize(), buffer, size);
	try {
		ref.type().data_to_dense_copy(data, ref.get());
	} catch (...) {
		::operator delete (buffer);
		throw;
	}
	return Ref {data, [buffer](void*){operator delete (buffer);}, std::move(densified_type), true, true};
}


const Datatype& Ref_base::type() const noexcept
{
	if (!m_content || !m_content->m_buffer) return UNDEF_TYPE;
	return *m_content->m_type;
}

} // namespace PDI
