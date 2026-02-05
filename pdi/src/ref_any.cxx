// SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "config.h"

#include <cassert>
#include <cstdint>
#include <cstring>
#include <map>
#include <memory>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"

#include "pdi/ref_any.h"

namespace PDI {

Ref Reference_base::do_copy(Ref_r ref)
{
	Datatype_sptr densified_type{ref.type()->densify()};
	if (!densified_type->buffersize()) {
		return Ref{};
	}
	// no std::aligned_alloc or std::align_val_t in C++14, hand-written version
	// size + (densified_type->alignment() - 1) <- we want to make sure that we fit the data even though the worst alignment occur
	size_t size = densified_type->buffersize() + (densified_type->alignment() - 1);
	void* buffer = operator new (size);
	void* data = std::align(densified_type->alignment(), densified_type->buffersize(), buffer, size);
	try {
		ref.type()->data_to_dense_copy(data, ref.get());
	} catch (...) {
		::operator delete (buffer);
		throw;
	}
	return Ref{data, [buffer](void*) { operator delete (buffer); }, std::move(densified_type), true, true};
}

Datatype_sptr Reference_base::type() const noexcept
{
	if (!m_content || !m_content->m_data) return UNDEF_TYPE;
	return m_content->m_type;
}

} // namespace PDI
