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

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/array_datatype.h"


namespace PDI {

using std::max;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

Array_datatype::Array_datatype(Data_type_uptr subtype, size_t size, size_t start, size_t subsize):
	m_subtype {std::move(subtype)},
	m_size{std::move(size)},
	m_start{std::move(start)},
	m_subsize{std::move(subsize)}
{}

Array_datatype::Array_datatype(Data_type_uptr subtype, size_t size):
	Array_datatype{std::move(subtype), size, 0, std::move(size)}
{}

const Datatype& Array_datatype::subtype() const
{
	return *m_subtype;
}

size_t Array_datatype::size() const
{
	return m_size;
}

size_t Array_datatype::start() const
{
	return m_start;
}

size_t Array_datatype::subsize() const
{
	return m_subsize;
}

Type_template_uptr Array_datatype::clone() const
{
	return clone_type();
}

Data_type_uptr Array_datatype::clone_type() const
{
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->clone_type(), m_size, m_start, m_subsize}};
}

Data_type_uptr Array_datatype::densify() const
{
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->densify(), m_subsize}};
}

Data_type_uptr Array_datatype::evaluate(Context&) const
{
	return Array_datatype::clone_type();
}

bool Array_datatype::dense() const
{
	if (m_size != m_subsize) return false;
	return m_subtype->dense();
}

size_t Array_datatype::datasize() const
{
	return m_subsize * m_subtype->datasize();
}

size_t Array_datatype::buffersize() const
{
	return m_size * m_subtype->datasize();
}

size_t Array_datatype::alignment() const
{
	return m_subtype->alignment();
}


} // namespace PDI
