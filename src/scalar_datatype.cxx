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

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/scalar_datatype.h"


namespace PDI {

using std::max;
using std::string;
using std::transform;
using std::unique_ptr;

Scalar_datatype::Scalar_datatype(Scalar_kind kind, size_t size):
	m_size{size},
	m_align{size},
	m_kind{kind}
{}

Scalar_datatype::Scalar_datatype(Scalar_kind kind, size_t size, size_t align):
	m_size{size},
	m_align{align},
	m_kind{kind}
{}

Scalar_kind Scalar_datatype::kind() const
{
	return m_kind;
}

Datatype_template_uptr Scalar_datatype::clone() const
{
	return clone_type();
}

Datatype_uptr Scalar_datatype::clone_type() const
{
	return unique_ptr<Scalar_datatype> {new Scalar_datatype{m_kind, m_size, m_align}};
}

Datatype_uptr Scalar_datatype::densify() const
{
	return unique_ptr<Scalar_datatype> {new Scalar_datatype{m_kind, m_size, m_align}};
}

Datatype_uptr Scalar_datatype::evaluate(Context&) const
{
	return clone_type();
}

bool Scalar_datatype::dense() const
{
	return true;
}

size_t Scalar_datatype::datasize() const
{
	return m_size;
}

size_t Scalar_datatype::buffersize() const
{
	return m_size;
}

size_t Scalar_datatype::alignment() const
{
	return m_align;
}

} // namespace PDI

