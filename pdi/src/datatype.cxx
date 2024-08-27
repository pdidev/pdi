/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <spdlog/spdlog.h>

#include "pdi/error.h"
#include "pdi/fmt.h"

#include "pdi/datatype.h"

namespace PDI {

using fmt::join;
using std::pair;
using std::static_pointer_cast;
using std::string;
using std::unique_ptr;
using std::vector;

Datatype::Datatype(const Attributes_map& attributes)
	: Datatype_template(attributes)
{}

Datatype::~Datatype() = default;

bool Datatype::operator!= (const Datatype& rhs) const
{
	return !(*this == rhs);
}

Datatype_sptr Datatype::index(size_t) const
{
	throw Type_error{"unable to access element by index in {}", debug_string()};
}

std::pair<void*, Datatype_sptr> Datatype::index(size_t, void*) const
{
	throw Type_error{"unable to access element by index in {}", debug_string()};
}

Datatype_sptr Datatype::slice(size_t, size_t) const
{
	throw Type_error{"unable to access slice in {}", debug_string()};
}

std::pair<void*, Datatype_sptr> Datatype::slice(size_t, size_t, void*) const
{
	throw Type_error{"unable to access slice in {}", debug_string()};
}

Datatype_sptr Datatype::member(const char*) const
{
	throw Type_error{"unable to access member in {}", debug_string()};
}

std::pair<void*, Datatype_sptr> Datatype::member(const char*, void*) const
{
	throw Type_error{"unable to access member in {}", debug_string()};
}

Datatype_sptr Datatype::dereference() const
{
	throw Type_error{"unable to dereference {}", debug_string()};
}

std::pair<void*, Datatype_sptr> Datatype::dereference(void*) const
{
	throw Type_error{"unable to dereference {}", debug_string()};
}

} // namespace PDI
