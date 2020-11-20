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

#include "config.h"

#include <spdlog/fmt/fmt.h>

#include <pdi/error.h>

#include "pdi/datatype.h"


namespace PDI {

using fmt::join;
using std::pair;
using std::string;
using std::unique_ptr;
using std::vector;

pair<void*, Datatype_uptr> Datatype::Accessor_base::access(const Array_datatype& type,
																void* from,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	throw Type_error{"Invalid {} access to an array datatype", access_kind()};
}

pair<void*, Datatype_uptr> Datatype::Accessor_base::access(const Pointer_datatype& type,
																void* from,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	throw Type_error{"Invalid {} access to a pointer datatype", access_kind()};
}

pair<void*, Datatype_uptr> Datatype::Accessor_base::access(const Record_datatype& type,
																void* from,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
																vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	throw Type_error{"Invalid {} access to a record datatype", access_kind()};
}

pair<void*, Datatype_uptr> Datatype::subaccess(void* from, const Accessor_base& accessor) const
{
	vector<unique_ptr<Accessor_base>> accessors {};
	accessors.emplace_back(const_cast<Accessor_base*>(&accessor));
	pair<void*, Datatype_uptr> result = subaccess_by_iterators(from, accessors.cbegin(), accessors.cend());
	accessors.begin()->release();
	return result;
}

pair<void*, Datatype_uptr> Datatype::subaccess(void* from, const vector<unique_ptr<Accessor_base>>& accessors) const
{
	return subaccess_by_iterators(from, accessors.begin(), accessors.end());
}

pair<void*, Datatype_uptr> Datatype::subaccess_by_iterators(void* from,
											vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
											vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (remaining_begin == remaining_end) {
		throw Type_error{"Invalid subaccess to type: {}", debug_string()};
	} else {
		vector<string> access_kinds;
		for (auto it = remaining_begin; it != remaining_end; it++) {
			access_kinds.emplace_back(remaining_begin->get()->access_kind());
		}
		throw Type_error{"Invalid subaccess using accessors: {}\n To type: {}", join(access_kinds, "; "), debug_string()};
	}
}

bool Datatype::operator!=(const Datatype& rhs) const
{
	return !(*this == rhs);
}

Datatype::~Datatype() = default;

} // namespace PDI
