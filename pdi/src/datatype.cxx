// SPDX-FileCopyrightText: 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

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
