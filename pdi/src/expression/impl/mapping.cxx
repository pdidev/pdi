// SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "mapping.h"

namespace PDI {

using std::dynamic_pointer_cast;
using std::make_shared;
using std::max;
using std::move;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::vector;

Expression::Impl::Mapping::Mapping(PC_tree_t value)
{
	size_t size = len(value);
	for (int i = 0; i < size; i++) {
		m_value.emplace(PDI::to_string(PC_get(value, "{%d}", i)), Expression{parse(PC_get(value, "<%d>", i))});
	}
}

Expression::Impl::Mapping::Mapping(const unordered_map< string, Expression >& value)
{
	for (const auto& element: value) {
		m_value.emplace(element.first, element.second);
	}
}

unique_ptr<Expression::Impl> Expression::Impl::Mapping::clone() const
{
	return unique_ptr<Impl>{new Expression::Impl::Mapping(m_value)};
}

long Expression::Impl::Mapping::to_long(Context& ctx) const
{
	throw Value_error{"Cannot interpret Map_expression as a long value"};
}

double Expression::Impl::Mapping::to_double(Context& ctx) const
{
	throw Value_error{"Cannot interpret Map_expression as a double value"};
}

string Expression::Impl::Mapping::to_string(Context& ctx) const
{
	throw Value_error{"Cannot interpret Map_expression as a string value"};
}

Ref Expression::Impl::Mapping::to_ref(Context& ctx) const
{
	vector<Record_datatype::Member> members;
	size_t displacement = 0;
	size_t record_alignment = 1;
	for (const auto& element: m_value) {
		Ref_rw element_ref{element.second.to_ref(ctx)};

		size_t alignment = element_ref.type()->alignment();
		record_alignment = max(record_alignment, alignment);

		// align the next member
		displacement += (alignment - (displacement % alignment)) % alignment;
		members.emplace_back(displacement, element_ref.type(), element.first);
		displacement += element_ref.type()->buffersize();
	}
	//add padding at the end of record
	displacement += (record_alignment - (displacement % record_alignment)) % record_alignment;

	return Impl::to_ref(ctx, Record_datatype::make(move(members), displacement));
}

size_t Expression::Impl::Mapping::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	if (auto&& record_type = dynamic_pointer_cast<const Record_datatype>(type)) {
		for (const auto& element: m_value) {
			auto member_it = find_if(record_type->members().begin(), record_type->members().end(), [&element](const Record_datatype::Member m) {
				return m.name() == element.first;
			});
			if (member_it != record_type->members().end()) {
				void* to = static_cast<uint8_t*>(buffer) + member_it->displacement();
				element.second.m_impl->copy_value(ctx, to, member_it->type());
			} else {
				throw Value_error{"Trying to reference non-existing member: {}", element.first};
			}
		}
		return type->buffersize();
	} else {
		throw Value_error{"Map literal cannot copy value of not record datatype"};
	}
}

} // namespace PDI
