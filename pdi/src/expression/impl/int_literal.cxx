// SPDX-FileCopyrightText: 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <cerrno>
#include <memory>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "int_literal.h"

namespace PDI {

using std::dynamic_pointer_cast;
using std::make_shared;
using std::unique_ptr;

Expression::Impl::Int_literal::Int_literal(long value)
	: m_value(value)
{}

long Expression::Impl::Int_literal::to_long(Context&) const
{
	return m_value;
}

double Expression::Impl::Int_literal::to_double(Context&) const
{
	return static_cast<double>(m_value);
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::clone() const
{
	return unique_ptr<Int_literal>{new Int_literal{*this}};
}

Ref Expression::Impl::Int_literal::to_ref(Context& ctx) const
{
	return Impl::to_ref(ctx, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)));
}

template <class T>
size_t from_long_cpy(void* buffer, long value_long)
{
	T value = static_cast<T>(value_long);
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Int_literal::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
		if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED) {
			switch (scalar_type->buffersize()) {
			case 1L:
				return from_long_cpy<uint8_t>(buffer, m_value);
			case 2L:
				return from_long_cpy<uint16_t>(buffer, m_value);
			case 4L:
				return from_long_cpy<uint32_t>(buffer, m_value);
			case 8L:
				return from_long_cpy<uint64_t>(buffer, m_value);
			default:
				throw Type_error{"Unknown size of integer datatype"};
			}
		} else if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
			switch (type->buffersize()) {
			case 1L:
				return from_long_cpy<int8_t>(buffer, m_value);
			case 2L:
				return from_long_cpy<int16_t>(buffer, m_value);
			case 4L:
				return from_long_cpy<int32_t>(buffer, m_value);
			case 8L:
				return from_long_cpy<int64_t>(buffer, m_value);
			default:
				break;
			}
		}
	}
	throw Value_error{"Cannot copy Int_literal as a non integer datatype->"};
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::parse(char const ** val_str)
{
	const char* constval = *val_str;

	errno = 0;
	unique_ptr<Int_literal> result{new Int_literal{strtol(constval, const_cast<char**>(&constval), 0)}};
	if (errno == ERANGE) {
		if (result > 0) {
			throw Value_error("Value too large for PDI integers: {}", constval);
		} else {
			throw Value_error("Value too small for PDI integers: {}", constval);
		}
	}
	if (*val_str == constval) {
		throw Value_error{"Expected integer, found `{}'", constval};
	}
	while (isspace(*constval))
		++constval;

	*val_str = constval;
	return result;
}

} // namespace PDI
