/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_EXPRESSION_IMPL_FLOAT_LITERAL_H_
#define PDI_EXPRESSION_IMPL_FLOAT_LITERAL_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a a float literal
 */
struct PDI_NO_EXPORT Expression::Impl::Float_literal: public Expression::Impl {
	double m_value;

	Float_literal(double value);

	std::unique_ptr<Impl> clone() const override;

	long to_long(Context& ctx) const override;

	double to_double(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;

	size_t copy_value(Context& ctx, void* buffer, Datatype_sptr type) const override;

	static std::unique_ptr<Impl> parse(char const ** val_str);
};

} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_FLOAT_LITERAL_H_
