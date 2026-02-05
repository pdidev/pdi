/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_EXPRESSION_IMPL_SEQUENCE_H_
#define PDI_EXPRESSION_IMPL_SEQUENCE_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a a sequence
 */
struct PDI_NO_EXPORT Expression::Impl::Sequence: public Expression::Impl {
	std::vector<Expression> m_value;

	Sequence(PC_tree_t value);

	Sequence(const std::vector<Expression>& value);

	std::unique_ptr<Impl> clone() const override;

	long to_long(Context& ctx) const override;

	double to_double(Context& ctx) const override;

	std::string to_string(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;

	size_t copy_value(Context& ctx, void* buffer, Datatype_sptr type) const override;
};


} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_SEQUENCE_H_
