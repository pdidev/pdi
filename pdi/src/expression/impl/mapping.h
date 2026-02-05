/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_
#define PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a a mapping
 */
struct PDI_NO_EXPORT Expression::Impl::Mapping: public Expression::Impl {
	std::unordered_map<std::string, Expression> m_value;

	Mapping(PC_tree_t value);

	Mapping(const std::unordered_map< std::string, PDI::Expression >& value);

	std::unique_ptr<Impl> clone() const override;

	long to_long(Context& ctx) const override;

	double to_double(Context& ctx) const override;

	std::string to_string(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;

	size_t copy_value(Context& ctx, void* buffer, Datatype_sptr type) const override;
};

} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_
