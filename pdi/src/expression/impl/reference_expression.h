/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_EXPRESSION_IMPL_REFERENCE_EXPRESSION_H_
#define PDI_EXPRESSION_IMPL_REFERENCE_EXPRESSION_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/// Forward declaration of base class for expression reference accessors
struct Accessor_expression;

/** An expression implemented by a a reference to a data
 */
class PDI_NO_EXPORT Expression::Impl::Reference_expression: public Expression::Impl
{
	/// The referenced data
	std::string m_referenced;

	/// fmt format of referenced data in to_string (starting with `:')
	std::string m_fmt_format;

	/// Subelements (sequence of index and member accessors)
	std::vector<std::unique_ptr<Accessor_expression>> m_subelements;

public:
	/** Creates empty reference expression
	 */
	Reference_expression();

	/** Copies reference expression
	 *
	 * \param[in] other the reference expression to copy
	 */
	Reference_expression(const Reference_expression& other);

	/** Copies reference expression
	 *
	 * \param[in] other the reference expression to copy
	 * \return copy of reference expression
	 */
	Reference_expression& operator= (const Reference_expression& other);

	std::unique_ptr<Impl> clone() const override;

	long to_long(Context& ctx) const override;

	double to_double(Context& ctx) const override;

	std::string to_string(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;

	size_t copy_value(Context& ctx, void* buffer, Datatype_sptr type) const override;

	static std::unique_ptr<Impl> parse(char const ** val_str);
};

} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_REFERENCE_EXPRESSION_H_
