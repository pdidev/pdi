/*******************************************************************************
 * Copyright (C) 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <iomanip>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include <spdlog/spdlog.h>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "operation.h"

#include "reference_expression.h"

namespace PDI {

using std::dynamic_pointer_cast;
using std::is_same;
using std::pair;
using std::setprecision;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

/// Base class for expression reference accessors
struct Accessor_expression {
	/** Accesses a type according to this Accessor_expression
	 *
	 * \param ctx in which to evaluate the expression
	 * \param ref the data to access
	 * \return The sub-type
	 */
	virtual Ref access(Context& ctx, Ref ref) const = 0;
	
	/** Clones expression reference accessor
	 * \return clone of this expression reference accessor
	 */
	virtual std::unique_ptr<Accessor_expression> clone() const = 0;
	
	/** Destroys expression accessor
	 */
	virtual ~Accessor_expression() = default;
};

/// Accessor used to access array element
class Index_accessor_expression: public Accessor_expression
{
	/// Expression to evaluate as index accessor
	Expression m_expression;
	
public:
	/** Creates new index accessors used for array access
	 * \param expression expression to evaluate as index accessor
	 */
	Index_accessor_expression(Expression expression)
		: m_expression{expression}
	{}
	
	Ref access(Context& ctx, Ref ref) const override
	{
		return ref[m_expression.to_long(ctx)];
	}
	
	std::unique_ptr<Accessor_expression> clone() const override
	{
		return unique_ptr<Accessor_expression> {new Index_accessor_expression{m_expression}};
	}
};

/// Accessor used to access record member
class Member_accessor_expression: public Accessor_expression
{
	/// Expression to evaluate as member accessor
	Expression m_expression;
	
public:
	/** Creates new member accessors used for record member access
	 * \param expression expression to evaluate as member accessor
	 */
	Member_accessor_expression(Expression expression)
		: m_expression{expression}
	{}
	
	Ref access(Context& ctx, Ref ref) const override
	{
		return ref[m_expression.to_string(ctx)];
	}
	
	std::unique_ptr<Accessor_expression> clone() const override
	{
		return unique_ptr<Accessor_expression> {new Member_accessor_expression{m_expression}};
	}
};

Expression::Impl::Reference_expression::Reference_expression() = default;

Expression::Impl::Reference_expression::Reference_expression(const Reference_expression& other)
	: m_referenced{other.m_referenced}
	, m_fmt_format{other.m_fmt_format}
{
	for (auto&& accessor: other.m_subelements) {
		m_subelements.emplace_back(accessor->clone());
	}
}

Expression::Impl::Reference_expression& Expression::Impl::Reference_expression::operator= (const Reference_expression& other)
{
	m_referenced = other.m_referenced;
	m_fmt_format = other.m_fmt_format;
	for (auto&& accessor: other.m_subelements) {
		m_subelements.emplace_back(accessor->clone());
	}
	return *this;
}

unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::clone() const
{
	return unique_ptr<Reference_expression> {new Reference_expression{*this}};
}

long Expression::Impl::Reference_expression::to_long(Context& ctx) const
{
	try {
		if (Ref_r ref = to_ref(ctx)) {
			return ref.scalar_value<long>();
		}
		throw Right_error{"Unable to grant access for value reference"};
	} catch (const Error& e) {
		throw Error{e.status(), "while referencing `{}': {}", m_referenced, e.what()};
	}
}

double Expression::Impl::Reference_expression::to_double(Context& ctx) const
{
	try {
		if (Ref_r ref = to_ref(ctx)) {
			return ref.scalar_value<double>();
		}
		throw Right_error{"Unable to grant read access for value reference"};
	} catch (const Error& e) {
		throw Error{e.status(), "while referencing `{}': {}", m_referenced, e.what()};
	}
}

std::string Expression::Impl::Reference_expression::to_string(Context& ctx) const
{
	string result;
	Ref_r raw_data = to_ref(ctx);
	if (auto&& referenced_type = dynamic_pointer_cast<const Array_datatype>(raw_data.type())) {
		if (auto&& scal_type = dynamic_pointer_cast<const Scalar_datatype>(referenced_type->subtype())) {
			if (scal_type->datasize() == 1 && (scal_type->kind() == Scalar_kind::SIGNED || scal_type->kind() == Scalar_kind::UNSIGNED)) {
				result = string{static_cast<const char*>(raw_data.get()), referenced_type->size()};
				if (!m_fmt_format.empty()) {
					result = fmt::format("{" + m_fmt_format + "}", result.c_str());
				}
			}
		} else {
			throw Type_error{"Cannot evaluate as string an array of non char elements"};
		}
	} else {
		long lres = to_long(ctx);
		double dres = to_double(ctx);
		if (static_cast<double>(lres) == dres) {
			if (m_fmt_format.empty()) {
				stringstream ss_result;
				ss_result << lres;
				result = ss_result.str();
			} else {
				result = fmt::format("{" + m_fmt_format + "}", lres);
			}
		} else {
			if (m_fmt_format.empty()) {
				stringstream ss_result;
				ss_result << setprecision(17) << dres;
				result = ss_result.str();
			} else {
				result = fmt::format("{" + m_fmt_format + "}", dres);
			}
		}
	}
	
	return result;
}

Ref Expression::Impl::Reference_expression::to_ref(Context& ctx) const
{
	Ref result = ctx.desc(m_referenced.c_str()).ref();
	for (auto&& accessor: m_subelements) {
		result = accessor->access(ctx, result);
	}
	return result;
}

template <class T>
size_t from_ref_cpy(void* buffer, Ref_r ref)
{
	T value = ref.scalar_value<T>();
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Reference_expression::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	if (Ref_r ref_r = to_ref(ctx)) {
		if (m_subelements.empty()) {
			if (ref_r.type()->buffersize() == type->buffersize()) {
				memcpy(buffer, ref_r.get(), type->buffersize());
				return type->buffersize();
			} else {
				throw Value_error{
					"Cannot copy reference expression value: reference buffersize ({}) != type bufferize ({})",
					ref_r.type()->buffersize(),
					    type->buffersize()};
			}
		} else {
			if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
				if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED) {
					switch (scalar_type->buffersize()) {
					case 1L:
						return from_ref_cpy<uint8_t>(buffer, ref_r);
					case 2L:
						return from_ref_cpy<uint16_t>(buffer, ref_r);
					case 4L:
						return from_ref_cpy<uint32_t>(buffer, ref_r);
					case 8L:
						return from_ref_cpy<uint64_t>(buffer, ref_r);
					default:
						throw Type_error{"Unknown size of integer datatype"};
					}
				} else if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
					switch (type->buffersize()) {
					case 1L:
						return from_ref_cpy<int8_t>(buffer, ref_r);
					case 2L:
						return from_ref_cpy<int16_t>(buffer, ref_r);
					case 4L:
						return from_ref_cpy<int32_t>(buffer, ref_r);
					case 8L:
						return from_ref_cpy<int64_t>(buffer, ref_r);
					default:
						break;
					}
				} else if (scalar_type->kind() == PDI::Scalar_kind::FLOAT) {
					switch (type->buffersize()) {
					case 4L: {
						return from_ref_cpy<float>(buffer, ref_r);
					}
					case 8L: {
						return from_ref_cpy<double>(buffer, ref_r);
					}
					default:
						break;
					}
				}
			}
			throw Value_error{"Cannot copy reference expression value: non scalar datatype for indexed reference"};
		}
	} else {
		throw Value_error{"Cannot copy reference expression value: cannot get read access to reference"};
	}
}

unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::parse(char const** val_str)
{
	const char* ref = *val_str;
	unique_ptr<Reference_expression> result{new Reference_expression};
	
	if (*ref != '$') throw Value_error{"Expected '$', got {}", *ref};
	++ref;
	
	bool has_curly_brace = false;
	if (*ref == '{') {
		++ref;
		has_curly_brace = true;
		while (isspace(*ref))
			++ref;
	}
	
	result->m_referenced = parse_id(&ref);
	
	while (isspace(*ref))
		++ref;
		
	while (*ref == '[' || *ref == '.') {
		if (*ref == '[') {
			++ref;
			while (isspace(*ref))
				++ref;
			result->m_subelements.emplace_back(new Index_accessor_expression{Expression{Operation::parse(&ref, 1)}});
			if (*ref != ']') {
				throw Value_error{"Expected ']', found {}", *ref};
			}
			++ref;
		} else { // *ref == '.'
			++ref;
			while (isspace(*ref))
				++ref;
			result->m_subelements.emplace_back(new Member_accessor_expression{Expression{parse_id(&ref)}});
		}
		while (isspace(*ref))
			++ref;
	}
	
	if (*ref == ':') {
		string fmt_format = ref;
		size_t found_end = fmt_format.find_first_of("}");
		if (found_end == string::npos) {
			found_end = fmt_format.length();
		}
		result->m_fmt_format = fmt_format.substr(0, found_end);
		ref += found_end;
	}
	
	if (has_curly_brace) {
		if (*ref != '}') {
			throw Value_error{"Expected '}}', found {}", *ref};
		}
		++ref;
		while (isspace(*ref))
			++ref;
	}
	
	*val_str = ref;
	return result;
}

} // namespace PDI
