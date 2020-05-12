/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <memory>

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

std::unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::clone() const
{
	return std::unique_ptr<Reference_expression> {new Reference_expression{*this}};
}

long Expression::Impl::Reference_expression::to_long(Context& ctx) const
try
{
	if (Ref_r ref = ctx.desc(m_referenced.c_str()).ref()) {
		const Datatype* type = &ref.type();
		long stride = 1;
		long idx = 0;
		for (auto&& ii : m_idx) {
			auto&& array_type = dynamic_cast<const Array_datatype*>(type);
			if (!array_type) throw Error {PDI_ERR_VALUE, "Accessing non-array data with an index"};
			idx += (array_type->start() + ii.to_long(ctx)) * stride;
			stride *= array_type->size();
			type = &array_type->subtype();
		}
		
		auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(type);
		if (!scalar_type) throw Error {PDI_ERR_VALUE, "Expected scalar found invalid type instead"};
		
		if (scalar_type->kind() == Scalar_kind::SIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<const int8_t*>(ref.get())[idx];
			case 2:
				return static_cast<const int16_t*>(ref.get())[idx];
			case 4:
				return static_cast<const int32_t*>(ref.get())[idx];
			case 8:
				return static_cast<const int64_t*>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected int size: {}", static_cast<long>(scalar_type->kind()));
			}
		} else if (scalar_type->kind() == Scalar_kind::UNSIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<const uint8_t*>(ref.get())[idx];
			case 2:
				return static_cast<const uint16_t*>(ref.get())[idx];
			case 4:
				return static_cast<const uint32_t*>(ref.get())[idx];
			case 8:
				return static_cast<const uint64_t*>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected uint size: {}", static_cast<long>(scalar_type->kind()));
			}
		} else if (scalar_type->kind() == Scalar_kind::FLOAT) {
			switch (scalar_type->datasize()) {
			case 4:
				return static_cast<long>(static_cast<const float*>(ref.get())[idx]);
			case 8:
				return static_cast<long>(static_cast<const double*>(ref.get())[idx]);
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected float size: {}", static_cast<long>(scalar_type->kind()));
			}
		}
		throw Error {PDI_ERR_VALUE, "Expected integer scalar"};
	}
	throw Error {PDI_ERR_RIGHT, "Unable to grant access for value reference"};
} catch (const Error& e)
{
	throw Error {e.status(), "while referencing `{}': {}", m_referenced, e.what()};
}


double Expression::Impl::Reference_expression::to_double(Context& ctx) const
try
{
	if (Ref_r ref = ctx.desc(m_referenced.c_str()).ref()) {
		const Datatype* type = &ref.type();
		long stride = 1;
		long idx = 0;
		for (auto&& ii : m_idx) {
			auto&& array_type = dynamic_cast<const Array_datatype*>(type);
			if (!array_type) throw Error {PDI_ERR_VALUE, "Accessing non-array data with an index"};
			idx += (array_type->start() + ii.to_long(ctx)) * stride;
			stride *= array_type->size();
			type = &array_type->subtype();
		}
		
		auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(type);
		if (!scalar_type) throw Error {PDI_ERR_VALUE, "Expected scalar found invalid type instead"};
		
		if (scalar_type->kind() == Scalar_kind::FLOAT) {
			switch (scalar_type->datasize()) {
			case 4:
				return static_cast<const float*>(ref.get())[idx];
			case 8:
				return static_cast<const double*>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected float size: {}", static_cast<long>(scalar_type->kind()));
			}
		} else if (scalar_type->kind() == Scalar_kind::SIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<double>(static_cast<const int8_t*>(ref.get())[idx]);
			case 2:
				return static_cast<double>(static_cast<const int16_t*>(ref.get())[idx]);
			case 4:
				return static_cast<double>(static_cast<const int32_t*>(ref.get())[idx]);
			case 8:
				return static_cast<double>(static_cast<const int64_t*>(ref.get())[idx]);
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected int size: {}", static_cast<long>(scalar_type->kind()));
			}
		} else if (scalar_type->kind() == Scalar_kind::UNSIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<double>(static_cast<const uint8_t*>(ref.get())[idx]);
			case 2:
				return static_cast<double>(static_cast<const uint16_t*>(ref.get())[idx]);
			case 4:
				return static_cast<double>(static_cast<const uint32_t*>(ref.get())[idx]);
			case 8:
				return static_cast<double>(static_cast<const uint64_t*>(ref.get())[idx]);
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected uint size: {}", static_cast<long>(scalar_type->kind()));
			}
		}
		throw Error {PDI_ERR_VALUE, "Expected float scalar"};
	}
	throw Error {PDI_ERR_RIGHT, "Unable to grant access for value reference"};
} catch (const Error& e)
{
	throw Error {e.status(), "while referencing `{}': {}", m_referenced, e.what()};
}

Ref Expression::Impl::Reference_expression::to_ref(Context& ctx) const
{
	if (!m_idx.empty()) {
		try {
			Ref_rw result {
				aligned_alloc(alignof(long), sizeof(long)),
				[](void* v){free(v);},
				std::unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},
				true,
				true
			};
			*static_cast<long*>(result.get()) = to_long(ctx);
			return result;
		} catch (const Error& e) {
			if (e.status() == PDI_ERR_VALUE) {
				Ref_rw result {
					aligned_alloc(alignof(double), sizeof(double)),
					[](void* v){free(v);},
					std::unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}},
					true,
					true
				};
				*static_cast<double*>(result.get()) = to_double(ctx);
				return result;
			} else {
				throw;
			}
		}
	}
	return ctx.desc(m_referenced.c_str()).ref();
}

Ref Expression::Impl::Reference_expression::to_ref(Context& ctx, const Datatype& type) const
{
	Ref_rw result {
		aligned_alloc(type.alignment(), type.buffersize()),
		[](void* v){free(v);},
		type.clone_type(),
		true,
		true
	};
	copy_value(ctx, result.get(), result.type());
	return result;
}

template<class T>
size_t from_long_cpy(void* buffer, long value_long)
{
	T value = static_cast<T>(value_long);
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Reference_expression::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (m_idx.empty()) {
		if (Ref_r reference = ctx.desc(m_referenced.c_str()).ref()) {
			if (reference.type().buffersize() == type.buffersize()) {
				memcpy(buffer, reference.get(), type.buffersize());
				return type.buffersize();
			} else {
				throw Error {PDI_ERR_VALUE,
				    "Cannot copy reference expression value: reference buffersize ({}) != type bufferize ({})",
				    reference.type().buffersize(),
				    type.buffersize()};
			}
		} else {
			throw Error {PDI_ERR_VALUE, "Cannot copy reference expression value: cannot get read access to reference"};
		}
	} else {
		if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
			if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED && type.buffersize() == (long)sizeof(char)) {
				return from_long_cpy<unsigned char>(buffer, to_long(ctx));
			} else if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
				switch (type.buffersize()) {
				case 1L:
					return from_long_cpy<signed char>(buffer, to_long(ctx));
				case 2L:
					return from_long_cpy<short>(buffer, to_long(ctx));
				case 4L:
					return from_long_cpy<int>(buffer, to_long(ctx));
				case 8L:
					return from_long_cpy<long>(buffer, to_long(ctx));
				default:
					break;
				}
			} else if (scalar_type->kind() == PDI::Scalar_kind::FLOAT) {
				switch (type.buffersize()) {
				case 4L: {
					float value = static_cast<float>(to_double(ctx));
					memcpy(buffer, &value, sizeof(float));
					return sizeof(float);
				}
				case 8L: {
					double value = to_double(ctx);
					memcpy(buffer, &value, sizeof(double));
					return sizeof(double);
				}
				default:
					break;
				}
			}
		}
		throw Error {PDI_ERR_VALUE, "Cannot copy reference expression value: non scalar datatype for indexed reference"};
	}
}

std::unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::parse(char const** val_str)
{
	const char* ref = *val_str;
	std::unique_ptr<Reference_expression> result{new Reference_expression};
	
	if (*ref != '$') throw Error {PDI_ERR_VALUE, "Expected '$', got {}", *ref};
	++ref;
	
	bool has_curly_brace = false;
	if (*ref == '{') {
		++ref;
		has_curly_brace = true;
		while (isspace(*ref)) ++ref;
	}
	
	result->m_referenced = parse_id(&ref);
	
	while (isspace(*ref)) ++ref;
	
	while (*ref == '[') {
		++ref;
		while (isspace(*ref)) ++ref;
		result->m_idx.emplace_back(Expression{Operation::parse(&ref, 1)});
		if (*ref != ']')  {
			throw Error {PDI_ERR_VALUE, "Expected ']', found {}", *ref};
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	if (has_curly_brace) {
		if (*ref != '}') {
			throw Error {PDI_ERR_VALUE, "Expected '}}', found {}", *ref};
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	*val_str = ref;
	return result;
}

} // namespace PDI
