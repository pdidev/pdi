/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <iomanip>
#include <memory>
#include <sstream>
#include <string>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "impl.h"
#include "impl/float_literal.h"
#include "impl/int_literal.h"
#include "impl/mapping.h"
#include "impl/operation.h"
#include "impl/reference_expression.h"
#include "impl/sequence.h"
#include "impl/string_literal.h"

namespace PDI {

using std::dynamic_pointer_cast;
using std::setprecision;
using std::string;
using std::stringstream;
using std::unique_ptr;

Expression::Impl::~Impl() = default;

string Expression::Impl::to_string(Context& ctx) const
{
	Ref_r raw_data = to_ref(ctx);
	if (auto&& referenced_type = dynamic_pointer_cast<const Array_datatype>(raw_data.type())) {
		if (auto&& scal_type = dynamic_pointer_cast<const Scalar_datatype>(referenced_type->subtype())) {
			if (scal_type->datasize() == 1 && (scal_type->kind() == Scalar_kind::SIGNED || scal_type->kind() == Scalar_kind::UNSIGNED)) {
				return string{static_cast<const char*>(raw_data.get()), referenced_type->size()};
			}
		}
	}
	long lres = to_long(ctx);
	double dres = to_double(ctx);
	stringstream result;
	if (static_cast<double>(lres) == dres) {
		result << lres;
	} else {
		result << setprecision(17) << dres;
	}
	return result.str();
}

Ref Expression::Impl::to_ref(Context& ctx, Datatype_sptr type) const
{
	// bug on MACOS with aligned_alloc
	// auto data = std::aligned_alloc(type->alignment(), type->buffersize());
	// Ref_rw result{data, [](void* v) { free(v); }, type, true, true};
	// copy_value(ctx, result.get(), type);
	// return result;

	// === option 1: use hand-written version fo aligned_alloc
	// size_t size = type->buffersize() + (type->alignment() - 1);
	// void* buffer = operator new (size);
	// void* data = std::align(type->alignment(), type->buffersize(), buffer, size);
	// Ref_rw result{data, [buffer](void*) { operator delete (buffer); }, type, true, true};
	// copy_value(ctx, result.get(), type);
	// return result;

	// === option 2: use posix_memalign
	// void * data;
	// int err = posix_memalign(&data, type->alignment()*sizeof(void*), type->buffersize());
	// Ref_rw result{data, [](void* v) { free(v); }, type, true, true};
	// copy_value(ctx, result.get(), type);
	// return result;

	// === option 3: use aligned_alloc, falls back to hand-written version if fails
	// auto data = std::aligned_alloc(type->alignment(), type->buffersize());
	// if(data) {
	// 	Ref_rw result{data, [](void* v) { free(v); }, type, true, true};
	// 	copy_value(ctx, result.get(), type);
	// 	return result;
	// }
	// else {
	// 	size_t size = type->buffersize() + (type->alignment() - 1);
	// 	void* buffer = operator new (size);
	// 	data = std::align(type->alignment(), type->buffersize(), buffer, size);
	// 	Ref_rw result{data, [buffer](void*) { operator delete (buffer); }, type, true, true};
	// 	copy_value(ctx, result.get(), type);
	// 	return result;
	// }

	// option 4: use aligned_alloc, falls back to use operator new with align_val_t (C++17)
	auto data = std::aligned_alloc(type->alignment(), type->buffersize());
	if(data) {
		Ref_rw result{data, [](void* v) { free(v); }, type, true, true};
		copy_value(ctx, result.get(), type);
		return result;
	} else {
		auto al = static_cast<std::align_val_t>(type->alignment());
		data = operator new (type->buffersize(), al);
		Ref_rw result{data, [al](void* v) { operator delete (v, al); }, type, true, true};
		copy_value(ctx, result.get(), type);
		return result;
	}
}

unique_ptr<Expression::Impl> Expression::Impl::parse(PC_tree_t value)
{
	if (PDI::is_map(value)) {
		return unique_ptr<Impl>{new Mapping{value}};
	} else if (PDI::is_list(value)) {
		return unique_ptr<Impl>{new Sequence{value}};
	} else {
		return parse(PDI::to_string(value).c_str());
	}
}

unique_ptr<Expression::Impl> Expression::Impl::parse(char const * val_str)
{
	try { // parse as a space enclosed intval
		const char* parse_val = val_str;
		while (isspace(*parse_val))
			++parse_val;
		unique_ptr<Expression::Impl> result = Impl::Operation::parse(&parse_val, 1);
		while (isspace(*parse_val))
			++parse_val;
		if (!*parse_val) return result; // take this if we parsed the whole string, otherwise, parse as a string
	} catch (Error&) {
	}
	// in case of error, parse as a string
	return Impl::String_literal::parse(&val_str);
}

unique_ptr<Expression::Impl> Expression::Impl::parse_term(char const ** val_str)
{
	if (**val_str == '(') {
		const char* term = *val_str;
		++term;
		while (isspace(*term))
			++term;
		unique_ptr<Expression::Impl> result = Operation::parse(&term, 1);
		if (*term != ')') throw Value_error{"Expected ')', found '{}'", *term};
		++term;
		while (isspace(*term))
			++term;
		*val_str = term;
		return result;
	} else if (**val_str == '$') {
		return Reference_expression::parse(val_str);
	} else if (string(*val_str).find(".") != string::npos) {
		return Float_literal::parse(val_str);
	} else {
		return Int_literal::parse(val_str);
	}
}

string Expression::Impl::parse_id(char const ** val_str)
{
	const char* id = *val_str;

	if (!((*id >= 'a' && *id <= 'z') || (*id >= 'A' && *id <= 'Z') || (*id == '_'))) {
		throw Value_error{"Invalid first ID character: {}", *id};
	}
	++id;
	size_t id_len = 1;

	while ((*id >= 'a' && *id <= 'z') || (*id >= 'A' && *id <= 'Z') || (*id >= '0' && *id <= '9') || (*id == '_')) {
		++(id_len);
		++id;
	}

	string result{*val_str, id_len};
	*val_str = id;
	return result;
}

} // namespace PDI
