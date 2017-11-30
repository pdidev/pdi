/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

/**
 * \file value.c
 * \brief Parsing functions
 * \details The data obtained from paraconf consist of arrays of chars.
 * Those string may contain variable name, operations... and are evaluated/parse using the above functions.
 * \author J. Bigot (CEA)
 */

#include <cassert>
#include <cstdint>
#include <vector>

#include <ctype.h>
#include <stdlib.h>

#include "pdi/state.h"
#include "pdi/data_reference.h"

#include "status.h"
#include "utils.h"

#include "pdi/value.h"

namespace PDI {

using std::string;
using std::vector;

/** The binary operators that can be used in values
 */
enum PDI_exprop_t
{
	PDI_OP_PLUS = '+',
	PDI_OP_MINUS = '-',
	PDI_OP_MULT = '*',
	PDI_OP_DIV = '/',
	PDI_OP_MOD = '%',
	PDI_OP_EQUAL = '=',
	PDI_OP_AND = '&',
	PDI_OP_OR = '|',
	PDI_OP_GT = '>',
	PDI_OP_LT = '<'
};

class PDI_refval_t
{
public:
	/// The referenced data
	Data_descriptor &m_referenced;
	
	/// Indexes in case the referenced data is an array
	vector<PDI_value_t> m_idx;
	
};

struct PDI_exprval_s
{
	vector<PDI_value_t> values;
	
	vector<PDI_exprop_t> ops;
	
};

struct PDI_strval_s {
	/// a char string containing the constant part of the str_value
	string str;
	
	struct Subvalue {
		PDI_value_t value;
		/// position in str where to insert the value
		int pos;
	};
	
	/// array of subvalues
	vector<Subvalue> values;
	
};

PDI_status_t parse_intval(char const **val_str, PDI_value_t *value, int level);

PDI_status_t parse_id(char const **val_str, int *id_len)
{
	PDI_status_t status = PDI_OK;
	
	const char *id = *val_str;
	
	if (!(
	        (*id >= 'a' && *id <= 'z')
	        || (*id >= 'A' && *id <= 'Z')
	        || (*id == '_')
	    )) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid first ID character: %c", *id), err0);
	}
	++id;
	*id_len = 1;
	
	while (
	    (*id >= 'a' && *id <= 'z')
	    || (*id >= 'A' && *id <= 'Z')
	    || (*id >= '0' && *id <= '9')
	    || (*id == '_')
	) {
		++(*id_len);
		++id;
	}
	
	*val_str = id;
	
err0:
	return status;
}

PDI_status_t parse_ref(char const **val_str, PDI_refval_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *ref = *val_str;
	
	if (*ref != '$') PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected '$', got %c", *ref), err0);
	++ref;
	
	int has_curly_brace;
	has_curly_brace = 0;
	if (*ref == '{') {
		++ref;
		has_curly_brace = 1;
	}
	
	int refid_len; PDI_handle_err(parse_id(&ref, &refid_len), err0);
	
	assert(PDI_state.desc(string(ref - refid_len, refid_len)).name() == string(ref - refid_len, refid_len));
	
	new (value) PDI_refval_t{PDI_state.desc(string(ref - refid_len, refid_len)), {}};
	
	if (!value->m_referenced.is_metadata()) {
		return PDI_make_err(PDI_ERR_VALUE, "Invalid reference to non-metadata `%s'", value->m_referenced.name().c_str());
	}
	
	assert(!value->m_referenced.name().empty());
	
	while (isspace(*ref)) ++ref;
	
	while (*ref == '[') {
		++ref;
		while (isspace(*ref)) ++ref;
		value->m_idx.push_back(Value());
		PDI_handle_err(parse_intval(&ref, &value->m_idx.back(), 1), err0);
		if (*ref != ']')  {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected ']', found %c", *ref), err0);
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	if (has_curly_brace) {
		if (*ref != '}') {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected '}', found %c", *ref), err0);
		}
		++ref;
	}
	
	*val_str = ref;
err0:
	return status;
}

PDI_status_t parse_const(char const **val_str, long *value)
{
	PDI_status_t status = PDI_OK;
	const char *constval = *val_str;
	
	long val_l = strtol(constval, (char **)&constval, 0);
	if (*val_str == constval) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected integer, found %s", constval), err0);
	}
	*value = val_l;
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
err0:
	return status;
}

PDI_status_t parse_term(char const **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *term = *val_str;
	
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	if (!parse_const(&term, &value->c.constval)) {
		PDI_errhandler(errh);
		value->kind = Value::PDI_VAL_CONST;
	} else if (*term == '(') {
		PDI_errhandler(errh);
		++term;
		while (isspace(*term)) ++term;
		PDI_handle_err(parse_intval(&term, value, 1), err0);
		if (*term != ')')  PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected ')', found '%c'", *term), err0);
		++term;
		while (isspace(*term)) ++term;
	} else {
		PDI_errhandler(errh);
		value->kind = Value::PDI_VAL_REF;
		value->c.refval = static_cast<PDI_refval_t*>(operator new (sizeof(PDI_refval_t)));
		PDI_handle_err(parse_ref(&term, value->c.refval), err1);
		PDI_errhandler(errh);
	}
	
	*val_str = term;
	return status;
	
err1:
	operator delete(value->c.refval);
err0:
	return status;
}

#define OP_LEVELS 6

int op_level(PDI_exprop_t op)
{
	switch (op) {
	case PDI_OP_OR: return 1;
	case PDI_OP_AND: return 2;
	case PDI_OP_EQUAL: return 3;
	case PDI_OP_GT: case PDI_OP_LT: return 4;
	case PDI_OP_PLUS: case PDI_OP_MINUS: return 5;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: return 6;
	}
	return 0;
}

PDI_status_t parse_op(char const **val_str, int level, PDI_exprop_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *c_op = *val_str;
	PDI_exprop_t op = (PDI_exprop_t)(*c_op);
	
	int found_level = op_level(op);
	if (found_level == 0) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected operator, found '%c'", *c_op), err0);
	}
	if (found_level != level) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Mixing operator priority"), err0);
	}
	*value = op;
	++c_op;
	
	while (isspace(*c_op)) ++c_op;
	
	*val_str = c_op;
err0:
	return status;
}

PDI_status_t parse_intval(char const **val_str, PDI_value_t *value, int level)
{
	PDI_status_t status = PDI_OK;
	const char *exprval = *val_str;
	
	{
		if (level >= OP_LEVELS) {
			if (parse_term(&exprval, value) ) return PDI_ERR_IMPL;
		} else {
			if (parse_intval(&exprval, value, level + 1)) return PDI_ERR_IMPL;
		}
	}
	PDI_exprval_t *expr = NULL;
	PDI_exprop_t op;
	
	PDI_errhandler_t errh;
	errh = PDI_errhandler(PDI_NULL_HANDLER);
	while (!parse_op(&exprval, level, &op)) {
		PDI_errhandler(errh);
		if (!expr) {
			expr = new PDI_exprval_t;
			expr->values.push_back(std::move(*value));
			value->kind = Value::PDI_VAL_EXPR;
			value->c.exprval = expr;
		}
		expr->ops.push_back(op);
		expr->values.push_back(Value());
		
		if (level >= OP_LEVELS) {
			PDI_handle_err(parse_term(&exprval, &expr->values.back()), err1);
		} else {
			PDI_handle_err(parse_intval(&exprval, &expr->values.back(), level + 1), err1);
		}
		errh = PDI_errhandler(PDI_NULL_HANDLER);
	}
	PDI_errhandler(errh);
	
	while (isspace(*exprval)) ++exprval;
	
	*val_str = exprval;
	return status;
	
err1:
	delete expr;
	return status;
}

PDI_status_t parse_strval(char const **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *str = *val_str;
	
	value->kind = Value::PDI_VAL_STR;
	value->c.strval = new PDI_strval_t;
	
	while (*str) {
		int sz = 0;
		while (str[sz] != '\\' && str[sz] != '$' && str[sz]) ++sz;
		value->c.strval->str.append(str, sz);
		str += sz;
		switch (*str) {
		case '\\': {
			++str;
			value->c.strval->str.append(str, 1);
			str += 1;
		} break;
		case '$': {
			value->c.strval->values.push_back({Value(), static_cast<int>(value->c.strval->str.size())});
			switch (str[1]) {
			case '(': {
				++str; // parse the term starting with the parenthesis (the intvl)
				PDI_handle_err(parse_term(&str, &value->c.strval->values.back().value), err0);
			} break;
			default: { // parse the term starting with the dollar (the ref)
				PDI_handle_err(parse_term(&str, &value->c.strval->values.back().value), err0);
			} break;
			}
		} break;
		}
	}
	
	*val_str = str;
	return status;
err0:
	delete value->c.strval;
	return status;
}

PDI_status_t eval_refval(PDI_refval_t *val, long *res)
{
	PDI_status_t status = PDI_OK;
	
	const PDI_datatype_t &ref_type = val->m_referenced.get_type();
	PDI_scalar_type_t type = ref_type.c.scalar;
	Data_ref cref;
	
	if (ref_type.kind == PDI_K_ARRAY) {
		if (val->m_idx.size() != static_cast<size_t>(ref_type.c.array->ndims)) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, %d expected", val->m_idx.size(), ref_type.c.array->ndims), err0);
		}
		if (ref_type.c.array->type.kind != PDI_K_SCALAR) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid type accessed"), err0);
		}
		type = ref_type.c.array->type.c.scalar;
	} else if (ref_type.kind == PDI_K_SCALAR) {
		if (!val->m_idx.empty()) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, 0 expected", val->m_idx.size()), err0);
		}
	} else {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid access to a struct"), err0);
	}
	
	long idx; idx = 0;
	long stride; stride = 1;
	for (size_t ii = 0; ii < val->m_idx.size(); ++ii) {
		long start; PDI_handle_err(PDI_value_int(&(ref_type.c.array->starts[ii]), &start), err0);
		long index; PDI_handle_err(PDI_value_int(&val->m_idx[ii], &index), err0);
		idx += (start + index) * stride;
		long size; PDI_handle_err(PDI_value_int(&(ref_type.c.array->sizes[ii]), &size), err0);
		stride *= size;
	}
	
	if (!val->m_referenced.value()) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Referenced variable `%s' is not shared", val->m_referenced.name().c_str()), err0);
	}
	
	if (Data_r_ref ref = val->m_referenced.value()) {
		void *value = ref.get();
		switch (type) {
		case PDI_T_INT8: {
			*res = ((int8_t *)value)[idx];
		} break;
		case PDI_T_INT16: {
			*res = ((int16_t *)value)[idx];
		} break;
		case PDI_T_INT32: {
			*res = ((int32_t *)value)[idx];
		} break;
		case PDI_T_INT64: {
			*res = ((int64_t *)value)[idx];
		} break;
		default: {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Non-integer type accessed"), err0);
		} break;
		}
	} else {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Referenced variable `%s' is not readable", val->m_referenced.name().c_str()), err0);
	}
	
	
	return status;
	
err0:
	return status;
}

PDI_status_t eval_exprval(PDI_exprval_t *val, long *res)
{
	PDI_status_t status = PDI_OK;
	
	long computed_value; PDI_value_int(&val->values[0], &computed_value);
	for (size_t ii = 1; ii < val->values.size(); ++ii) {
		long operand; PDI_value_int(&val->values[ii], &operand);
		switch (val->ops[ii - 1]) {
		case PDI_OP_PLUS: {
			computed_value += operand;
		} break;
		case PDI_OP_MINUS: {
			computed_value -= operand;
		} break;
		case PDI_OP_MULT: {
			computed_value *= operand;
		} break;
		case PDI_OP_DIV: {
			computed_value /= operand;
		} break;
		case PDI_OP_MOD: {
			computed_value %= operand;
		} break;
		case PDI_OP_EQUAL: {
			computed_value = (computed_value == operand);
		} break;
		case PDI_OP_AND: {
			computed_value = computed_value && operand;
		} break;
		case PDI_OP_OR: {
			computed_value = computed_value || operand;
		} break;
		case PDI_OP_GT: {
			computed_value = (computed_value > operand);
		} break;
		case PDI_OP_LT: {
			computed_value = (computed_value < operand);
		} break;
		default: {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Unknown operator: `%c'", val->ops[ii - 1]), err0);
		}
		}
	}
	
	*res = computed_value;
	return status;
	
err0:
	return status;
}

PDI_status_t eval_strval(PDI_strval_t *val, char **res)
{
	PDI_status_t status = PDI_OK;
	
	*res = NULL;
	size_t from_idx = 0;
	size_t res_idx = 0;
	
	char *build_str = NULL;
	for (size_t ii = 0; ii < val->values.size(); ++ii) {
		size_t blk_sz = val->values[ii].pos - from_idx;
		build_str = (char *) realloc(build_str, res_idx + blk_sz + 1);
		memcpy(build_str + res_idx, val->str.c_str() + from_idx, blk_sz);
		from_idx += blk_sz;
		res_idx += blk_sz;
		
		char *val_str; PDI_handle_err(PDI_value_str(&val->values[ii].value, &val_str), err1);
		blk_sz = strlen(val_str);
		build_str = (char *) realloc(build_str, res_idx + blk_sz + 1);
		memcpy(build_str + res_idx, val_str, blk_sz);
		res_idx += blk_sz;
err1:
		free(val_str);
		PDI_handle_err(status, err0);
	}
	
	size_t blk_sz; blk_sz = val->str.size()-from_idx;
	build_str = (char *) realloc(build_str, res_idx + blk_sz + 1);
	memcpy(build_str + res_idx, val->str.c_str() + from_idx, blk_sz);
	from_idx += blk_sz;
	res_idx += blk_sz;
	
	build_str[res_idx] = 0;
	
	*res = build_str;
	return status;
	
err0:
	free(build_str);
	return status;
}


PDI_status_t strval_copy(PDI_strval_t *value, PDI_strval_t *copy)
{
	copy->str = value->str;
	copy->values = value->values;
	return PDI_OK;
}

PDI_status_t exprval_copy(PDI_exprval_t *value, PDI_exprval_t *copy)
{
	copy->ops = value->ops;
	copy->values = value->values;
	return PDI_OK;
};

PDI_status_t refval_copy(PDI_refval_t *value, PDI_refval_t *copy)
{
	new (copy) PDI_refval_t{value->m_referenced, {}};
	
	for (auto &&idx : value->m_idx) {
		copy->m_idx.push_back(Value());
		idx = value->m_idx.back();
	}
	
	return PDI_OK;
};

// public functions

Value::Value(const char *val_str)
{
	PDI_status_t err = PDI_ERR_VALUE;
	const char *parse_val = val_str;
	
	if (err || *parse_val) {
		parse_val = val_str;
		/// Remove leading space
		while (isspace(*parse_val)) ++parse_val;
		PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
		/// Try to parse as if it was an intval (const integer or expression)
		err = parse_intval(&parse_val, this, 1);
		PDI_errhandler(errh);
		/// Goes to '\0' if the remaining characters are spaces
		while (isspace(*parse_val)) ++parse_val;
	}
	// In case they are not spaces (something remains), we do not have an intval
	if (!err && *parse_val) this->~Value();
	
	/// Try to parse as a strval
	if (err || *parse_val) {
		parse_val = val_str;
		parse_strval(&parse_val, this);
	}
}

PDI_status_t PDI_value_int(const PDI_value_t *value, long *res)
{
	PDI_status_t status = PDI_OK;
	
	switch (value->kind) {
	case Value::PDI_VAL_CONST: {
		*res = value->c.constval;
	} break;
	case Value::PDI_VAL_REF: {
		PDI_handle_err(eval_refval(value->c.refval, res), err0);
	} break;
	case Value::PDI_VAL_EXPR: {
		PDI_handle_err(eval_exprval(value->c.exprval, res), err0);
	} break;
	default: {
		char *strval = NULL; PDI_handle_err(PDI_value_str(value, &strval), err0);
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Non integer value type: %s", strval), err0);
	}
	}
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_value_str(const PDI_value_t *value, char **res)
{
	PDI_status_t status = PDI_OK;
	
	if (value->kind == Value::PDI_VAL_STR) {
		PDI_handle_err(eval_strval(value->c.strval, res), err0);
	} else {
		long intval; PDI_handle_err(PDI_value_int(value, &intval), err0);
		*res = msprintf("%ld", intval);
	}
	
err0:
	return status;
}

Value::Value():
		kind(PDI_VAL_CONST)
{
	c.constval = 0;
}

Value::Value(const Value& origin):
		kind(origin.kind)
{
	switch (origin.kind) {
	case PDI_VAL_CONST:
		c.constval = origin.c.constval;
		break;
	case PDI_VAL_REF:
		c.refval = static_cast<PDI_refval_t*>(operator new (sizeof(PDI_refval_t)));
		refval_copy(origin.c.refval, c.refval);
		break;
	case PDI_VAL_EXPR:
		c.exprval = new PDI_exprval_t;
		exprval_copy(origin.c.exprval, c.exprval);
		break;
	case PDI_VAL_STR:
		c.strval = new PDI_strval_t;
		strval_copy(origin.c.strval, c.strval);
		break;
	}
}

Value::Value(Value&& origin):
		kind(origin.kind),
		c(origin.c)
{
	origin.kind = PDI_VAL_CONST;
}

Value& Value::operator=(const Value& origin)
{
	kind = origin.kind;
	switch (origin.kind) {
	case PDI_VAL_CONST:
		c.constval = origin.c.constval;
		break;
	case PDI_VAL_REF:
		c.refval = static_cast<PDI_refval_t*>(operator new (sizeof(PDI_refval_t)));
		refval_copy(origin.c.refval, c.refval);
		break;
	case PDI_VAL_EXPR:
		c.exprval = new PDI_exprval_t;
		exprval_copy(origin.c.exprval, c.exprval);
		break;
	case PDI_VAL_STR:
		c.strval = new PDI_strval_t;
		strval_copy(origin.c.strval, c.strval);
		break;
	}
	return *this;
}

Value& Value::operator=(Value&& origin)
{
	kind = origin.kind;
	c = origin.c;
	origin.kind = PDI_VAL_CONST;
	return *this;
}

Value::~Value()
{
	switch (kind) {
	case PDI_VAL_EXPR: {
		delete c.exprval;
	} break;
	case PDI_VAL_REF: {
		delete c.refval;
	} break;
	case PDI_VAL_STR: {
		delete c.strval;
	} break;
	case PDI_VAL_CONST: break;
	}
}

} // namespace PDI
