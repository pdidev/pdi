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
  
//The following is used for doxygen documentation:
 /**
 * \file value.c
 * \brief Parsing functions
 * \details The data obtained from paraconf consist of arrays of chars.
 * Those string may contain variable name, operations... and are evaluated/parse using the above functions.
 * \author J. Bigot (CEA)
 */

#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>

#include "pdi/state.h"

#include "status.h"
#include "utils.h"

#include "pdi/value.h"

struct PDI_refval_s
{
	PDI_data_t *ref;
	
	PDI_value_t *idx;
	
	int nb_idx;
	
};

struct PDI_exprval_s
{
	int nb_value;
	
	PDI_value_t *values;
	
	PDI_exprop_t *ops;
	
};

struct PDI_strval_s
{
	/// a char string containing the constant part of the str_value
	char *str;
	
	int nb_values;
	
	/// array of nb_values values
	PDI_value_t *values;
	
	/// array of nb_values positions in str where to insert the values
	int *value_pos;
	
};

PDI_status_t parse_intval(char const**val_str, PDI_value_t* value, int level);

PDI_status_t exprval_destroy(PDI_exprval_t* value);

PDI_status_t parse_id(char const **val_str, int *id_len)
{
	PDI_status_t status = PDI_OK;
	
	const char *id = *val_str;
	
	if (!(
		   (*id>='a' && *id<='z')
		|| (*id>='A' && *id<='Z')
		|| (*id=='_')
	)) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid first ID character: %c", *id), err0);
	}
	++id;
	*id_len=1;
	
	while (
		   (*id>='a' && *id<='z')
		|| (*id>='A' && *id<='Z')
		|| (*id>='0' && *id<='9')
		|| (*id=='_')
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
	
	if ( *ref != '$' ) PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected '$', got %c", *ref), err0);
	++ref;
	
	int has_curly_brace = 0;
	if ( *ref == '{' ) {
		++ref;
		has_curly_brace = 1;
	}
	
	int refid_len; PDI_handle_err(parse_id(&ref, &refid_len), err0);
	
	value->ref = NULL;
	for ( int met_id=0; met_id<PDI_state.nb_data; ++met_id ) {
		if ( !strncmp(PDI_state.data[met_id].name, ref-refid_len, refid_len) ) {
			value->ref = &PDI_state.data[met_id];
			break;
		}
	}
	if ( !value->ref ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid reference %*s", refid_len, ref-refid_len), err0);
	}
	
	while ( isspace(*ref) ) ++ref;
	
	value->idx = NULL;
	value->nb_idx = 0;
	while ( *ref == '[' ) {
		++ref;
		while ( isspace(*ref) ) ++ref;
		++(value->nb_idx);
		value->idx = realloc(value->idx, value->nb_idx*sizeof(PDI_value_t));
		PDI_handle_err(parse_intval(&ref, &value->idx[value->nb_idx-1], 1), err0);
		if ( *ref != ']' )  {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected ']', found %c", *ref), err0);
		}
		++ref;
		while ( isspace(*ref) ) ++ref;
	}
	
	if ( has_curly_brace ) {
		if ( *ref != '}' ) {
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
	
	long val_l = strtol(constval, (char**)&constval, 0);
	if ( *val_str == constval ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected integer, found %s", constval), err0);
	}
	*value = val_l;
	while ( isspace(*constval) ) ++constval;
	
	*val_str = constval;
err0:
	return status;
}

PDI_status_t parse_term(char const **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *term = *val_str;

	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	if ( !parse_const(&term, &value->c.constval) ) {
		PDI_errhandler(errh);
		value->kind = PDI_VAL_CONST;
	} else if ( *term == '(' ) {
		PDI_errhandler(errh);
		++term;
		while ( isspace(*term) ) ++term;
		PDI_handle_err(parse_intval(&term, value, 1), err0);
		if ( *term != ')' )  PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected ')', found '%c'", *term), err0);;
		++term;
		while ( isspace(*term) ) ++term;
	} else {
		value->c.refval = malloc(sizeof(PDI_refval_t));
		if ( !parse_ref(&term, value->c.refval) ) {
			value->kind = PDI_VAL_REF;
		} else {
			free(value->c.refval);
			PDI_errhandler(errh);
			PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Invalid ref: `%s'", *val_str), err0);
		}
		PDI_errhandler(errh);
	}
	
	*val_str = term;
err0:
	return status;
}

#define OP_LEVELS 5

int op_level(PDI_exprop_t op)
{
	switch(op) {
	case PDI_OP_OR: return 1;
	case PDI_OP_AND: return 2;
	case PDI_OP_EQUAL: return 3;
	case PDI_OP_PLUS: case PDI_OP_MINUS: return 4;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: return 5;
	}
	return 0;
}

PDI_status_t parse_op(char const **val_str, int level, PDI_exprop_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *op = *val_str;
	
	int found_level = op_level(*op);
	if ( found_level == 0 ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Expected operator, found '%c'", *op), err0);
	}
	if ( found_level != level ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Mixing operator priority"), err0);
	}
	*value = *op;
	++op;
	
	while ( isspace(*op) ) ++op;
	
	*val_str = op;
err0:
	return status;
}

PDI_status_t parse_intval(char const **val_str, PDI_value_t *value, int level)
{
	PDI_status_t status = PDI_OK;
	const char *exprval = *val_str;
	
	if ( level >= OP_LEVELS ) {
		PDI_handle_err(parse_term(&exprval, value), err0);
	} else {
		PDI_handle_err(parse_intval(&exprval, value, level+1), err0);
	}
	PDI_exprval_t *expr = NULL;
	PDI_exprop_t op;
	
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	while ( !parse_op(&exprval, level, &op) ) {
		PDI_errhandler(errh);
		if ( !expr ) {
			expr = malloc(sizeof(PDI_exprval_t));
			expr->nb_value = 1;
			expr->values = malloc(sizeof(PDI_value_t));
			expr->ops = NULL;
			expr->values[0] = *value;
			value->kind = PDI_VAL_EXPR;
			value->c.exprval = expr;
		}
		++expr->nb_value;
		expr->ops = realloc(expr->ops, (expr->nb_value-1)*sizeof(PDI_exprop_t));
		expr->ops[expr->nb_value-2] = op;
		expr->values = realloc(expr->values, expr->nb_value*sizeof(PDI_value_t));
		
		if ( level >= OP_LEVELS ) {
			PDI_handle_err(parse_term(&exprval, &expr->values[expr->nb_value-1]), err1);
		} else {
			PDI_handle_err(parse_intval(&exprval, &expr->values[expr->nb_value-1], level+1), err1);
		}
		errh = PDI_errhandler(PDI_NULL_HANDLER);
	}
	PDI_errhandler(errh);
	
	while ( isspace(*exprval) ) ++exprval;
	
	*val_str = exprval;
	return status;
	
err1:
	exprval_destroy(expr);
	free(expr);
err0:
	return status;
}

PDI_status_t parse_strval(char const **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	const char *str = *val_str;
	
	value->kind = PDI_VAL_STR;
	value->c.strval = malloc(sizeof(PDI_strval_t));
	size_t str_size = 0;
	value->c.strval->str = malloc(str_size+1);
	value->c.strval->str[str_size] = 0;
	value->c.strval->nb_values = 0;
	value->c.strval->values = NULL;
	value->c.strval->value_pos = NULL;
	
	while ( *str ) {
		int sz = 0;
		while (str[sz] != '\\' && str[sz] != '$' && str[sz] ) ++sz;
		value->c.strval->str = mstrcat(value->c.strval->str, str_size, str, sz);
		str_size += sz;
		str += sz;
		switch ( *str ) {
		case '\\': {
			++str;
			value->c.strval->str = mstrcat(value->c.strval->str, str_size, str, 1);
			str_size += 1;
			str += 1;
		} break;
		case '$': {
			++value->c.strval->nb_values;
			value->c.strval->values = realloc(
					value->c.strval->values,
					value->c.strval->nb_values*sizeof(PDI_value_t)
				);
			value->c.strval->value_pos = realloc(
					value->c.strval->value_pos,
					value->c.strval->nb_values*sizeof(int)
				);
			value->c.strval->value_pos[value->c.strval->nb_values-1] = str_size;
			switch ( str[1] ) {
			case '(': {
				++str; // parse the term starting with the parenthesis (the intvl)
				PDI_handle_err(parse_term(&str, &value->c.strval->values[value->c.strval->nb_values-1]), err0);
			} break;
			default: { // parse the term starting with the dollar (the ref)
				PDI_handle_err(parse_term(&str, &value->c.strval->values[value->c.strval->nb_values-1]), err0);
			} break;
			}
		} break;
		}
	}
	
	*val_str = str;
	return status;
err0:
	// don't free the last one that's responsible for the error
	for ( int ii=0; ii<value->c.strval->nb_values-1; ++ii ) {
		//TODO: free value->c.strval->values[ii]
	}
	free(value->c.strval->value_pos);
	free(value->c.strval->values);
	free(value->c.strval->str);
	free(value->c.strval);
	return status;
}

PDI_status_t eval_refval(PDI_refval_t *val, long *res)
{
	PDI_status_t status = PDI_OK;
	
	PDI_scalar_type_t type = val->ref->type.c.scalar;
	
	if ( val->ref->type.kind == PDI_K_ARRAY ) {
		if ( val->nb_idx != val->ref->type.c.array->ndims ) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, %d expected", val->nb_idx, val->ref->type.c.array->ndims), err0);
		}
		if ( val->ref->type.c.array->type.kind != PDI_K_SCALAR ) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid type accessed"), err0);
		}
		type = val->ref->type.c.array->type.c.scalar;
	} else if ( val->ref->type.kind == PDI_K_SCALAR ) {
		if ( val->nb_idx ) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, 0 expected", val->nb_idx), err0);
		}
	} else {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid access to a struct"), err0);
	}
	
	long idx = 0;
	long stride = 1;
	for ( int ii=0; ii<val->nb_idx; ++ii ) {
		long start; PDI_handle_err(PDI_value_int(&val->ref->type.c.array->starts[ii], &start), err0);
		long index; PDI_handle_err(PDI_value_int(&val->idx[ii], &index), err0);
		idx += (start+index) * stride;
		long size; PDI_handle_err(PDI_value_int(&val->ref->type.c.array->sizes[ii], &size), err0);
		stride *= size;
	}
	
	if ( val->ref->nb_content < 1 ) PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Referenced variable `%s' has no value set", val->ref->name), err0);
	void *value = val->ref->content[0].data;
	
	switch ( type ) {
	case PDI_T_INT8: {
		*res = ((int8_t*)value)[idx];
	} break;
	case PDI_T_INT16: {
		*res = ((int16_t*)value)[idx];
	} break;
	case PDI_T_INT32: {
		*res = ((int32_t*)value)[idx];
	} break;
	case PDI_T_INT64: {
		*res = ((int64_t*)value)[idx];
	} break;
	default: {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Non-integer type accessed"), err0);
	} break;
	}

	return status;
	
err0:
	return status;
}

PDI_status_t eval_exprval(PDI_exprval_t *val, long *res)
{
	PDI_status_t status = PDI_OK;
	
	long computed_value; PDI_value_int(&val->values[0], &computed_value);
	for ( int ii=1; ii<val->nb_value; ++ii ) {
		long operand; PDI_value_int(&val->values[ii], &operand);
		switch ( val->ops[ii-1] ) {
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
		default: {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Unknown operator: `%c'", val->ops[ii-1]), err0);
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
	for ( int ii=0; ii<val->nb_values; ++ii ) {
		size_t blk_sz = val->value_pos[ii]-from_idx;
		build_str = realloc(build_str, res_idx+blk_sz+1);
		memcpy(build_str+res_idx, val->str+from_idx, blk_sz);
		from_idx += blk_sz;
		res_idx += blk_sz;
		
		char *val_str; PDI_handle_err(PDI_value_str(&val->values[ii], &val_str), err1);
		blk_sz = strlen(val_str);
		build_str = realloc(build_str, res_idx+blk_sz+1);
		memcpy(build_str+res_idx, val_str, blk_sz);
		res_idx += blk_sz;
err1:
		free(val_str);
		PDI_handle_err(status, err0);
	}
	
	size_t blk_sz = strlen(val->str+from_idx);
	build_str = realloc(build_str, res_idx+blk_sz+1);
	memcpy(build_str+res_idx, val->str+from_idx, blk_sz);
	from_idx += blk_sz;
	res_idx += blk_sz;
	
	build_str[res_idx] = 0;
	
	*res = build_str;
	return status;
	
err0:
	free(build_str);
	return status;
}

PDI_status_t strval_destroy(PDI_strval_t* value)
{
	PDI_status_t status = PDI_OK;
	
	for ( int ii=0; ii<value->nb_values; ++ii ) {
		PDI_value_destroy(&value->values[ii]);
	}
	free(value->value_pos);
	free(value->values);
	free(value->str);
	
	return status;
}

PDI_status_t exprval_destroy(PDI_exprval_t* value)
{
	PDI_status_t status = PDI_OK;
	
	int ii;
	for (ii=0; ii<value->nb_value; ++ii) {
		PDI_value_destroy(&value->values[ii]); // ignore potential errors
	}
	free(value->values);
	free(value->ops);
	
	return status;
}

PDI_status_t refval_destroy(PDI_refval_t* value)
{
	PDI_status_t status = PDI_OK;
	
	int ii;
	for (ii=0; ii<value->nb_idx; ++ii) {
		PDI_value_destroy(&value->idx[ii]); // ignore potential errors
	}
	free(value->idx);
	
	return status;
}

// public functions

PDI_status_t PDI_value_parse(const char* val_str, PDI_value_t* value)
{
	PDI_status_t status = PDI_OK;
	
	PDI_status_t err = PDI_ERR_VALUE;
	const char *parse_val = val_str;
	if ( err || *parse_val ) {
		parse_val = val_str;
		while ( isspace(*parse_val) ) ++parse_val;
		PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
		err = parse_intval(&parse_val, value, 1);
		PDI_errhandler(errh);
		while ( isspace(*parse_val) ) ++parse_val;
	}
	if ( err || *parse_val ) {
		parse_val = val_str;
		status = parse_strval(&parse_val, value);
	}
	
	return status;
}

PDI_status_t PDI_value_destroy(PDI_value_t* value)
{
	PDI_status_t status = PDI_OK;
	
	switch ( value->kind ) {
	case PDI_VAL_EXPR: {
		exprval_destroy(value->c.exprval); // ignore portential errors
		free(value->c.exprval);
	} break;
	case PDI_VAL_REF: {
		refval_destroy(value->c.refval);
		free(value->c.refval);
	} break;
	case PDI_VAL_STR: {
		strval_destroy(value->c.strval);
		free(value->c.strval);
	} break;
	case PDI_VAL_CONST: break;
	}
	
	return status;
}

PDI_status_t PDI_value_int(const PDI_value_t *value, long *res)
{
	PDI_status_t status = PDI_OK;
	
	switch ( value->kind ) {
	case PDI_VAL_CONST: {
		*res = value->c.constval;
	} break;
	case PDI_VAL_REF: {
		PDI_handle_err(eval_refval(value->c.refval, res), err0);
	} break;
	case PDI_VAL_EXPR: {
		PDI_handle_err(eval_exprval(value->c.exprval, res), err0);
	} break;
	default: {
		char *strval; PDI_value_str(value, &strval);
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Non integer value type: %s", strval), err0);
	}}
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_value_str(const PDI_value_t* value, char** res)
{
	PDI_status_t status = PDI_OK;
	
	if ( value->kind == PDI_VAL_STR ) {
		PDI_handle_err(eval_strval(value->c.strval, res), err0);
	} else {
		long intval; PDI_handle_err(PDI_value_int(value, &intval), err0);
		*res = msprintf("%ld", intval);
	}
	
err0:
	return status;
}
