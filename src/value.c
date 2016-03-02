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

#include <ctype.h>
#include <stdlib.h>

#include "pdi/state.h"

#include "status.h"
#include "utils.h"

#include "pdi/value.h"

struct PDI_refval_s
{
	PDI_param_t *ref;
	
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

PDI_status_t parse_intval(char **val_str, PDI_value_t *value);

PDI_status_t parse_id(char **val_str, int *id_len)
{
	PDI_status_t status = PDI_OK;
	
	char *id = *val_str;
	
	if (!(
		   (*id>='a' && *id<='z')
		|| (*id>='A' && *id<='Z')
		|| (*id=='_')
	)) {
		handle_err(handle_error(PDI_ERR_VALUE, "Invalid first ID character: %c", *id), err0);
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

PDI_status_t parse_ref(char **val_str, PDI_refval_t *value)
{
	PDI_status_t status = PDI_OK;
	char *ref = *val_str;
	
	if ( *ref != '$' ) handle_err(handle_error(PDI_ERR_VALUE, "Expected '$', got %c", *ref), err0);
	++ref;
	
	int has_curly_brace = 0;
	if ( *ref == '{' ) {
		++ref;
		has_curly_brace = 1;
	}
	
	int refid_len; status = parse_id(&ref, &refid_len); handle_err(status, err0);
	
	value->ref = NULL;
	for ( int met_id=0; met_id<PDI_state.nb_params; ++met_id ) {
		if ( !strncmp(PDI_state.params[met_id].name, ref-refid_len, refid_len) ) {
			value->ref = &PDI_state.params[met_id];
		}
	}
	if ( !value->ref ) {
		handle_err(handle_error(PDI_ERR_VALUE, "Invalid reference %*s", refid_len, ref-refid_len), err0);
	}
	
	while ( isspace(*ref) ) ++ref;
	
	value->idx = NULL;
	value->nb_idx = 0;
	while ( *ref == '[' ) {
		++ref;
		while ( isspace(*ref) ) ++ref;
		++(value->nb_idx);
		value->idx = realloc(value->idx, value->nb_idx*sizeof(PDI_value_t));
		status = parse_intval(&ref, &value->idx[value->nb_idx-1]); handle_err(status, err0);
		if ( *ref != ']' )  {
			handle_err(handle_error(PDI_ERR_VALUE, "Expected ']', found %c", *ref), err0);
		}
		++ref;
		while ( isspace(*ref) ) ++ref;
	}
	
	if ( has_curly_brace ) {
		if ( *ref != '}' ) {
			handle_err(handle_error(PDI_ERR_VALUE, "Expected '}', found %c", *ref), err0);
		}
		++ref;
	}
	
	*val_str = ref;
err0:
	return status;
}

PDI_status_t parse_const(char **val_str, int *value)
{
	PDI_status_t status = PDI_OK;
	char *constval = *val_str;
	
	long val_l = strtol(constval, &constval, 0);
	if ( *val_str == constval ) {
		handle_err(handle_error(PDI_ERR_VALUE, "Expected integer, found %s", constval), err0);
	}
	*value = val_l;
	while ( isspace(*constval) ) ++constval;
	
	*val_str = constval;
err0:
	return status;
}

PDI_status_t parse_term(char **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	char *term = *val_str;
	
	if ( !parse_const(&term, &value->c.constval) ) {
		value->kind = PDI_VAL_CONST;
	} else if ( *term == '(' ) {
		++term;
		while ( isspace(*term) ) ++term;
		status = parse_intval(&term, value); handle_err(status, err0);
		if ( *term != ')' )  handle_err(handle_error(PDI_ERR_VALUE, "Expected ')', found '%c'", *term), err0);;
		++term;
		while ( isspace(*term) ) ++term;
	} else {
		value->c.refval = malloc(sizeof(PDI_refval_t));
		if ( !parse_ref(&term, value->c.refval) ) {
			value->kind = PDI_VAL_REF;
		} else {
			free(value->c.refval);
		}
	}
	
	*val_str = term;
err0:
	return status;
}

PDI_status_t parse_op(char **val_str, int prio, PDI_exprop_t *value)
{
	PDI_status_t status = PDI_OK;
	char *op = *val_str;
	
	switch ( *op ) {
	case PDI_OP_PLUS: case PDI_OP_MINUS: {
		if ( prio != 1 ) {
			handle_err(handle_error(PDI_ERR_VALUE, "Mixing operator priority"), err0);
		}
		*value = *op;
		++op;
	} break;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: {
		if ( prio != 2 ) {
			handle_err(handle_error(PDI_ERR_VALUE, "Mixing operator priority"), err0);
		}
		*value = *op;
		++op;
	} break;
	default:
		handle_err(handle_error(PDI_ERR_VALUE, "Expected operator, found '%c'", *op), err0);
	}
	
	while ( isspace(*op) ) ++op;
	
	*val_str = op;
err0:
	return status;
}

PDI_status_t parse_intval2(char **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	char *exprval = *val_str;
	
	status = parse_term(&exprval, value); handle_err(status, err0);
	PDI_exprval_t *expr = NULL;
	PDI_exprop_t op;
	while ( !parse_op(&exprval, 2, &op) ) {
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
		if ( status = parse_term(&exprval, &expr->values[expr->nb_value-1]) ) {
			free(expr->values);
			free(expr->ops);
			free(expr);
			handle_err(status, err0);
		}
	}
	
	*val_str = exprval;
err0:
	return status;
}

PDI_status_t parse_intval(char **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	char *exprval = *val_str;
	
	status = parse_intval2(&exprval, value); handle_err(status, err0);
	PDI_exprval_t *expr = NULL;
	PDI_exprop_t op;
	while ( !parse_op(&exprval, 1, &op) ) {
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
		if ( status = parse_intval2(&exprval, &expr->values[expr->nb_value-1]) ) {
			free(expr->values);
			free(expr->ops);
			free(expr);
			handle_err(status, err0);
		}
	}
	
	*val_str = exprval;
err0:
	return status;
}

PDI_status_t parse_strval(char **val_str, PDI_value_t *value)
{
	PDI_status_t status = PDI_OK;
	char *str = *val_str;
	
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
				status = parse_term(&str, &value->c.strval->values[value->c.strval->nb_values-1]);
				handle_err(status, err0);
			} break;
			default: { // parse the term starting with the dollar (the ref)
				parse_term(&str, &value->c.strval->values[value->c.strval->nb_values-1]);
				handle_err(status, err0);
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

PDI_status_t eval_strval(PDI_strval_t *val, char **res)
{
	PDI_status_t status = PDI_OK;
	
	//TODO: handle errors
	*res = NULL;
	size_t from_idx = 0;
	size_t res_idx = 0;
	
	for ( int ii=0; ii<val->nb_values; ++ii ) {
		size_t blk_sz = val->value_pos[ii]-from_idx;
		*res = realloc(*res, res_idx+blk_sz+1);
		memcpy((*res)+res_idx, val->str+from_idx, blk_sz);
		from_idx += blk_sz;
		res_idx += blk_sz;
		
		char *val_str; PDI_value_str(&val->values[ii], &val_str);
		blk_sz = strlen(val_str);
		*res = realloc(*res, res_idx+blk_sz+1);
		memcpy((*res)+res_idx, val_str, blk_sz);
		res_idx += blk_sz;
	}
	
	size_t blk_sz = strlen(val->str+from_idx);
	*res = realloc(*res, res_idx+blk_sz+1);
	memcpy((*res)+res_idx, val->str+from_idx, blk_sz);
	from_idx += blk_sz;
	res_idx += blk_sz;
	
	(*res)[res_idx] = 0;
	
err0:
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
	
err0:
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
	
err0:
	return status;
}

// public functions

PDI_status_t PDI_value_parse(char *val_str, PDI_value_t* value)
{
	PDI_status_t status = PDI_OK;
	
	PDI_status_t err = PDI_ERR_VALUE;
	char *parse_val = val_str;
	if ( err || *parse_val ) {
		parse_val = val_str;
		while ( isspace(*parse_val) ) ++parse_val;
		err = parse_intval(&parse_val, value);
		while ( isspace(*parse_val) ) ++parse_val;
	}
	if ( err || *parse_val ) {
		parse_val = val_str;
		err = parse_strval(&parse_val, value);
	}
	
err0:
	return status;
}

PDI_status_t PDI_value_destroy(PDI_value_t* value)
{
	PDI_status_t status = PDI_OK;
	
	if ( value->kind == PDI_VAL_EXPR ) {
		exprval_destroy(value->c.exprval); // ignore portential errors
	}
	
err0:
	return status;
}

PDI_status_t PDI_value_int(PDI_value_t* value, int* res)
{
	PDI_status_t status = PDI_ERR_VALUE;
	
err0:
	return status;
}

PDI_status_t PDI_value_str(PDI_value_t* value, char** res)
{
	PDI_status_t status = PDI_OK;
	int intval; handle_err(PDI_value_int(value, &intval), err0); 
	
	if ( !status ) {
		*res = msprintf("%d", intval);
	} else if ( value->kind == PDI_VAL_STR ) {
		status = eval_strval(value->c.strval, res);
	}
	
err0:
	return status;
}
