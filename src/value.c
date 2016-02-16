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
	PDI_value_t **values;
	
	char *str;
	
};

PDI_status_t parse_intval(char **val_str, PDI_value_t *value);

PDI_status_t parse_id(char **val_str, int *id_len)
{
	char *id = *val_str;
	
	if (!(
		   (*id>='a' && *id<='z')
		|| (*id>='A' && *id<='Z')
		|| (*id=='_')
	)) {
		return PDI_ERR_VALUE;
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
	
	*val_str = id+*id_len;
	return PDI_OK;
}

PDI_status_t parse_ref(char **val_str, PDI_refval_t *value)
{
	PDI_status_t res = PDI_OK;
	
	char *ref = *val_str;
	if ( *ref != '$' ) return PDI_ERR_VALUE;
	++ref;
	
	int has_curly_brace = 0;
	if ( *ref == '{' ) {
		++ref;
		has_curly_brace = 1;
	}
	
	int refid_len; res = parse_id(&ref, &refid_len); if (res) return res;
	
	for ( int met_id=0; met_id<PDI_state.nb_params; ++met_id ) {
		if ( !strncmp(PDI_state.params[met_id].name, ref-refid_len, refid_len) ) {
			value->ref = &PDI_state.params[met_id];
			*val_str = ref+refid_len;
			while ( isspace(**val_str) ) ++*val_str;
			return PDI_OK;
		}
	}
	
	value->idx = NULL;
	value->nb_idx = 0;
	while ( *ref == '[' ) {
		++ref;
		while ( isspace(*ref) ) ++ref;
		++(value->nb_idx);
		value->idx = realloc(value->idx, value->nb_idx*sizeof(PDI_value_t));
		res = parse_intval(&ref, &value->idx[value->nb_idx-1]); if (res) return res;
		if ( *ref != ']' )  return PDI_ERR_VALUE;
		++ref;
		while ( isspace(*ref) ) ++ref;
	}
	
	if ( has_curly_brace ) {
		if ( *ref != '}' ) return PDI_ERR_VALUE;
		++ref;
	}
	
	*val_str = ref;
	return PDI_OK;
}

PDI_status_t parse_const(char **val_str, int *value)
{
	char *constval = *val_str;
// 	if ( *constval < '1' || *constval > '9' ) return PDI_ERR_VALUE;
	long val_l = strtol(constval, &constval, 0);
	if ( *val_str == constval ) return PDI_ERR_VALUE;
	*value = val_l;
	while ( isspace(*constval) ) ++constval;
	*val_str = constval; return PDI_OK;
}

PDI_status_t parse_term(char **val_str, PDI_value_t *value)
{
	PDI_status_t res = PDI_ERR_VALUE;
	char *term = *val_str;
	
	if ( !parse_const(&term, &value->c.constval) ) {
		value->kind = PDI_VAL_CONST;
		res = PDI_OK;
	} else if ( *term == '(' ) {
		++term;
		while ( isspace(*term) ) ++term;
		res = parse_intval(&term, value); if (res) goto err0;
		if ( *term != ')' )  res = PDI_ERR_VALUE; if (res) goto err0;
		++term;
		while ( isspace(*term) ) ++term;
	} else {
		value->c.refval = malloc(sizeof(PDI_refval_t));
		if ( !parse_ref(&term, value->c.refval) ) {
			value->kind = PDI_VAL_REF;
			res = PDI_OK;
		} else {
			free(value->c.refval);
		}
	}
	*val_str = term;
	
err0:
	return res;
}

PDI_status_t parse_op(char **val_str, int prio, PDI_exprop_t *value)
{
	switch ( **val_str ) {
	case PDI_OP_PLUS: case PDI_OP_MINUS: {
		if ( prio != 1 ) return PDI_ERR_VALUE;
	} break;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: {
		if ( prio != 2 ) return PDI_ERR_VALUE;
	} break;
	default:
		return PDI_ERR_VALUE;
	}
	++ *val_str;
	while ( isspace(**val_str) ) ++*val_str;
	return PDI_OK;
}

PDI_status_t parse_intval2(char **val_str, PDI_value_t *value)
{
	char *exprval = *val_str;
	if ( parse_term(&exprval, value) ) return PDI_ERR_VALUE;
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
		if ( parse_term(&exprval, &expr->values[expr->nb_value-1]) ) {
			free(expr->values);
			free(expr->ops);
			free(expr);
			return PDI_ERR_VALUE;
		}
	}
	*val_str = exprval; return PDI_OK;
}

PDI_status_t parse_intval(char **val_str, PDI_value_t *value)
{
	char *exprval = *val_str;
	if ( parse_intval2(&exprval, value) ) return PDI_ERR_VALUE;
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
		if ( parse_intval2(&exprval, &expr->values[expr->nb_value-1]) ) {
			free(expr->values);
			free(expr->ops);
			free(expr);
			return PDI_ERR_VALUE;
		}
	}
	*val_str = exprval; return PDI_OK;
}

PDI_status_t parse_strval(char **val_str, PDI_value_t *value)
{
	//TODO: implement
	abort();
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

PDI_status_t PDI_value_parse(char *val_str, PDI_value_t* value)
{
	PDI_status_t err = PDI_ERR_VALUE;
	char *parse_val = val_str;
	if ( err || *parse_val ) {
		parse_val = val_str;
		while ( isspace(*parse_val) ) ++parse_val;
		PDI_status_t err = parse_intval(&parse_val, value);
	}
	if ( err || *parse_val ) {
		parse_val = val_str;
		err = parse_strval(&parse_val, value);
	}
	return PDI_OK;
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
	//TODO: implement
	abort();
}

PDI_status_t PDI_value_str(PDI_value_t* value, char** res)
{
	//TODO: implement
	abort();
}
