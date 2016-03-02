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

#ifndef PDI_VALUE_H__
#define PDI_VALUE_H__

#include <pdi.h>

#include <pdi/value_fwd.h>
#include <pdi/state_fwd.h>

/* Grammar
VALUE   := INTVAL | STRVAL
STRVAL  := ( CHAR | '\' '\' | '\' '$' | REF | '$' '(' INTVAL ')' )*
INTVAL  := INTVAL2 ( OP1 INTVAL2 )*
INTVAL2 := TERM ( OP2 TERM )*
TERM    := ( CONST | REF | '(' INTVAL ')' )
REF     := '$' ( IREF | '{' IREF '}' )
IREF    := ID ( '[' INTVAL ']' )*
CONST ~= (0x)? [0-9]+ ( \.  )
OP1   ~= \+ | -
OP2   ~= \* | / | %
ID    ~= [a-zA-Z_][a-zA-Z0-9_]*
CHAR  ~= [^$\\]
*/

typedef struct PDI_refval_s PDI_refval_t;

typedef struct PDI_exprval_s PDI_exprval_t;

typedef struct PDI_strval_s PDI_strval_t;

struct PDI_value_s
{
	PDI_valkind_t kind;
	
	union {
		int constval;
		
		PDI_refval_t *refval;
		
		PDI_exprval_t *exprval;
		
		PDI_strval_t *strval;
		
	} c;
	
};

PDI_status_t PDI_EXPORT PDI_value_parse(char *val_str, PDI_value_t *value);

PDI_status_t PDI_EXPORT PDI_value_int(PDI_value_t *value, int *res);

PDI_status_t PDI_EXPORT PDI_value_str(PDI_value_t *value, char **res);

PDI_status_t PDI_EXPORT PDI_value_destroy(PDI_value_t *value);

#endif // PDI_VALUE_H__
