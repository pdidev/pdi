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

#ifndef PDI_VALUE_FWD_H__
#define PDI_VALUE_FWD_H__

namespace PDI {

/** A parsed value as specified by an expression.
 *
 * References are not resolved, this is only the AST.
 *
 * The grammar of an expression is as follow:
 * ```
 * VALUE   := INTVAL | STRVAL
 * STRVAL  := ( CHAR | '\' '\' | '\' '$' | REF | '$' '(' INTVAL ')' )*
 * INTVAL  := INTVAL2 ( '|' INTVAL2 )*
 * INTVAL2 := INTVAL3 ( '&' INTVAL3 )*
 * INTVAL3 := INTVAL4 ( '=' INTVAL4 )*
 * INTVAL4 := INTVAL5 ( ( '+' | '-' ) INTVAL5 )*
 * INTVAL5 := TERM ( ( '*' | '/' | '%' ) TERM )*
 * TERM    := ( CONST | REF | '(' INTVAL ')' )
 * REF     := '$' ( IREF | '{' IREF '}' )
 * IREF    := ID ( '[' INTVAL ']' )*
 * CONST ~= (0x)? [0-9]+ ( \.  )
 * ID    ~= [a-zA-Z_][a-zA-Z0-9_]*
 * CHAR  ~= [^$\\]
 * ```
 * 
 * \author Julie Bigot (CEA) <julien.bigot@cea.fr>
 */
class Value;

} // namespace PDI

typedef PDI::Value PDI_value_t;

#endif // PDI_VALUE_FWD_H__
