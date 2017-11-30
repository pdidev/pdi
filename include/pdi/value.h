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
* \file value.h
* \brief Structures and functions to parse expression
* \author J. Bigot (CEA)
*/

#ifndef PDI_VALUE_H__
#define PDI_VALUE_H__

#include <pdi.h>

#include <pdi/value_fwd.h>
#include <pdi/state_fwd.h>

namespace PDI {

/** A value in case this is a reference to another value
 */
class PDI_refval_t;

/** A value in case this is an expression
 */
typedef struct PDI_exprval_s PDI_exprval_t;

/** A value in case this is a string (potentially with dollar refs inside)
 */
typedef struct PDI_strval_s PDI_strval_t;

class Value
{
public:
	/** the possible kind of values
	 */
	typedef enum PDI_valkind_e {
		PDI_VAL_CONST,
		PDI_VAL_REF,
		PDI_VAL_EXPR,
		PDI_VAL_STR
	} PDI_valkind_t;

	/// the kind of value this is
	PDI_valkind_t kind;
	
	union {
		/// in case this is a PDI_VAL_CONST, the constant value
		long constval;
		
		/// in case of PDI_VAL_REF the referenced value (not owned)
		PDI_refval_t *refval;
		
		/// in case of PDI_VAL_EXPR the expression (owned)
		PDI_exprval_t *exprval;
		
		/// in case of PDI_VAL_STR the string (owned)
		PDI_strval_t *strval;
		
	} c;
	
	Value();
	
	/** Builds (i.e. parse) a value from a string
	 *
	 * \param[in] val_str the string to parse
	 */
	explicit Value(const char *val_str);
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 */
	Value(const Value& value);
	
	Value(Value&& value);
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 */
	Value& operator=(const Value& value);

	Value& operator=(Value&& value);

	/** Destroys a PDI value
	 */
	~Value();
	
};

/** Evaluates a value as an integer
 *
 * \param[in] value the value to evaluate
 * \param[out] res a pointer to the integer value
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_value_int(const PDI_value_t *value, long *res);

/** Evaluates a value as a string
 *
 * \param[in] value the value to evaluate
 * \param[out] res a pointer to the string value
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_value_str(const PDI_value_t *value, char **res);

} // namespace PDI

#endif // PDI_VALUE_H__
