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

#ifndef PDI_DATA_DESCRIPTOR_FWD_H_
#define PDI_DATA_DESCRIPTOR_FWD_H_

#include <memory>

#include <pdi.h>

/** Forward declaration of all PDI types
 * 
 * \file fwd.h
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 * \author Corentin Roussel (CEA) <corentin.roussel@cea.fr>
 */

namespace PDI
{

/** Describes the state of a PDI instanciation, its configuration, the
 *  currently exposed data, etc...
 */
class Context;

/** The properties of a data (data type, kind, ...etc.)
 */
class Data_descriptor;

template<bool, bool> class Data_A_ref;

typedef Data_A_ref<false, false> Data_ref;

typedef Data_A_ref<true, false> Data_r_ref;

typedef Data_A_ref<false, true> Data_w_ref;

typedef Data_A_ref<true, true> Data_rw_ref;

/** A PDI type descriptor
 *
 * A Data type is either a scalar, an array, or a record.
 */
class Data_type;

typedef std::unique_ptr<Data_type> Data_type_uptr;

/** Different possible interpretations for a scalar
	*/
enum class Scalar_kind: uint8_t { UNKNOWN, SIGNED, UNSIGNED, FLOAT, ADDRESS };

/** A PDI type template descriptor
 *
 * A template can be evaluated into a type by resolving its references
 */
class Type_template;

typedef std::unique_ptr<Type_template> Type_template_uptr;

/** A parsed value as specified by an expression.
 *
 * References are not resolved, this is only the AST.
 */
class Value;

}

/** Definition of a plugin
 */
typedef struct PDI_plugin_s PDI_plugin_t;

#endif
