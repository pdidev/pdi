/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

/** \file pdi/pdi_fwd.h
 * Forward declaration of all public PDI types
 */

#ifndef PDI_PDI_FWD_H_
#define PDI_PDI_FWD_H_

#include <cstdint>
#include <memory>

#include <pdi.h>

namespace PDI {

/** Describes the state of a PDI instanciation, mostly the set of data
 * descriptors
 */
class Context;

/** A PDI datatype
 *
 * A datatype is either: an array, a scalar, a pointer, a record or a tuple.
 */
class Datatype;

/** Array datatype derived from Datatype
    */
class Array_datatype;

/** Pointer datatype derived from Datatype
    */
class Pointer_datatype;

/** Record datatype derived from Datatype
    */
class Record_datatype;

/** Scalar datatype derived from Datatype
    */
class Scalar_datatype;

/** Tuple datatype derived from Datatype
    */
class Tuple_datatype;

/** A PDI datatype template
 *
 * A template can be evaluated into a datatype by resolving its references
 */
class Datatype_template;

using Datatype_template_ptr [[deprecated]] = std::shared_ptr<const Datatype_template>;
using Datatype_template_sptr = std::shared_ptr<const Datatype_template>;

using Datatype_sptr = std::shared_ptr<const Datatype>;

/**
 * A class to delayed the moment to call call_data_callback
*/
class Delayed_data_callbacks;

/** A data descriptors with a name and a value, it contains an implicit type
 * template that is used when exposing untyped data
 */
class Data_descriptor;

/** A class used as base for all PDI plugins
 */
class Plugin;

template <bool, bool>
class Ref_any;

typedef Ref_any<false, false> Ref;

typedef Ref_any<true, false> Ref_r;

typedef Ref_any<false, true> Ref_w;

typedef Ref_any<true, true> Ref_rw;

/** Different possible interpretations for a scalar
 */
enum class Scalar_kind : uint8_t {
	UNKNOWN,
	SIGNED,
	UNSIGNED,
	FLOAT
};

/** A parsed value as specified by an expression.
 *
 * References are not resolved, this is only the AST.
 */
class Expression;

} // namespace PDI

#endif // PDI_PDI_FWD_H_
