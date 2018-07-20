/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <memory>

#include <pdi.h>


namespace spdlog {
class logger;
}

namespace PDI {

/** Describes the state of a PDI instanciation, mostly the set of data
 * descriptors
 */
class Context;

/** A PDI datatype
 *
 * A datatype is either a scalar, an array, or a record.
 */
class Datatype;

/** A PDI datatype template
 *
 * A template can be evaluated into a datatype by resolving its references
 */
class Datatype_template;

typedef std::unique_ptr<Datatype_template> Datatype_template_uptr;

typedef std::unique_ptr<Datatype> Datatype_uptr;

/** A data descriptors with a name and a value, it contains an implicit type
 * template that is used when exposing untyped data
 */
class Data_descriptor;

/** A shared pointer to a logger instance
 */
typedef std::shared_ptr<spdlog::logger> Logger_sptr;

/** A class used as base for all PDI plugins
 */
class Plugin;

template<bool, bool> class Ref_any;

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
	FLOAT,
	ADDRESS,
	MPI_COMM
};

/** A parsed value as specified by an expression.
 *
 * References are not resolved, this is only the AST.
 */
class Expression;

}

#endif // PDI_PDI_FWD_H_
