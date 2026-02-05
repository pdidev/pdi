/*
 * SPDX-FileCopyrightText: 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
