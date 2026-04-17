/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_SUBFILING_H_
#define DECL_HDF5_SUBFILING_H_

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace decl_hdf5 {

/** This identifies a selection in an (array) Datatype
 */
class Subfiling
{
	/// The number of subfiles
	PDI::Expression m_sf_count = 0L;

	/// The stripe size for subfiling
	PDI::Expression m_sf_stripe_size = 0L;

	/// The policy for subfiling when problem
	PDI::Expression m_sf_policy = "STOP";

public:
	/** The default constructor for an empty selection (everything selected)
	 */
	Subfiling() = default;

	/** Builds a subfiling from a yaml tree.
	 *
	 * The tree should be a mapping with two optional keys:
	 * - count: an expressions, or an integer value
	 * - stripe_size: an expressions, or an integer value
	 * - policy: an expression representing either CONTINUE or STOP
	 *
	 * \param tree the tree representing the subfiling.
	 */
	Subfiling(PC_tree_t tree);

	/** Accesses the number of subfiles that will be generated.
	 *
	 * \return the number of subfiles
	 */
	const PDI::Expression& count() const { return m_sf_count; }

	/** Accesses the stripe size of the subfiling configuration.
	 *
	 * \return The stripe size of the subfiling configuration
	 */
	const PDI::Expression& stripe_size() const { return m_sf_stripe_size; }

	/** Accesses the policy of subfiling configuration if problem
	 * default.
	 *
	 * \return The the policy of subfiling configuration if problem
	 */
	const PDI::Expression& policy() const { return m_sf_policy; }
};

} // namespace decl_hdf5

#endif // DECL_HDF5_SELECTION_H_
