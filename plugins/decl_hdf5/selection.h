/*
 * SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef DECL_HDF5_SELECTION_H_
#define DECL_HDF5_SELECTION_H_

#include <vector>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace decl_hdf5 {

/** This identifies a selection in an (array) Datatype
 */
class Selection
{
	/// The tree representing the selection
	PC_tree_t m_selection_tree;

	/// The size of the selection in each dimension or empty for default
	std::vector<PDI::Expression> m_size;

	/// The first included point in each dimension or empty for default
	std::vector<PDI::Expression> m_start;

public:
	/** The default constructor for an empty selection (everything selected)
	 */
	Selection() = default;

	/** Builds a selection from a yaml tree.
	 *
	 * The tree should be a mapping with two optional keys:
	 * - size: a list (or scalar in 1D) of expressions or nothing for default
	 * - start: a list (or scalar in 1D) of expressions or nothing for default
	 *   if absent
	 *
	 * \param tree the tree representing the selection.
	 */
	Selection(PC_tree_t tree);

	/** Accesses the size of the selection in each dimension or nothing for
	 * default.
	 *
	 * \return the size of the selection in each dimension or nothing for default
	 */
	const std::vector<PDI::Expression>& size() const { return m_size; }

	/** Accesses the first included point in each dimension or nothing for
	 * default.
	 *
	 * \return The first included point in each dimension or nothing for default
	 */
	const std::vector<PDI::Expression>& start() const { return m_start; }

	/** Accesses the first included point in each dimension or nothing for
	 * default.
	 *
	 * \return The first included point in each dimension or nothing for default
	 */
	PC_tree_t selection_tree() const { return m_selection_tree; }

	/** Select a part of a HDF5 space based on a selection
	 * \param ctx the context in which to operate
	 * \param h5_space the space to modify
	 * \param dflt_space a space to match if the selection is empty
	 */
	void apply(PDI::Context& ctx, hid_t h5_space, hid_t dflt_space = -1) const;
};

} // namespace decl_hdf5

#endif // DECL_HDF5_SELECTION_H_
