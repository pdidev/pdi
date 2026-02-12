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


#ifndef DECL_HDF5_FILE_OP_H_
#define DECL_HDF5_FILE_OP_H_

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
#include <mpi.h>
#endif

#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

#include "attribute_op.h"
#include "collision_policy.h"
#include "dataset_explicit_type.h"
#include "dataset_op.h"

namespace decl_hdf5 {

/** A File_op represents an operation on a file: opening it, applying one or
 * more dataset operations and closing it.
 */
class File_op
{
	/// What to do when file already exists (default = OVERWRITE)
	Collision_policy m_collision_policy;

	/// the file where the operation takes place (mandatory)
	PDI::Expression m_file;

	/// a list of events that trigger this operation
	std::vector<std::string> m_event;

#ifdef H5_HAVE_PARALLEL
	/// a communicator for parallel HDF5 (null if no comm is specified)
	PDI::Expression m_communicator;
#endif

	/// type information for the datasets for which an explicit type is specified
	std::vector<Dataset_explicit_type> m_datasets;

	/// the dataset operations
	std::vector<Dataset_op> m_dset_ops;

	/// attributes of this file (for groups and datasets)
	std::vector<Attribute_op> m_attr_ops;

	/// map of descriptors to datasets name to get their sizes
	std::unordered_map<std::string, PDI::Expression> m_dset_size_ops;

	/// HDF5 subfiling
	PDI::Expression m_subfiling;

public:
	/** Parse a "file" subtree to create one or multiple File_op's.
	 *
	 * Multiple File_op's are created when a "file" subtree is data activated
	 * and contains multiple data.
	 *
	 * \param ctx the context from which to access datatypes used
	 * \param tree the "file" subtree
	 * \return a vector containing all parsed File_op's
	 */
	static std::vector<File_op> parse(PDI::Context& ctx, PC_tree_t tree);

	File_op(File_op&&) = default;

	File_op(const File_op&);

	File_op(PDI::Expression&& file, Collision_policy collision_policy = Collision_policy::WRITE_INTO);

	/** a list of events that trigger this operation
	 */
	const std::vector<std::string>& event() const { return m_event; }

	/** the dataset operations
	 */
	const std::vector<Dataset_op>& dataset_ops() const { return m_dset_ops; }

	/** Returns the attribute operations
	 *
	 * \return attribute operations
	 */
	const std::vector<Attribute_op>& attribute_ops() const { return m_attr_ops; }

	/** Returns the dataset size operations
	 *
	 * \return dataset size operations
	 */
	const std::unordered_map<std::string, PDI::Expression>& dataset_size_ops() const { return m_dset_size_ops; }

#ifdef H5_HAVE_PARALLEL
	PDI::Expression communicator() const { return m_communicator; }

	PDI::Expression subfiling() const { return m_subfiling; }
#endif

	/** Executes the requested operation.
	 *
	 * \param ctx the context in which to operate
	 */
	void execute(PDI::Context& ctx);
};

} // namespace decl_hdf5

#endif // DECL_HDF5_FILE_OP_H_
