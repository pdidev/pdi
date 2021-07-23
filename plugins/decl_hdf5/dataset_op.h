/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_DATASET_OP_H_
#define DECL_HDF5_DATASET_OP_H_

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
	#include <mpi.h>
#endif

#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/expression.h>

#include "attribute_op.h"
#include "collision_policy.h"
#include "selection.h"


namespace decl_hdf5 {

/** A Dataset_op represents an operation on a dataset: creating it and a read or
 * write.
 */
class Dataset_op
{
public:
	/** an  I/O direction (read or write)
	 */
	enum Direction { READ, WRITE };
	
private:
	/// What to do when dataset already exists (default = OVERWRITE)
	Collision_policy m_collision_policy;
	
	/// direction of the transfer (read or write)
	Direction m_direction;
	
	/// the name of the dataset where to transfer
	PDI::Expression m_dataset;
	
	/// the name of the value to transfer
	std::string m_value;
	
	/// a condition to check before doing the transfer
	PDI::Expression m_when;
	
#ifdef H5_HAVE_PARALLEL
	/// a communicator for parallel HDF5 (only for data triggered)
	PDI::Expression m_communicator;
#endif
	
	/// a selection in-memory
	Selection m_memory_selection;
	
	/// a selection in-dataset
	Selection m_dataset_selection;
	
	/// chunking property set from yaml
	PDI::Expression m_chunking;
	
	/// deflate property set from yaml
	PDI::Expression m_deflate;
	
	/// fletcher property set from yaml
	PDI::Expression m_fletcher;
	
	/// attributes of this dataset
	std::vector<Attribute_op> m_attributes;
	
	/** Creates dataset plist to pass to H5D_create
	 *
	 * \param ctx the context in which to operate
	 * \param dataset_type type of the dataset
	 * \param dataset_name name of the dataset
	 *
	 * \return dataset creation plist hid_t
	 */
	hid_t dataset_creation_plist(PDI::Context& ctx, const PDI::Datatype* dataset_type, const std::string& dataset_name);
	
public:
	/** Builds a Dataset_op from its yaml config
	 *
	 * \param dir the operation direction
	 * \param name the value name
	 * \param default_when the default "when" clause as read from the file level (optional)
	 */
	Dataset_op(Direction dir, std::string name, PDI::Expression default_when, Collision_policy file_collision_policy = Collision_policy::WRITE_INTO);
	/** Builds a Dataset_op from its yaml config
	 *
	 * \param dir the operation direction
	 * \param name the value name
	 * \param default_when the default "when" clause as read from the file level (optional)
	 * \param tree the configuration tree
	 */
	Dataset_op(Direction dir, std::string name, PDI::Expression default_when, PC_tree_t tree, Collision_policy collision_policy = Collision_policy::WRITE_INTO);
	
	/** Accesses the direction of the transfer (read or write).
	 *
	 * \return The direction of the transfer (read or write)
	 */
	Direction direction()
	{
		return m_direction;
	}
	
	/** Accesses the name of the value to transfer.
	 *
	 * \return The name of the value to transfer
	 */
	std::string value() const
	{
		return m_value;
	}
	
	/** Accesses the name of the dataset where to transfer.
	 *
	 * \return The name of the dataset where to transfer
	 */
	const PDI::Expression& dataset() const
	{
		return m_dataset;
	}
	
	/** Accesses the condition to check before doing the transfer.
	 *
	 * \return The condition to check before doing the transfer
	 */
	const PDI::Expression& when() const
	{
		return m_when;
	}
	
#ifdef H5_HAVE_PARALLEL
	/** Accesses the communicator for parallel HDF5 (only for data triggered).
	 *
	 * \return The communicator for parallel HDF5 (only for data triggered)
	 */
	const PDI::Expression& communicator() const
	{
		return m_communicator;
	}
#endif
	
	/** Set deflate dataset level
	 *
	 * \param ctx the context in which to operate
	 * \param level level of the deflate
	 */
	void deflate(PDI::Context& ctx, PDI::Expression level);
	
	/** Set fletcher dataset
	 *
	 * \param ctx the context in which to operate
	 * \param value turn on fletcher if true, turn off if false
	 */
	void fletcher(PDI::Context& ctx, PDI::Expression value);
	
	/** Executes the requested operation.
	 *
	 * \param ctx the context in which to operate
	 * \param h5_file the already opened HDF5 file id
	 * \param xfer_lst the already created transfer property list including any
	 *                 parallel HDF5 required property.
	 * \param dsets the type of the explicitly typed datasets
	 */
	void execute(PDI::Context& ctx, hid_t h5_file, hid_t xfer_lst, const std::unordered_map<std::string, PDI::Datatype_template_uptr>& dsets);
	
private:
	void do_read(PDI::Context& ctx, hid_t h5_file, hid_t read_lst);
	
	void do_write(PDI::Context& ctx, hid_t h5_file, hid_t xfer_lst, const std::unordered_map<std::string, PDI::Datatype_template_uptr>& dsets);
};

} // namespace decl_hdf5

#endif // DECL_HDF5_DATASET_OP_H_

