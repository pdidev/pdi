/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi/expression.h>

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
	
public:
	/** Builds a Dataset_op from its yaml config
	 *
	 * \param dir the operation direction
	 * \param name the value name
	 * \param default_when the default "when" clause as read from the file level (optional)
	 */
	Dataset_op(Direction dir, std::string name, PDI::Expression default_when):
		m_direction{dir},
		m_dataset{name},
		m_value{name},
		m_when{default_when}
	{
	}
	
	/** Builds a Dataset_op from its yaml config
	 *
	 * \param dir the operation direction
	 * \param name the value name
	 * \param default_when the default "when" clause as read from the file level (optional)
	 * \param tree the configuration tree
	 */
	Dataset_op(Direction dir, std::string name, PDI::Expression default_when, PC_tree_t tree);
	
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

