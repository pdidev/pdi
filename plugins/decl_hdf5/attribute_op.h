/*******************************************************************************
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

#ifndef DECL_HDF5_ATTRIBUTE_OP_H_
#define DECL_HDF5_ATTRIBUTE_OP_H_

#include <string>
#include <hdf5.h>

#include <pdi/context.h>
#include <pdi/expression.h>

namespace decl_hdf5 {

class Attribute_op
{
public:
	/** an  I/O direction (read or write)
	 */
	enum Direction {
		READ,
		WRITE
	};

private:
	/// Descriptor of the attribute (can be empty)
	std::string m_desc;

	/// Direction of the transfer (read or write)
	Direction m_direction;

	/// Name of the attribute
	std::string m_name;

	/// Path to the object of this attribute
	PDI::Expression m_object_path;

	/// Value of the attribute
	PDI::Expression m_value;

	/// Condition to check before doing the transfer
	PDI::Expression m_when;

public:
	/** Creates HDF5 attribute operation
	 *
	 * \param direction direction of I/O (READ or WRITE)
	 * \param attr_path_tree tree with object path with attribute name separated by #
	 * \param when a condition to check before doing the transfer
	 */
	Attribute_op(Direction direction, PC_tree_t attr_path_tree, PDI::Expression when);

	/** Creates HDF5 attribute operation
	 *
	 * \param direction direction of I/O (READ or WRITE)
	 * \param desc descriptor correlated with attribute
	 * \param when a condition to check before doing the transfer
	 * \param tree tree of the attribute
	 */
	Attribute_op(Direction direction, const std::string& desc, PDI::Expression when, PC_tree_t tree);

	/** Creates HDF5 attribute operation
	 *
	 * \param direction direction of I/O (READ or WRITE)
	 * \param object_path object path of attribute (without attribute name)
	 * \param attr_name name of the attribute
	 * \param value attribute value to write or read
	 * \param when a condition to check before doing the transfer
	 */
	Attribute_op(Direction direction, PDI::Expression object_path, const std::string& attr_name, PDI::Expression value, PDI::Expression when = 1L);

	/** Accesses the condition to check before doing the transfer.
	 *
	 * \return The condition to check before doing the transfer
	 */
	PDI::Expression when() const;

	/** Accesses the direction of the transfer (read or write).
	 *
	 * \return The direction of the transfer (read or write)
	 */
	Direction direction() const;

	/** Accesses the name of the attribute.
	 *
	 * \return The name of the attribute
	 */
	std::string name() const;

	/** Returns descriptor of the attribute (can be empty)
	 *
	 * \return descriptor of the attribute
	 */
	std::string desc() const;

	/** Executes the requested operation.
	 *
	 * \param ctx the context in which to operate
	 * \param h5_file the already opened HDF5 file id
	 */
	void execute(PDI::Context& ctx, hid_t h5_file) const;

private:
	/** Executes write operation.
	 *
	 * \param ctx the context in which to operate
	 * \param h5_file the already opened HDF5 file id
	 */
	void do_write(PDI::Context& ctx, hid_t h5_file) const;

	/** Executes read operation.
	 *
	 * \param ctx the context in which to operate
	 * \param h5_file the already opened HDF5 file id
	 */
	void do_read(PDI::Context& ctx, hid_t h5_file) const;
};

} // namespace decl_hdf5

#endif // DECL_HDF5_ATTRIBUTE_OP_H_
