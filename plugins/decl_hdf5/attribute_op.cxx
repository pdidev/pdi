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

#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>

#include "hdf5_wrapper.h"

#include "attribute_op.h"

namespace decl_hdf5 {


using PDI::Config_error;
using PDI::Context;
using PDI::each;
using PDI::Expression;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::to_string;
using PDI::Value_error;
using std::move;
using std::string;
using std::tie;


Attribute_op::Attribute_op(Direction direction, PC_tree_t attr_path_tree, Expression when):
	m_direction{direction},
	m_when{move(when)}
{
	string attr_path = to_string(attr_path_tree);
	size_t pos = attr_path.find('#');
	if (pos == string::npos) {
		throw Config_error{attr_path_tree, "Attribute path must contain `#' sign to separate group/dataset from attribute name"};
	}
	m_object_path = Expression{attr_path.substr(0, pos)};
	m_name = attr_path.substr(pos + 1);
	m_desc = m_name;
	m_value = "$" + m_name;
}

Attribute_op::Attribute_op(Direction direction, const string& desc, Expression when, PC_tree_t tree):
	m_desc{desc},
	m_direction{direction},
	m_value{"$" + desc},
	m_when{move(when)}
{
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		string key = to_string(key_tree);
		if ( key == "attribute" ) {
			string attr_path = to_string(value);
			size_t pos = attr_path.find('#');
			if (pos == string::npos) {
				m_object_path = attr_path;
				m_name = m_desc;
			} else {
				m_object_path = Expression{attr_path.substr(0, pos)};
				m_name = attr_path.substr(pos + 1);
			}
		} else if ( key == "when" ) {
			m_when = to_string(value);
		} else {
			throw Config_error{key_tree, "Unknown key for HDF5 attribute configuration: `{}'", key};
		}
	});
}

Attribute_op::Attribute_op(Direction direction, Expression object_path, const string& attr_name, Expression value, Expression when):
	m_direction{direction},
	m_name{attr_name},
	m_object_path{move(object_path)},
	m_value{move(value)},
	m_when{when}
{}

Expression Attribute_op::when() const
{
	return m_when;
}

Attribute_op::Direction Attribute_op::direction() const
{
	return m_direction;
}

string Attribute_op::name() const
{
	return m_name;
}

string Attribute_op::desc() const
{
	return m_desc;
}

void Attribute_op::do_write(Context& ctx, hid_t h5_file) const
{
	ctx.logger().trace("Preparing for writing `{}' attribute", m_name);
	Ref_r ref = m_value.to_ref(ctx);
	if (!ref) {
		ctx.logger().warn("Cannot write `{}' attribute: data not available", m_name);
		return;
	}
	
	// cannot use H5Oopen, because HDF5 1.8 does not support opening groups
	std::string object_path_str = m_object_path.to_string(ctx);
	ctx.logger().trace("Trying to open `{}' as a group", object_path_str);
	Raii_hid h5_dest = Raii_hid{H5Gopen(h5_file, object_path_str.c_str(), H5P_DEFAULT), H5Gclose};
	if (h5_dest < 0) {
		ctx.logger().trace("Failed to open `{}' as a group, trying as a dataset", object_path_str);
		h5_dest = make_raii_hid(H5Dopen(h5_file, object_path_str.c_str(), H5P_DEFAULT), H5Dclose, ("Cannot open attribute destination (" + object_path_str + "): ").c_str());
	}
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	
	ctx.logger().trace("Opening `{}' attribute", m_name);
	Raii_hid attr_id = Raii_hid{H5Aopen(h5_dest, m_name.c_str(), H5P_DEFAULT), H5Aclose};
	if (attr_id < 0) {
		ctx.logger().trace("Cannot open `{}' attribute, creating", m_name);
		attr_id =  make_raii_hid(H5Acreate(h5_dest, m_name.c_str(), h5_mem_type, h5_mem_space, H5P_DEFAULT, H5P_DEFAULT), H5Aclose, ("Cannot open nor create " + m_name + " attribute: ").c_str());
	}
	
	ctx.logger().trace("Writing `{}' attribute", m_name);
	if (H5Awrite(attr_id, h5_mem_type, ref.get()) < 0) {
		handle_hdf5_err(("Cannot write " + m_name + " attribute value: ").c_str());
	}
	
	ctx.logger().trace("`{}' attribute write finished", m_name);
}

void Attribute_op::execute(Context& ctx, hid_t h5_file) const
{
	if (m_direction == Direction::WRITE) {
		do_write(ctx, h5_file);
	} else {
		do_read(ctx, h5_file);
	}
}

void Attribute_op::do_read(Context& ctx, hid_t h5_file) const
{
	ctx.logger().trace("Preparing for reading `{}' attribute", m_name);
	Ref_w ref = m_value.to_ref(ctx);
	if (!ref) {
		ctx.logger().warn("Cannot read `{}' attribute: data not available", m_name);
		return;
	}
	
	// cannot use H5Oopen, because HDF5 1.8 does not support opening groups
	std::string object_path_str = m_object_path.to_string(ctx);
	ctx.logger().trace("Trying to open `{}' as a group", object_path_str);
	Raii_hid h5_dest = Raii_hid{H5Gopen(h5_file, object_path_str.c_str(), H5P_DEFAULT), H5Gclose};
	if (h5_dest < 0) {
		ctx.logger().trace("Failed to open `{}' as a group, trying as a dataset", object_path_str);
		h5_dest = make_raii_hid(H5Dopen(h5_file, object_path_str.c_str(), H5P_DEFAULT), H5Dclose, ("Cannot open attribute destination (" + object_path_str + "): ").c_str());
	}
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	
	ctx.logger().trace("Opening `{}' attribute", m_name);
	Raii_hid attr_id = make_raii_hid(H5Aopen(h5_dest, m_name.c_str(), H5P_DEFAULT), H5Aclose, ("Cannot open " + m_name + " attribute value: ").c_str());
	
	ctx.logger().trace("Reading `{}' attribute", m_name);
	if (H5Aread(attr_id, h5_mem_type, ref.get()) < 0) {
		handle_hdf5_err(("Cannot read " + m_name + " attribute value: ").c_str());
	}
	
	ctx.logger().trace("`{}' attribute read finished", m_name);
}

} // namespace decl_hdf5
