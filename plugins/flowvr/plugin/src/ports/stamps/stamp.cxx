/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/array_datatype.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/scalar_datatype.h>

#include "ports/stamps/stamp.h"

namespace _flowvr_plugin {

using std::dynamic_pointer_cast;

void Stamp::load_stamp_desc_type(const flowvr::Port* parent_port, const std::string& name, const std::string& data_desc)
{
	size_t stamp_size = 1; //default for scalars
	
	const PDI::Datatype_sptr stamp_datatype = m_ctx[data_desc].default_type()->evaluate(m_ctx);
	auto&& scalar_datatype = dynamic_pointer_cast<const PDI::Scalar_datatype>(stamp_datatype);
	auto&& array_datatype = dynamic_pointer_cast<const PDI::Array_datatype>(stamp_datatype);
	
	if (array_datatype) {
		stamp_size = array_datatype->size();
		scalar_datatype = dynamic_pointer_cast<const PDI::Scalar_datatype>(array_datatype->subtype());
	}
	
	if (scalar_datatype) {
		PDI::Scalar_kind stamp_kind = scalar_datatype->kind();
		if (scalar_datatype->kind() == PDI::Scalar_kind::SIGNED && scalar_datatype->buffersize() == sizeof(int)) {
			if (stamp_size == 1) {
				m_stamp.reset(new Stamp_desc_int(m_ctx, parent_port, name, data_desc));
			} else {
				m_stamp.reset(new Stamp_desc_int_array(m_ctx, parent_port, name, data_desc, stamp_size));
			}
			return;
		} else if (scalar_datatype->kind() == PDI::Scalar_kind::FLOAT  && scalar_datatype->buffersize() == sizeof(float)) {
			if (stamp_size == 1) {
				m_stamp.reset(new Stamp_desc_float(m_ctx, parent_port, name, data_desc));
			} else {
				m_stamp.reset(new Stamp_desc_float_array(m_ctx, parent_port, name, data_desc, stamp_size));
			}
			return;
		} else if (array_datatype && scalar_datatype->kind() == PDI::Scalar_kind::UNSIGNED && scalar_datatype->buffersize() == sizeof(char)) {
			m_stamp.reset(new Stamp_desc_string(m_ctx, parent_port, name, data_desc));
			return;
		}
	}
	throw PDI::Type_error{"FlowVR stamp must be int, float, array of ints, floats or chars"};
}

void Stamp::load_stamp_expr(const flowvr::Port* parent_port, const std::string& name, PC_tree_t expr_node, PC_tree_t type_node)
{
	std::string type;
	long size = 1;
	try {
		type = PDI::to_string(PC_get(type_node, ".type"));
	} catch (PDI::Error e) {
		type = PDI::to_string(type_node);
	}
	
	if (type == "int") {
		PDI::Expression expr = PDI::to_string(expr_node);
		m_stamp.reset(new Stamp_expr_int(m_ctx, parent_port, std::move(name), std::move(expr)));
		return;
	}
	
	if (type == "float") {
		PDI::Expression expr = PDI::to_string(expr_node);
		m_stamp.reset(new Stamp_expr_float(m_ctx, parent_port, std::move(name), std::move(expr)));
		return;
	}
	
	if (type == "array") {
		std::string subtype_str = PDI::to_string(PC_get(type_node, ".subtype"));
		if (subtype_str == "int") {
			size = PDI::Expression(PDI::to_string(PC_get(type_node, ".size"))).to_long(m_ctx);
			std::vector<PDI::Expression> expressions;
			for (int expr_id = 0; expr_id < size; expr_id++) {
				expressions.emplace_back(PDI::to_string(PC_get(expr_node, "[%d]", expr_id)));
			}
			m_stamp.reset(new Stamp_expr_int_array(m_ctx, parent_port, std::move(name), std::move(expressions)));
		} else if (subtype_str == "float") {
			size = PDI::Expression(PDI::to_string(PC_get(type_node, ".size"))).to_long(m_ctx);
			std::vector<PDI::Expression> expressions;
			for (int expr_id = 0; expr_id < size; expr_id++) {
				expressions.emplace_back(PDI::to_string(PC_get(expr_node, "[%d]", expr_id)));
			}
			m_stamp.reset(new Stamp_expr_float_array(m_ctx, parent_port, std::move(name), std::move(expressions)));
		} else if (subtype_str == "char") {
			PDI::Expression expr = PDI::to_string(expr_node);
			m_stamp.reset(new Stamp_expr_string(m_ctx, parent_port, std::move(name), std::move(expr)));
			return;
		} else {
			throw PDI::Type_error{"FlowVR stamp must be int, float, array of ints, floats or chars"};
		}
	}
}

Stamp::Stamp(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, PC_tree_t stamp_node):
	m_ctx{ctx}
{
	PC_tree_t expr_node = PC_get(stamp_node, ".expression");
	if (!PC_status(expr_node)) {
		load_stamp_expr(parent_port, name, expr_node, PC_get(stamp_node, ".type"));
	} else {
		load_stamp_desc_type(parent_port, name, PDI::to_string(stamp_node));
	}
}

Stamp::Stamp(Stamp&& other):
	m_ctx{other.m_ctx},
	m_stamp{std::move(other.m_stamp)}
{}

Stamp& Stamp::operator=(Stamp&& other)
{
	m_ctx = other.m_ctx;
	m_stamp = std::move(other.m_stamp);
	return *this;
}

void Stamp::read_from_flowvr_stamp(const flowvr::Stamps& read_stamp)
{
	m_stamp->read_from_flowvr_stamp(read_stamp);
}

void Stamp::write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const
{
	m_stamp->write_to_flowvr_stamp(write_stamp);
}

std::string Stamp::get_name() const
{
	return m_stamp->get_name();
}

flowvr::StampInfo* Stamp::get_stamp_info() const
{
	return m_stamp->get_stamp_info();
}

Stamp::~Stamp() = default;

} // namespace _flowvr_plugin
