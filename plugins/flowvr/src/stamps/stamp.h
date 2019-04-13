/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_FLOWVR_STAMP
#define PDI_FLOWVR_STAMP

#include <memory>
#include <string>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <spdlog/spdlog.h>

#include <flowvr/module.h>
#include <flowvr/stamp.h>

#include "stamp_base.h"
#include "stamp_desc.h"
#include "stamp_expr.h"

namespace  {

class Stamp
{
	PDI::Context& m_ctx;
	std::unique_ptr<Stamp_base> m_stamp; // stamp holder
	
	/**
	 *  Sets up stamp as descriptor
	 *
	 *  \param[in] parent_port the pointer to the port that holds this stamp
	 *  \param[in] name name of this stamp
	 *  \param[in] data_desc descriptor of the stamp
	 */
	void load_stamp_desc_type(const flowvr::Port* parent_port, std::string name, std::string data_desc)
	{
		size_t stamp_size = 1; //default for scalars
		
		const PDI::Datatype_uptr stamp_datatype = m_ctx[data_desc].default_type()->evaluate(m_ctx);
		const PDI::Scalar_datatype* scalar_datatype = dynamic_cast<PDI::Scalar_datatype*>(stamp_datatype.get());
		const PDI::Array_datatype* array_datatype = dynamic_cast<PDI::Array_datatype*>(stamp_datatype.get());
		
		if (array_datatype) {
			stamp_size = array_datatype->size();
			scalar_datatype = dynamic_cast<const PDI::Scalar_datatype*>(&array_datatype->subtype());
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
		throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Stamp must be int, float, array of ints, floats or chars"};
	}
	
	/**
	 *  Sets up stamp as expression
	 *
	 *  \param[in] parent_port the pointer to the port that holds this stamp
	 *  \param[in] name name of this stamp
	 *  \param[in] expr_node expression node of stamp definition
	 *  \param[in] type_node type node of stamp definition
	 */
	void load_stamp_expr(const flowvr::Port* parent_port, std::string name, PC_tree_t expr_node, PC_tree_t type_node)
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
		//until expression double added
		if (type == "float") {
			throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Float expression stamp is not yet supported"};
			PDI::Expression expr = PDI::to_string(expr_node);
			// m_stamp.reset(new Stamp_expr_float(m_ctx, parent_port, std::move(name), std::move(expr)));
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
				//until expression double added
				throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Float expression stamp is not yet supported"};
				size = PDI::Expression(PDI::to_string(PC_get(type_node, ".size"))).to_long(m_ctx);
				std::vector<PDI::Expression> expressions;
				for (int expr_id = 0; expr_id < size; expr_id++) {
					expressions.emplace_back(PDI::to_string(PC_get(expr_node, "[%d]", expr_id)));
				}
				// m_stamp.reset(new Stamp_expr_float_array(m_ctx, parent_port, std::move(name), std::move(expressions)));
			} else if (subtype_str == "char") {
				PDI::Expression expr = PDI::to_string(expr_node);
				m_stamp.reset(new Stamp_expr_string(m_ctx, parent_port, std::move(name), std::move(expr)));
				return;
			} else {
				throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Stamp must be int, float, array of ints, floats or chars"};
			}
		}
	}
	
public:
	Stamp(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, PC_tree_t stamp_node):
		m_ctx{ctx}
	{
		PC_tree_t expr_node = PC_get(stamp_node, ".expression");
		if (!PC_status(expr_node)) {
			load_stamp_expr(parent_port, name, expr_node, PC_get(stamp_node, ".type"));
		} else {
			load_stamp_desc_type(parent_port, name, PDI::to_string(stamp_node));
		}
	}
	
	Stamp(const Stamp& other) = delete;
	
	Stamp(Stamp&& other):
		m_ctx{other.m_ctx},
		m_stamp{std::move(other.m_stamp)}
	{}
	
	Stamp& operator = (const Stamp& other) = delete;
	
	Stamp& operator = (Stamp&& other)
	{
		m_ctx = other.m_ctx;
		m_stamp = std::move(other.m_stamp);
		return *this;
	}
	
	/**
	 *  Updates stamp values from flowvr::Stamps
	 *
	 *  \param[in] read_stamp flowvr::Stamp with update values
	 */
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp)
	{
		m_stamp->read_from_flowvr_stamp(read_stamp);
	}
	
	/**
	 *  Put stamps to flowvr::StampsWrite
	 *
	 *  \param[in] write_stamp flowvr::StampWrite where to write stamp values
	 */
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const
	{
		m_stamp->write_to_flowvr_stamp(write_stamp);
	}
	
	/**
	 *  \return name of this stamp
	 */
	std::string get_name() const
	{
		return m_stamp->get_name();
	}
	
	/**
	 *  \return flowvr::StampInfo of this stamp
	 */
	flowvr::StampInfo* get_stamp_info() const
	{
		return m_stamp->get_stamp_info();
	}
	
	~Stamp() = default;
	
}; // class Stamp

} // namespace <anonymous>

#endif // PDI_FLOWVR_STAMP