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

#ifndef PDI_FLOWVR_STAMP_EXPR
#define PDI_FLOWVR_STAMP_EXPR

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

namespace  {

class Stamp_expr_int : public Stamp_base
{
	PDI::Expression m_value;
public:
	Stamp_expr_int(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, PDI::Expression expression):
		Stamp_base{ctx, parent_port, name},
		m_value{expression}
	{
		if (!m_name.compare("it")) {
			m_stamp_info = &m_parent_port->stamps->it;
		} else if (!name.compare("num")) {
			m_stamp_info = &m_parent_port->stamps->num;
		} else {
			m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeInt::create());
		}
		m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Created", m_name);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		bool status = write_stamp.write(*m_stamp_info, static_cast<int>(m_value.to_long(m_ctx)));
		if (status) {
			m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value.to_long(m_ctx));
		} else {
			throw PDI::Error{PDI_UNAVAILABLE, "(FlowVR) Int STAMP ({}): Cannot write stamp to message", m_name};
		}
	}
};

// class Stamp_expr_float : public Stamp_base
// {
//  PDI::Expression m_value;
// public:
//  Stamp_expr_float(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, PDI::Expression expression):
//      Stamp_base{ctx, parent_port, name},
//      m_value{expression}
//  {
//      m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeFloat::create());
//      m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Created", m_name);
//  }

//  void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
//  {}

//  void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
//  {
//      bool status = write_stamp.write(*m_stamp_info, static_cast<float>(m_value.to_double(m_ctx)));
//      if (status) {
//          m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value.to_double(m_ctx));
//      } else {
//          throw PDI::Error{PDI_UNAVAILABLE, "(FlowVR) Float STAMP ({}): Cannot write stamp to message", m_name};
//      }
//  }

// };

class Stamp_expr_string : public Stamp_base
{
	PDI::Expression m_value;
public:
	Stamp_expr_string(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, PDI::Expression expression):
		Stamp_base{ctx, parent_port, name},
		m_value{expression}
	{
		if (!m_name.compare("source")) {
			m_stamp_info = &parent_port->stamps->source;
		} else {
			m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeString::create());
		}
		m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Created", m_name);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		bool status = write_stamp.write(*m_stamp_info, m_value.to_string(m_ctx));
		if (status) {
			m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value.to_string(m_ctx));
		} else {
			throw PDI::Error{PDI_UNAVAILABLE, "(FlowVR) String STAMP ({}): Cannot write stamp to message", m_name};
		}
	}
};

class Stamp_expr_int_array : public Stamp_base
{
	std::vector<PDI::Expression> m_value;
public:
	Stamp_expr_int_array(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::vector<PDI::Expression> expression):
		Stamp_base{ctx, parent_port, name},
		m_value{expression}
	{
		m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(m_value.size(), flowvr::TypeInt::create()));
		m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Created with size = {}", m_name, m_value.size());
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
			bool status = write_stamp.write((*m_stamp_info)[stamp_id], static_cast<int>(m_value[stamp_id].to_long(m_ctx)));
			if (status) {
				m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id].to_long(m_ctx));
			} else {
				throw PDI::Error{PDI_UNAVAILABLE, "(FlowVR) Int array STAMP ({}): Cannot write stamp to message", m_name};
			}
		}
	}
};

// class Stamp_expr_float_array : public Stamp_base
// {
//  std::vector<PDI::Expression> m_value;
// public:
//  Stamp_expr_float_array(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::vector<PDI::Expression> expression):
//      Stamp_base{ctx, parent_port, name},
//      m_value{expression}
//  {
//      m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(m_value.size(), flowvr::TypeFloat::create()));
//      m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Created with size = {}", m_name, m_value.size());
//  }

//  void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
//  {}

//  void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
//  {
//      for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
//          bool status = write_stamp.write((*m_stamp_info)[stamp_id], static_cast<float>(m_value[stamp_id].to_double(m_ctx)));
//          if (status) {
//              m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id].to_double(m_ctx));
//          } else {
//              throw PDI::Error{PDI_UNAVAILABLE, "(FlowVR) Float array STAMP ({}): Cannot write stamp to message", m_name};
//          }
//      }
//  }
// };

} // namespace <anonymous>

#endif // PDI_FLOWVR_STAMP_EXPR
