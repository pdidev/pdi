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

#ifndef PDI_FLOWVR_STAMP_DESC
#define PDI_FLOWVR_STAMP_DESC

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

namespace  {

class Stamp_desc_int : public Stamp_base
{
	int m_value;
public:
	Stamp_desc_int(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::string data_desc):
		Stamp_base{ctx, parent_port, name}
	{
		if (!m_name.compare("it")) {
			m_stamp_info = &m_parent_port->stamps->it;
		} else if (!name.compare("num")) {
			m_stamp_info = &m_parent_port->stamps->num;
		} else {
			m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeInt::create());
		}
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, data_desc));
		m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Created", m_name);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{
		int stamp_value;
		bool status = read_stamp.read(*m_stamp_info, stamp_value);
		if (status) {
			m_value = stamp_value;
			m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Update from message: Stamp = {}", m_name, stamp_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) Int STAMP ({}): Cannot read stamp value from message", m_name};
		}
	}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		bool status = write_stamp.write(*m_stamp_info, m_value);
		if (status) {
			m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) Int STAMP ({}): Cannot write stamp to message", m_name};
		}
	}
	
	void data(const std::string& data_name, const PDI::Ref& ref)
	{
		if (PDI::Ref_w ref_w{ref}) {
			//can write to desc, put value to it
			*static_cast<int*>(ref_w.get()) = m_value;
			m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Desc `{}' = {}", m_name, data_name, m_value);
		}
		if (PDI::Ref_r ref_r{ref}) {
			//can read from desc, update m_value
			m_value = *static_cast<const int*>(ref_r.get());
			m_ctx.logger()->debug("(FlowVR) Int STAMP ({}): Stamp = {} from `{}'", m_name, m_value, data_name);
		}
	}
};

class Stamp_desc_float : public Stamp_base
{
	float m_value;
public:
	Stamp_desc_float(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::string data_desc):
		Stamp_base{ctx, parent_port, name}
	{
		m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeFloat::create());
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, data_desc));
		m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Created", m_name);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{
		float stamp_value;
		bool status = read_stamp.read(*m_stamp_info, stamp_value);
		if (status) {
			m_value = stamp_value;
			m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Update from message: Stamp = {}", m_name, stamp_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) Float STAMP ({}): Cannot read stamp value from message", m_name};
		}
	}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		bool status = write_stamp.write(*m_stamp_info, m_value);
		if (status) {
			m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) Float STAMP ({}): Cannot write stamp to message", m_name};
		}
	}
	
	void data(const std::string& data_name, const PDI::Ref& ref)
	{
		if (PDI::Ref_w ref_w{ref}) {
			//can write to desc, put value to it
			*static_cast<float*>(ref_w.get()) = m_value;
			m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Desc `{}' = {}", m_name, data_name, m_value);
		}
		if (PDI::Ref_r ref_r{ref}) {
			//can read from desc, update m_value
			m_value = *static_cast<const float*>(ref_r.get());
			m_ctx.logger()->debug("(FlowVR) Float STAMP ({}): Stamp = {} from `{}'", m_name, m_value, data_name);
		}
	}
};

class Stamp_desc_string : public Stamp_base
{
	std::string m_value;
public:
	Stamp_desc_string(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::string data_desc):
		Stamp_base{ctx, parent_port, name}
	{
		if (!m_name.compare("source")) {
			m_stamp_info = &parent_port->stamps->source;
		} else {
			m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeString::create());
		}
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, data_desc));
		m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Created", m_name);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{
		std::string stamp_value;
		bool status = read_stamp.read(*m_stamp_info, stamp_value);
		if (status) {
			m_value = stamp_value;
			m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Update from message: Stamp = {}", m_name, stamp_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) Float STAMP ({}): Cannot read stamp value from message", m_name};
		}
	}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		bool status = write_stamp.write(*m_stamp_info, m_value);
		if (status) {
			m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Message update: Message.stamps.{} = {}", m_name, m_name, m_value);
		} else {
			throw PDI::Unavailable_error{"(FlowVR) String STAMP ({}): Cannot write stamp to message", m_name};
		}
	}
	
	void data(const std::string& data_name, const PDI::Ref& ref)
	{
		if (PDI::Ref_w ref_w{ref}) {
			//can write to desc, put value to it
			memcpy(ref_w.get(), m_value.c_str(), m_value.size() + 1); // +1 for '\0'
			m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Desc `{}' = {}", m_name, data_name, m_value);
		}
		if (PDI::Ref_r ref_r{ref}) {
			//can read from desc, update m_value
			m_value = static_cast<const char*>(ref_r.get());
			m_ctx.logger()->debug("(FlowVR) String STAMP ({}): Stamp = {} from `{}'", m_name, m_value, data_name);
		}
	}
};

class Stamp_desc_int_array : public Stamp_base
{
	std::vector<int> m_value;
public:
	Stamp_desc_int_array(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::string data_desc, long size):
		Stamp_base{ctx, parent_port, name},
		m_value(size)
	{
		m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(size, flowvr::TypeInt::create()));
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, data_desc));
		m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Created with size = {}", m_name, size);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{
		for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
			int stamp_value;
			bool status = read_stamp.read((*m_stamp_info)[stamp_id], stamp_value);
			if (status) {
				m_value[stamp_id] = stamp_value;
				m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Update from message: Stamp[{}] = {}", m_name, stamp_id, stamp_value);
			} else {
				throw PDI::Unavailable_error{"(FlowVR) Int array STAMP ({}): Cannot read stamp value from message. Index = {}", m_name, stamp_id};
			}
		}
	}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
			bool status = write_stamp.write((*m_stamp_info)[stamp_id], m_value[stamp_id]);
			if (status) {
				m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id]);
			} else {
				throw PDI::Unavailable_error{"(FlowVR) Int array STAMP ({}): Cannot write stamp to message", m_name};
			}
		}
	}
	
	void data(const std::string& data_name, const PDI::Ref& ref)
	{
		if (PDI::Ref_w ref_w{ref}) {
			//can write to desc, put value to it
			memcpy(ref_w.get(), m_value.data(), m_value.size() * sizeof(int));
			m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): `{}' update from stamp", m_name, data_name);
		}
		if (PDI::Ref_r ref_r{ref}) {
			//can read from desc, update m_value
			memcpy(m_value.data(), ref_r.get(), m_value.size() * sizeof(int));
			m_ctx.logger()->debug("(FlowVR) Int array STAMP ({}): Stamp update from `{}'", m_name, data_name);
		}
	}
};

class Stamp_desc_float_array : public Stamp_base
{
	std::vector<float> m_value;
public:
	Stamp_desc_float_array(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name, std::string data_desc, long size):
		Stamp_base{ctx, parent_port, name},
		m_value(size)
	{
		m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(size, flowvr::TypeFloat::create()));
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, data_desc));
		m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Created with size = {}", m_name, size);
	}
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override
	{
		for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
			float stamp_value;
			bool status = read_stamp.read((*m_stamp_info)[stamp_id], stamp_value);
			if (status) {
				m_value[stamp_id] = stamp_value;
				m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Update from message: Stamp[{}] = {}", m_name, stamp_id, stamp_value);
			} else {
				throw PDI::Unavailable_error{"(FlowVR) Int array STAMP ({}): Cannot read stamp value from message. Index = {}", m_name, stamp_id};
			}
		}
	}
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override
	{
		for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
			bool status = write_stamp.write((*m_stamp_info)[stamp_id], m_value[stamp_id]);
			if (status) {
				m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id]);
			} else {
				throw PDI::Unavailable_error{"(FlowVR) Float array STAMP ({}): Cannot write stamp to message", m_name};
			}
		}
	}
	
	void data(const std::string& data_name, const PDI::Ref& ref)
	{
		if (PDI::Ref_w ref_w{ref}) {
			//can write to desc, put value to it
			memcpy(ref_w.get(), m_value.data(), m_value.size() * sizeof(float));
			m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): `{}' update from stamp", m_name, data_name);
		}
		if (PDI::Ref_r ref_r{ref}) {
			//can read from desc, update m_value
			memcpy(m_value.data(), ref_r.get(), m_value.size() * sizeof(float));
			m_ctx.logger()->debug("(FlowVR) Float array STAMP ({}): Stamp update from `{}'", m_name, data_name);
		}
	}
};

} // namespace <anonymous>

#endif // PDI_FLOWVR_STAMP_DESC
