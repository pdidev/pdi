/*******************************************************************************
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include "ports/stamps/desc/stamp_desc_int.h"

namespace _flowvr_plugin {

Stamp_desc_int::Stamp_desc_int(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, const std::string& data_desc):
	Stamp_base{ctx, parent_port, name}
{
	if (!m_name.compare("it")) {
		m_stamp_info = &m_parent_port->stamps->it;
	} else if (!name.compare("num")) {
		m_stamp_info = &m_parent_port->stamps->num;
	} else {
		m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeInt::create());
	}
	m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
		this->data(name, ref);
	}, data_desc));
	m_ctx.logger().debug("{} stamp: Int descriptor created", m_name);
}

void Stamp_desc_int::read_from_flowvr_stamp(const flowvr::Stamps& read_stamp)
{
	int stamp_value;
	bool status = read_stamp.read(*m_stamp_info, stamp_value);
	if (status) {
		m_value = stamp_value;
		m_ctx.logger().trace("{} stamp: Update from message: Stamp = {}", m_name, stamp_value);
	} else {
		throw PDI::Unavailable_error{"{} stamp: Cannot read stamp value from message", m_name};
	}
}

void Stamp_desc_int::write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const
{
	bool status = write_stamp.write(*m_stamp_info, m_value);
	if (status) {
		m_ctx.logger().trace("{} stamp: Message update: Message.stamps.{} = {}", m_name, m_name, m_value);
	} else {
		throw PDI::Unavailable_error{"{} stamp: Cannot write stamp to message", m_name};
	}
}

void Stamp_desc_int::data(const std::string& data_name, const PDI::Ref& ref)
{
	if (PDI::Ref_w ref_w{ref}) {
		//can write to desc, put value to it
		*static_cast<int*>(ref_w.get()) = m_value;
		m_ctx.logger().trace("{} stamp: Desc `{}' = {}", m_name, data_name, m_value);
	}
	if (PDI::Ref_r ref_r{ref}) {
		//can read from desc, update m_value
		m_value = *static_cast<const int*>(ref_r.get());
		m_ctx.logger().trace("{} stamp: Stamp = {} from `{}'", m_name, m_value, data_name);
	}
}

} // namespace _flowvr_plugin
