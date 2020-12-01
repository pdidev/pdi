/*******************************************************************************
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

#include <pdi/error.h>
#include <spdlog/spdlog.h>

#include "ports/stamps/desc/stamp_desc_int_array.h"

namespace _flowvr_plugin {

Stamp_desc_int_array::Stamp_desc_int_array(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, const std::string& data_desc, long size):
	Stamp_base{ctx, parent_port, name},
	m_value(size)
{
	m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(size, flowvr::TypeInt::create()));
	m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
		this->data(name, ref);
	}, data_desc));
	m_ctx.logger()->debug("{} stamp: Int array descriptor created with size = {}", m_name, size);
}

void Stamp_desc_int_array::read_from_flowvr_stamp(const flowvr::Stamps& read_stamp)
{
	for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
		int stamp_value;
		bool status = read_stamp.read((*m_stamp_info)[stamp_id], stamp_value);
		if (status) {
			m_value[stamp_id] = stamp_value;
			m_ctx.logger()->trace("{} stamp: Update from message: Stamp[{}] = {}", m_name, stamp_id, stamp_value);
		} else {
			throw PDI::Unavailable_error{"{} stamp: Cannot read stamp value from message. Index = {}", m_name, stamp_id};
		}
	}
}

void Stamp_desc_int_array::write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const
{
	for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
		bool status = write_stamp.write((*m_stamp_info)[stamp_id], m_value[stamp_id]);
		if (status) {
			m_ctx.logger()->trace("{} stamp: Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id]);
		} else {
			throw PDI::Unavailable_error{"{} stamp: Cannot write stamp to message", m_name};
		}
	}
}

void Stamp_desc_int_array::data(const std::string& data_name, const PDI::Ref& ref)
{
	if (PDI::Ref_w ref_w{ref}) {
		//can write to desc, put value to it
		memcpy(ref_w.get(), m_value.data(), m_value.size() * sizeof(int));
		m_ctx.logger()->trace("{} stamp: `{}' update from stamp", m_name, data_name);
	}
	if (PDI::Ref_r ref_r{ref}) {
		//can read from desc, update m_value
		memcpy(m_value.data(), ref_r.get(), m_value.size() * sizeof(int));
		m_ctx.logger()->trace("{} stamp: Stamp update from `{}'", m_name, data_name);
	}
}

} // namespace _flowvr_plugin
