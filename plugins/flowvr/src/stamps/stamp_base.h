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

#ifndef PDI_FLOWVR_STAMP_BASE
#define PDI_FLOWVR_STAMP_BASE

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

class Stamp_base
{
protected:
	PDI::Context& m_ctx;
	const flowvr::Port* m_parent_port; // the pointer to the port that holds this stamp
	
	std::string m_name; //name of the stamp
	
	flowvr::StampInfo* m_stamp_info; //value doesn't own the stampInfo - flowvr module does
	
	Stamp_base(PDI::Context& ctx, const flowvr::Port* parent_port, std::string name):
		m_ctx{ctx},
		m_parent_port{parent_port},
		m_name{std::move(name)}
	{}
	
	Stamp_base(const Stamp_base& other) = delete;
	
	Stamp_base(Stamp_base&& other) = delete;
	
	Stamp_base& operator = (const Stamp_base& other) = delete;
	
	Stamp_base& operator = (Stamp_base&& other) = delete;
	
public:
	/**
	 *  Updates stamp values from flowvr::Stamps
	 *
	 *  \param[in] read_stamp flowvr::Stamp with update values
	 */
	virtual void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) = 0;
	
	/**
	 *  Put stamps to flowvr::StampsWrite
	 *
	 *  \param[in] write_stamp flowvr::StampWrite where to write stamp values
	 */
	virtual void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const = 0;
	
	/**
	 *  Called if user accessing data descriptor. Write to or read from it.
	 *
	 *  \param[in] data_name descriptor name
	 */
	virtual bool data(const char* data_name, const PDI::Ref& ref) = 0;
	
	/**
	 *  \return name of this stamp
	 */
	std::string get_name() const
	{
		return m_name;
	}
	
	/**
	 *  \return flowvr::StampInfo of this stamp
	 */
	flowvr::StampInfo* get_stamp_info() const
	{
		return m_stamp_info;
	}
}; // class Value

} // namespace <anonymous>

#endif // PDI_FLOWVR_STAMP_BASE