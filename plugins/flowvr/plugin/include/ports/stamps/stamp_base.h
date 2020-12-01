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

#ifndef PDI_FLOWVR_STAMP_BASE
#define PDI_FLOWVR_STAMP_BASE

#include <functional>
#include <string>
#include <vector>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include <flowvr/module.h>
#include <flowvr/stamp.h>

namespace _flowvr_plugin {

/// Base class for stamps
class Stamp_base
{
protected:
	/// Context of this stamp
	PDI::Context& m_ctx;
	
	/// Port that contains this stamp
	const flowvr::Port* m_parent_port;
	
	/// Name of the stamp
	std::string m_name;
	
	/// Stamp doesn't own the stampInfo (flowvr module does)
	flowvr::StampInfo* m_stamp_info;
	
	/// Callbacks to remove on destruction
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/** Creates stamp base
	 * \param[in] ctx context of the stamp
	 * \param[in] parent_port port that contains this stamp
	 * \param[in] name name of the stamp
	 */
	Stamp_base(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name);
	
	/** Deleted copy constructor
	 * \param[in] other stamp to copy
	 */
	Stamp_base(const Stamp_base& other) = delete;
	
	/** Deleted move constructor
	 * \param[in] other stamp to move
	 */
	Stamp_base(Stamp_base&& other) = delete;
	
	/** Deleted copy operator
	 * \param[in] other stamp to copy
	 */
	Stamp_base& operator=(const Stamp_base& other) = delete;
	
	/** Deleted move operator
	 * \param[in] other stamp to move
	 */
	Stamp_base& operator=(Stamp_base&& other) = delete;
	
public:
	/** Updates stamp values from flowvr::Stamps
	 *
	 *  \param[in] read_stamp flowvr::Stamp with update values
	 */
	virtual void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) = 0;
	
	/** Put stamps to flowvr::StampsWrite
	 *
	 *  \param[in] write_stamp flowvr::StampWrite where to write stamp values
	 */
	virtual void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const = 0;
	
	/** Returns name of this stamp
	 *  \return name of this stamp
	 */
	std::string get_name() const;
	
	/** Returns flowvr::StampInfo of this stamp
	 *  \return flowvr::StampInfo of this stamp
	 */
	flowvr::StampInfo* get_stamp_info() const;
	
	/** Destroys stamp
	 */
	virtual ~Stamp_base();
	
}; // class Stamp_base

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_STAMP_BASE
