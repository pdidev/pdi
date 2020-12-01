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

#ifndef PDI_FLOWVR_STAMP_DESC_FLOAT
#define PDI_FLOWVR_STAMP_DESC_FLOAT

#include <string>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include <flowvr/module.h>
#include <flowvr/stamp.h>

#include "../stamp_base.h"

namespace _flowvr_plugin {

/// Descriptor float stamp
class Stamp_desc_float : public Stamp_base
{
	/// Value of the stamp
	float m_value;
	
public:
	/** Creates float descriptor stamp
	 * \param[in] ctx context of the stamp
	 * \param[in] parent_port port that contains this stamp
	 * \param[in] name name of the stamp
	 * \param[in] data_desc name of stamp descriptor
	 */
	Stamp_desc_float(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, const std::string& data_desc);
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override;
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override;
	
	/** Depending on right access on Ref, read/write stamp value from/to reference
	 *
	 * \param[in] data_name name of shared data
	 * \param[in] ref reference to shared data
	 */
	void data(const std::string& data_name, const PDI::Ref& ref);
	
}; // Stamp_desc_float

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_STAMP_DESC_FLOAT
