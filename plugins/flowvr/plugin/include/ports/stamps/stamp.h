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

#ifndef PDI_FLOWVR_STAMP
#define PDI_FLOWVR_STAMP

#include <memory>
#include <string>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include <flowvr/module.h>
#include <flowvr/stamp.h>

#include "desc/stamp_desc_int.h"
#include "desc/stamp_desc_float.h"
#include "desc/stamp_desc_string.h"
#include "desc/stamp_desc_int_array.h"
#include "desc/stamp_desc_float_array.h"

#include "expr/stamp_expr_int.h"
#include "expr/stamp_expr_float.h"
#include "expr/stamp_expr_string.h"
#include "expr/stamp_expr_int_array.h"
#include "expr/stamp_expr_float_array.h"

#include "stamp_base.h"

namespace _flowvr_plugin {

/// Stamp used in ports
class Stamp
{
	/// Context of this stamp
	PDI::Context& m_ctx;
	
	/// Stamp holder
	std::unique_ptr<Stamp_base> m_stamp;
	
	/** Sets up stamp as descriptor
	 *
	 *  \param[in] parent_port the pointer to the port that holds this stamp
	 *  \param[in] name name of this stamp
	 *  \param[in] data_desc descriptor of the stamp
	 */
	void load_stamp_desc_type(const flowvr::Port* parent_port, const std::string& name, const std::string& data_desc);
	
	/** Sets up stamp as expression
	 *
	 * \param[in] parent_port the pointer to the port that holds this stamp
	 * \param[in] name name of this stamp
	 * \param[in] expr_node expression node of stamp definition
	 * \param[in] type_node type node of stamp definition
	 */
	void load_stamp_expr(const flowvr::Port* parent_port, const std::string& name, PC_tree_t expr_node, PC_tree_t type_node);
	
public:
	/** Creates new stamp
	 *
	 * \param[in] ctx context of this stamp
	 * \param[in] parent_port the pointer to the port that holds this stamp
	 * \param[in] name name of this stamp
	 * \param[in] stamp_node config node of this stamp
	 */
	Stamp(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, PC_tree_t stamp_node);
	
	/** Deleted copy constructor
	 * \param[in] other stamp to copy
	 */
	Stamp(const Stamp& other) = delete;
	
	/** Move constructor
	 * \param[in] other stamp to move
	 */
	Stamp(Stamp&& other);
	
	/** Deleted copy operator
	 * \param[in] other stamp to copy
	 */
	Stamp& operator=(const Stamp& other) = delete;
	
	/** Move operator
	 * \param[in] other stamp to move
	 */
	Stamp& operator=(Stamp&& other);
	
	/**
	 *  Updates stamp values from flowvr::Stamps
	 *
	 *  \param[in] read_stamp flowvr::Stamp with update values
	 */
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp);
	
	/** Put stamps to flowvr::StampsWrite
	 *
	 *  \param[in] write_stamp flowvr::StampWrite where to write stamp values
	 */
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const;
	
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
	~Stamp();
	
}; // class Stamp

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_STAMP
