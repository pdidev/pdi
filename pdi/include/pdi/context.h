/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_CONTEXT_H_
#define PDI_CONTEXT_H_

#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>

#include <pdi/pdi_fwd.h>
#include <pdi/callbacks.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>
#include <pdi/logger.h>
#include <pdi/ref_any.h>

namespace PDI {

class PDI_EXPORT Context
{
public:
	virtual ~Context();

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Ref operator[] (std::string_view name) const { return data(std::move(name)); }

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Ref data(std::string_view name) const = 0;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Ref local_data(std::string_view name) const = 0;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Ref global_data(std::string_view name) const = 0;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Datatype_template_sptr datatype(std::string_view name) const = 0;

	/** Shares some data with PDI
	 * \param ref a reference to the shared data
	 * \param read whether the stored reference should have read access
	 * \param write whether the stored reference should have write access
	 * \return the just shared buffer
	 */
	virtual void* share(std::string_view name, Ref ref, bool read, bool write) = 0;

	/** Releases ownership of a data shared with PDI. PDI is then responsible to
	 * free the associated memory whenever necessary.
	 */
	virtual void release() = 0;

	/** Reclaims ownership of a data buffer shared with PDI. PDI does not manage
	 * the buffer memory anymore.
	 * \return the address of the buffer
	 */
	virtual void* reclaim() = 0;

	/** Creates a new datatype template from a paraconf-style config
	 * \param[in] node the configuration to read
	 *
	 * \return the type generated
	 */
	virtual Datatype_template_sptr datatype(PC_tree_t node) = 0;

	/** Adds new datatype to the context
	 *
	 * \param[in] name name of the datatype to add
	 * \param[in] parser function that creates new datatype_template from PC_tree_t
	 */
	virtual void add_datatype(const std::string& name, Datatype_template_sptr type) = 0;
};

} // namespace PDI

#endif // PDI_CONTEXT_H_
