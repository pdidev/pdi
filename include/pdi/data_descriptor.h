/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_DATA_DESCRIPTOR_H_
#define PDI_DATA_DESCRIPTOR_H_

#include <functional>
#include <memory>
#include <stack>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/ref_any.h>
#include <pdi/datatype_template.h>


namespace PDI {

class PDI_EXPORT Data_descriptor
{
	friend class Context;
	
	struct PDI_NO_EXPORT Ref_holder;
	
	
	/// The context this descriptor is part of
	Context& m_context;
	
	/// References to the values of this descriptor
	std::stack<std::unique_ptr<Ref_holder>> m_refs;
	
	PC_tree_t m_config;
	
	Datatype_template_uptr m_type;
	
	const std::string m_name;
	
	bool m_metadata;
	
	
	/** Create an empty descriptor
	 */
	PDI_NO_EXPORT Data_descriptor(Context& ctx, const char* name);
	
	Data_descriptor(const Data_descriptor&) = delete;
	
	Data_descriptor& operator= (const Data_descriptor&) = delete;
	
	Data_descriptor& operator= (Data_descriptor&&) = delete;
	
public:
	Data_descriptor(Data_descriptor&&);
	
	~Data_descriptor();
	
	/** Sets the creation template used to type raw pointers shared through this descriptor
	 * \param config the full configuration attached to the descriptor
	 */
	void creation_template(PC_tree_t config);
	
	/** Returns the PC_tree_t config
	 * \todo remove this and attach this config to the type
	 * \return the full configuration attached to the descriptor
	 */
	PC_tree_t config() const;
	
	/** Return true if the data is a metadata
	 */
	bool metadata() const;
	
	/** Sets whether this describes a metadata or not
	 * \param metadata whether data shared through this descriptor should behave as a metadata
	 */
	void metadata(bool metadata);
	
	const std::string& name() const;
	
	/** Return a reference to the value of the data behind this descriptor
	 */
	Ref ref();
	
	/** Shares some data with PDI
	 * \param[in,out] data the shared data
	 * \param read whether read access is granted to other references
	 * \param write whether write access is granted to other references
	 */
	void share(void* data, bool read, bool write);
	
	/** Shares some data with PDI
	 * \param[in,out] ref a reference to the shared data
	 * \param read whether the stored reference should have read access
	 * \param write whether write access is granted to other references
	 * \return the just shared buffer
	 */
	void* share(Ref ref, bool read, bool write);
	
	/** Releases ownership of a data shared with PDI. PDI is then responsible to
	 * free the associated memory whenever necessary.
	 */
	void release();
	
	/** Reclaims ownership of a data buffer shared with PDI. PDI is then responsible to
	 * free the associated memory whenever necessary.
	 */
	void reclaim();
	
}; // class Data_descriptor

} // namespace PDI

#endif // PDI_DATA_DESCRIPTOR_H_
