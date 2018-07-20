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

#ifndef PDI_GLOBAL_CONTEXT_H_
#define PDI_GLOBAL_CONTEXT_H_

#include <mpi.h>

#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/pdi_fwd.h>
#include <pdi/data_descriptor.h>
#include <pdi/ref_any.h>


namespace PDI {

class PDI_EXPORT Global_context : public Context
{
private:
	friend class Data_descriptor_impl;
	
	/// Global logger of PDI
	Logger_sptr m_logger;
	
	/// Descriptors of the data
	std::unordered_map<std::string, std::unique_ptr<Data_descriptor>> m_descriptors;
	
	/// The loaded plugins - need to be after m_descriptors (to guarantee proper destroy order)
	std::unordered_map<std::string, std::unique_ptr<Plugin>> m_plugins;
	
	Global_context(const Global_context&) = delete;
	
	Global_context(Global_context&&) = delete;
	
public:
	Global_context(PC_tree_t conf, MPI_Comm* world);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const std::string& name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const char* name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[](const std::string& name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[](const char* name) override;
	
	/** Returns an iterator on the first descriptor
	 */
	Iterator begin() override;
	
	/** Returns an iterator past the last descriptor
	 */
	Iterator end() override;
	
	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	void event(const char* name) override;
	
	Logger_sptr logger() const override;
	
};

} // namespace PDI

#endif // PDI_GLOBAL_CONTEXT_H_
