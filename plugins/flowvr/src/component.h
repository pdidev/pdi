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

#ifndef PDI_FLOWVR_COMPONENT
#define PDI_FLOWVR_COMPONENT

#include <pdi/context.h>
#include <pdi/ref_any.h>

namespace  {

class Component
{
	PDI::Context& m_context;
	
public:
	/** Notification for a named event
	 * \param[in] event the event name
	 */
	virtual void event(const char* event_name) {};
	
	/** Notification that some data becomes available
	 * \param[in] name the name of the data made available
	 * \param[in] ref a reference to the data value
	 */
	virtual void data(const char* data_name, const PDI::Ref& ref) {};
	
	/** Notification for accessing empty desc by user
	 * \param[in] name the name of accessing desc
	 */
	virtual void empty_desc_access(const char* data_name) {};
	
	Component(PDI::Context& ctx):
		m_context{ctx}
	{}
	
	Component(const Component& other) = delete;
	
	Component(Component&& other):
		m_context{other.m_context}
	{}
	
	Component& operator = (const Component& other) = delete;
	
	Component& operator = (Component&& other)
	{
		m_context = other.m_context;
		return *this;
	}
	
	PDI::Context& context()
	{
		return m_context;
	}
	
	virtual ~Component () = default;
};

} // namespace <anonymous>

#endif // PDI_FLOWVR_COMPONENT