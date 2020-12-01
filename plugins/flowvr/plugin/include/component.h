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

#ifndef PDI_FLOWVR_COMPONENT
#define PDI_FLOWVR_COMPONENT

#include <pdi/context.h>

namespace _flowvr_plugin {

/// Base class for flowvr components
class Component
{
	/// Context of this component
	PDI::Context& m_context;
	
public:
	/** Creates new component
	 * \param[in] ctx context of this component
	 */
	Component(PDI::Context& ctx);
	
	/** Copy constructor
	 * \param[in] other component to copy
	 */
	Component(const Component& other);
	
	/** Deleted move constructor
	 * \param[in] other component to move
	 */
	Component(Component&& other) = delete;
	
	/** Copy operator
	 * \param[in] other component to copy
	 */
	Component& operator=(const Component& other);
	
	/** Deleted move operator
	 * \param[in] other component to move
	 */
	Component& operator=(Component&& other) = delete;
	
	/** Returns context of the component
	 * \return context of the component
	 */
	PDI::Context& context();
	
	/** Destroys component
	 */
	virtual ~Component ();
};

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_COMPONENT