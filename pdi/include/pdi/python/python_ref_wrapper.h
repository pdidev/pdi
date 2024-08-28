/*******************************************************************************
* Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_PYTHON_REF_WRAPPER
#define PDI_PYTHON_REF_WRAPPER

#include <cstddef>
#include <string>

#include <pybind11/pybind11.h>

#include <pdi/ref_any.h>

namespace PDI {

class PDI_EXPORT Python_ref_wrapper
{
	/// Wrapped reference
	Ref m_ref;

public:
	/** Creates python reference wrapper
	 * \param ref reference to wrap
	 */
	Python_ref_wrapper(Ref ref);

	/** Gets a member if ref has a record datatype
	 * \param member_name name of the member to get
	 * \return Python_ref_wrapper if member has record type inside, numpy array otherwise
	 */
	pybind11::object getattribute(std::string member_name);

	/** Sets a member value if ref has a record datatype
	 * \param member_name name of the member to set
	 * \param value value to set to member
	 */
	void setattribute(std::string member_name, const pybind11::object value);

	/** Gets element of the array if ref has a array datatype
	 * \param index index of element to get
	 * \return Python_ref_wrapper if member has record type inside, numpy array otherwise
	 */
	pybind11::object getitem(size_t index);
};

} // namespace PDI

#endif // PDI_PYTHON_REF_WRAPPER
