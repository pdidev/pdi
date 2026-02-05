/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
