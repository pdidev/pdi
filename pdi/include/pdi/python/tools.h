/*
 * SPDX-FileCopyrightText: 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_PYTHON_TOOLS_H_
#define PDI_PYTHON_TOOLS_H_

#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

#include <pdi/pdi_fwd.h>
#include <pdi/ref_any.h>

namespace PDI {

/** Wraps a PDI reference in a python object
 *
 * \param r PDI reference to wrap
 * \param force_const whether the object should be const even if the ref can be write accessed
 * \return python object wrapper for the PDI reference
 */
pybind11::object PDI_EXPORT to_python(Ref r, bool force_const = false);

/** Function takes python numpy array and converts it into PDI datatype
 *
 * \param a python numpy array data
 * \return PDI datatype of python object
 */
Datatype_sptr PDI_EXPORT python_type(const pybind11::array& a);

/** Convert a Scalar_datatype to a pybind11 data type.
 *
 * \param scalar_type to convert to pybind11 data type
 * \return pybind11 data type for the given scalar type
 */
pybind11::dtype PDI_EXPORT to_python(const std::shared_ptr<const Scalar_datatype>& scalar_type);

} // namespace PDI

#endif // PDI_PYTHON_TOOLS_H_
