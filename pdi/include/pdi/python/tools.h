/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

} // namespace PDI

#endif // PDI_PYTHON_TOOLS_H_
