/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <algorithm>

#include <pdi/array_datatype.h>
#include <pdi/python/tools.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include "pdi/python/python_ref_wrapper.h"

namespace PDI {

using std::dynamic_pointer_cast;

Python_ref_wrapper::Python_ref_wrapper(Ref ref)
	: m_ref{ref}
{}

pybind11::object Python_ref_wrapper::getattribute(std::string member_name)
{
	return to_python(m_ref[member_name]);
}

void Python_ref_wrapper::setattribute(std::string member_name, const pybind11::object value)
{
	Ref subref = m_ref[member_name];
	if (Ref_w subref_w{subref}) {
		if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(subref_w.type())) {
			switch (scalar_type->kind()) {
			case Scalar_kind::FLOAT: {
				switch (scalar_type->datasize()) {
				case sizeof(float): {
					float float_data = value.cast<float>();
					memcpy(subref_w.get(), &float_data, sizeof(float));
					break;
				}
				case sizeof(double): {
					double double_data = value.cast<double>();
					memcpy(subref_w.get(), &double_data, sizeof(double));
					break;
				}
				default:
					throw Type_error{"Unable to pass {} bytes floating point value to python {}", scalar_type->datasize(), sizeof(float)};
				}
			} break;
			case Scalar_kind::SIGNED: {
				switch (scalar_type->datasize()) {
				case sizeof(int8_t): {
					int8_t int8_t_data = value.cast<int8_t>();
					memcpy(subref_w.get(), &int8_t_data, sizeof(int8_t));
					break;
				}
				case sizeof(int16_t): {
					int16_t int16_t_data = value.cast<int16_t>();
					memcpy(subref_w.get(), &int16_t_data, sizeof(int16_t));
					break;
				}
				case sizeof(int32_t): {
					int32_t int32_t_data = value.cast<int32_t>();
					memcpy(subref_w.get(), &int32_t_data, sizeof(int32_t));
					break;
				}
				case sizeof(int64_t): {
					int64_t int64_t_data = value.cast<int64_t>();
					memcpy(subref_w.get(), &int64_t_data, sizeof(int64_t));
					break;
				}
				default:
					throw Type_error{"Unable to pass {} bytes integer value to python", scalar_type->datasize()};
				}
			} break;
			case Scalar_kind::UNSIGNED: {
				switch (scalar_type->datasize()) {
				case sizeof(uint8_t): {
					uint8_t uint8_t_data = value.cast<uint8_t>();
					memcpy(subref_w.get(), &uint8_t_data, sizeof(uint8_t));
					break;
				}
				case sizeof(uint16_t): {
					uint16_t uint16_t_data = value.cast<uint16_t>();
					memcpy(subref_w.get(), &uint16_t_data, sizeof(uint16_t));
					break;
				}
				case sizeof(uint32_t): {
					uint32_t uint32_t_data = value.cast<uint32_t>();
					memcpy(subref_w.get(), &uint32_t_data, sizeof(uint32_t));
					break;
				}
				case sizeof(uint64_t): {
					uint64_t float_data = value.cast<uint64_t>();
					memcpy(subref_w.get(), &float_data, sizeof(uint64_t));
					break;
				}
				default:
					throw Type_error{"Unable to pass {} bytes unsigned integer value to python", scalar_type->datasize()};
				}
			} break;
			default:
				throw Type_error{"Unable to pass value of unexpected type to python"};
			}
		} else if (auto&& array_type = std::dynamic_pointer_cast<const Array_datatype>(subref_w.type())) {
			const pybind11::array py_array{value};
			Datatype_sptr py_type = python_type(py_array);
			if (const Array_datatype* array_py_type = dynamic_cast<const Array_datatype*>(py_type.get())) {
				if (array_py_type->subtype()->buffersize() != array_type->subtype()->buffersize()) {
					throw Type_error{"Setting a member ({}) of array type with subtype different than int64 is unsupported", member_name};
				}
			}
			memcpy(subref_w.get(), py_array.data(), array_type->buffersize());
		} else {
			throw Type_error{"Setting a member ({}) of record type is unsupported", member_name};
		}
	} else {
		throw Right_error{"Cannot set member that is read only: {}", member_name};
	}
}

pybind11::object Python_ref_wrapper::getitem(size_t index)
{
	return to_python(m_ref[index]);
}

} // namespace PDI
