/*******************************************************************************
 * Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_DATASET_EXPLICIT_TYPE_H_
#define DECL_HDF5_DATASET_EXPLICIT_TYPE_H_

// #include <hdf5.h>
// #ifdef H5_HAVE_PARALLEL
// #include <mpi.h>
// #endif

#include <regex>
#include <string>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>

//#include <pdi/context.h>
//#include <pdi/expression.h>

namespace decl_hdf5 {

/// Information about the types that should be used to create datasets as provided in the Yaml file
class Dataset_explicit_type
{
    /// definition from the YAML as a string for debugging purpose
	std::string m_definition;
	
	///< the parsed regex that determines if the provided type applies (depend only on m_definition and regex grammar)
	std::regex m_regex;
	
	/// begin line number in the YAML for debugging purposes
	int m_begin_line;
	
	/// end line number in the YAML for debugging purposes
	int m_end_line;
	
	///< the type to use for the dataset in case the regex matches
	PDI::Datatype_template_sptr m_type;

public:
	Dataset_explicit_type() = default;

	Dataset_explicit_type(std::string def, int b_line, int e_line, std::regex regex, PDI::Datatype_template_sptr type)
		: m_definition(def)
		, m_begin_line(b_line)
		, m_end_line(e_line)
		, m_regex(regex)
		, m_type(type)
	{}

	/// function to get the line where the dataset is defined in Yaml file
	std::string get_msg_err_line() const
	{
		std::string result;
		if (m_begin_line == m_end_line) {
			result = " defined in line " + std::to_string(m_begin_line + 1);
		} else {
			result = " defined in lines " + std::to_string(m_begin_line + 1) + " - " + std::to_string(m_end_line);
		}
		return result;
	}

	/** Accesses the name given in Yaml file to define the dataset
	 *
	 * \return the name given in Yaml file to define the dataset
	 */
	std::string definition() const { return m_definition; }

	/** Accesses the regex corresponding to this dataset
	 * 
	 * \return the regex corresponding to this dataset
	 */
	std::regex regex() const { return m_regex; }

	/** Accesses the type to use for the dataset
	 * 
	 * \return the type to use for the dataset
	 */
	PDI::Datatype_template_sptr type() const { return m_type; }
};

} // namespace decl_hdf5

#endif // DECL_HDF5_DATASET_EXPLICIT_TYPE_H_
