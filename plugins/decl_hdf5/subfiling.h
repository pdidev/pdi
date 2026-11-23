/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_SUBFILING_H_
#define DECL_HDF5_SUBFILING_H_

#include <cstdlib>
#include <optional>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace decl_hdf5 {

/** A Subfiling represents an HDF5 subfiling configuration on a file: 
 * number of subfiles, 
 * IOC strategy, etc.
 */
class Subfiling
{
public:
	/// Enumeration of different strategy for selecting I/O Concentrator processes across MPI ranks.
	enum class Ioc_select {
		OnePerNode, ///< number of IOC == number of node
		EveryNthRank, ///< number of IOC == (total number of MPI ranks / N)
		Total ///< number of IOC == Total
	};

private:
	/// The number of subfiles
	PDI::Expression m_sf_count = 0L;

	/// The stripe size for subfiling
	PDI::Expression m_sf_stripe_size = 0L;

	/// The policy for subfiling when problem
	PDI::Expression m_sf_policy = "STOP";

	/// The strategy for selecting I/O Concentrator processes across MPI ranks.
	std::optional<IocSelect> m_sf_ioc_selection;

	/// H5FD_SUBFILING_IOC_SELECTION_CRITERIA
	PDI::Expression m_sf_ioc_criteria = 1L;

public:
	/** The default constructor for an empty selection (everything selected)
	 */
	Subfiling() = default;

	/** Builds a subfiling from a yaml tree.
	 *
	 * The tree should be a mapping with two optional keys:
	 * - count: an expressions, or an integer value
	 * - stripe_size: an expressions, or an integer value
	 * - policy: an expression representing either CONTINUE or STOP
	 * - ioc_selection: a string/mapping specifying the IOC strategy
	 * \param tree the tree representing the subfiling.
	 */
	Subfiling(PC_tree_t tree);

	/** Accesses the number of subfiles that will be generated.
	 *
	 * \return the number of subfiles
	 */
	const PDI::Expression& count() const { return m_sf_count; }

	/** Accesses the stripe size of the subfiling configuration.
	 *
	 * \return The stripe size of the subfiling configuration
	 */
	const PDI::Expression& stripe_size() const { return m_sf_stripe_size; }

	/** Accesses the policy of subfiling configuration if problem.
	 *
	 * \return The policy of subfiling configuration if problem
	 */
	const PDI::Expression& policy() const { return m_sf_policy; }

	/** Accesses the ioc_selection of subfiling configuration.
	 *
	 * \return The IOC strategy of subfiling
	 */
	std::optional<IocSelect> ioc_selection() const { return m_sf_ioc_selection; }

	/** Accesses the H5FD_SUBFILING_IOC_SELECTION_CRITERIA of subfiling configuration.
	 *
	 * \return The H5FD_SUBFILING_IOC_SELECTION_CRITERIA of subfiling
	 */
	const PDI::Expression& ioc_criteria() const { return m_sf_ioc_criteria; }
};

/// Returns the value of an environment variable, or nullopt if it is not defined.
inline std::optional<std::string> get_env(const char* name)
{
	if (const char* raw = std::getenv(name)) {
		return std::string{raw};
	}
	return std::nullopt;
}

/// Sets the variable if value holds something, unsets it otherwise.
/// Returns true on success.
inline bool restore_env(const char* name, const std::optional<std::string>& value)
{
	if (value.has_value()) {
		return setenv(name, value->c_str(), 1) == 0;
	}
	return unsetenv(name) == 0;
}

/// Save the state of one environment variable on construction and
/// restore it on destruction, or
/// unset it on destruction if variable not defined.
class EnvGuard
{
public:
	explicit EnvGuard(const char* name)
		: m_name(name)
		, m_saved(get_env(name))
	{}

	~EnvGuard() { restore_env(m_name.c_str(), m_saved); }

	EnvGuard(const EnvGuard&) = delete;
	EnvGuard& operator= (const EnvGuard&) = delete;

	/// getter of m_saved, not used for now
	const std::optional<std::string>& saved() const { return m_saved; }

private:
	std::string m_name;
	std::optional<std::string> m_saved;
};

} // namespace decl_hdf5

#endif // DECL_HDF5_SELECTION_H_
