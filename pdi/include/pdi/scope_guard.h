/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_SCOPE_GUARD_H_
#define PDI_SCOPE_GUARD_H_

#include <stdexcept>

namespace PDI {

/** A ScopeGuard ensures PDI_init() and PDI_finalize() are always called
 * in a matched pair, even in the presence of exceptions or early returns.
 *
 * Inspired by Kokkos::ScopeGuard.
 *
 * Typical usage:
 * \code
 * int main(int argc, char* argv[]) {
 *     MPI_Init(&argc, &argv);
 *     PC_tree_t conf = PC_parse_path("config.yml");
 *     {
 *         PDI::ScopeGuard pdi_guard(conf);
 *         // PDI is live here; PDI_finalize() is guaranteed on scope exit
 *     }
 *     PC_tree_destroy(&conf);
 *     MPI_Finalize();
 * }
 * \endcode
 */
class ScopeGuard
{
public:
	/** Initializes PDI with the given configuration tree.
	 *
	 * \param conf the paraconf configuration tree passed to PDI_init()
	 * \throws std::runtime_error if PDI_init() returns an error status
	 */
	[[nodiscard]] ScopeGuard(PC_tree_t conf)
	{
		PDI_status_t status = PDI_init(conf);
		if (status != PDI_OK) {
			throw std::runtime_error("PDI_init() failed");
		}
	}

	/** Finalizes PDI by calling PDI_finalize().
	 */
	~ScopeGuard() noexcept { PDI_finalize(); }

	ScopeGuard(const ScopeGuard&) = delete;
	ScopeGuard(ScopeGuard&&) = delete;
	ScopeGuard& operator= (const ScopeGuard&) = delete;
	ScopeGuard& operator= (ScopeGuard&&) = delete;
};

} // namespace PDI

#endif // PDI_SCOPE_GUARD_H_
