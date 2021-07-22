/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef DECL_HDF5_COLLISION_POLICY_H_
#define DECL_HDF5_COLLISION_POLICY_H_

#include <cstdint>
#include <ostream>

namespace decl_hdf5 {

enum class Collision_policy :uint8_t {
	WARNING = 1,    // 00001b
	SKIP = 2,       // 00010b
	REPLACE = 4,    // 00100b
	WRITE_INTO = 8, // 01000b
	ERROR = 16      // 10000b
};

/** OR operator on Collision policies
 *
 * \param a first collision policy
 * \param b second collision policy
 *
 * \return result of OR operation
 */
Collision_policy operator|(Collision_policy a, Collision_policy b);

/** AND operator on Collision policies
 *
 * \param a first collision policy
 * \param b second collision policy
 *
 * \return result of AND operation as bool value
 */
bool operator&(Collision_policy a, Collision_policy b);

/** << operator of Collision policies
 *
 * \param os output stream
 * \param policy policy value to output
 *
 * \return os output stream
 */
std::ostream& operator<<(std::ostream& os, Collision_policy policy);

/** Converts string policy to Collision_policy enum
 *
 * \param collision_policy collision policy string to convert
 *
 * \return Collision_policy result
 */
Collision_policy to_collision_policy(std::string collision_policy);

} // namespace decl_hdf5

#endif // DECL_HDF5_COLLISION_POLICY_H_
