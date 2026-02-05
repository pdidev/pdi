/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef DECL_HDF5_COLLISION_POLICY_H_
#define DECL_HDF5_COLLISION_POLICY_H_

#include <cstdint>
#include <ostream>

namespace decl_hdf5 {

enum class Collision_policy : uint8_t {
	WARNING = 1, // 00001b
	SKIP = 2, // 00010b
	REPLACE = 4, // 00100b
	WRITE_INTO = 8, // 01000b
	ERROR = 16 // 10000b
};

/** OR operator on Collision policies
 *
 * \param a first collision policy
 * \param b second collision policy
 *
 * \return result of OR operation
 */
Collision_policy operator| (Collision_policy a, Collision_policy b);

/** AND operator on Collision policies
 *
 * \param a first collision policy
 * \param b second collision policy
 *
 * \return result of AND operation as bool value
 */
bool operator& (Collision_policy a, Collision_policy b);

/** << operator of Collision policies
 *
 * \param os output stream
 * \param policy policy value to output
 *
 * \return os output stream
 */
std::ostream& operator<< (std::ostream& os, Collision_policy policy);

/** Converts string policy to Collision_policy enum
 *
 * \param collision_policy collision policy string to convert
 *
 * \return Collision_policy result
 */
Collision_policy to_collision_policy(std::string collision_policy);

} // namespace decl_hdf5

#endif // DECL_HDF5_COLLISION_POLICY_H_
