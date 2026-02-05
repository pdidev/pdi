// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <algorithm>

#include <pdi/error.h>

#include "collision_policy.h"

using PDI::Value_error;
using std::ostream;
using std::string;

namespace decl_hdf5 {

Collision_policy operator| (Collision_policy a, Collision_policy b)
{
	return static_cast<Collision_policy>(static_cast<uint8_t>(a) | static_cast<uint8_t>(b));
}

bool operator& (Collision_policy a, Collision_policy b)
{
	return static_cast<bool>(static_cast<uint8_t>(a) & static_cast<uint8_t>(b));
}

ostream& operator<< (ostream& os, Collision_policy policy)
{
	if (policy & Collision_policy::WARNING) {
		os << "WARNING";
	}
	if (policy & Collision_policy::SKIP) {
		os << "SKIP";
	}
	if (policy & Collision_policy::REPLACE) {
		os << "REPLACE";
	}
	if (policy & Collision_policy::WRITE_INTO) {
		os << "WRITE_INTO";
	}
	if (policy & Collision_policy::ERROR) {
		os << "ERROR";
	}
	return os;
}

Collision_policy to_collision_policy(string collision_policy)
{
	std::transform(collision_policy.begin(), collision_policy.end(), collision_policy.begin(), [](unsigned char c) { return std::toupper(c); });
	if (collision_policy == "SKIP") {
		return Collision_policy::SKIP;
	} else if (collision_policy == "SKIP_AND_WARN") {
		return Collision_policy::SKIP | Collision_policy::WARNING;
	} else if (collision_policy == "REPLACE") {
		return Collision_policy::REPLACE;
	} else if (collision_policy == "REPLACE_AND_WARN") {
		return Collision_policy::REPLACE | Collision_policy::WARNING;
	} else if (collision_policy == "WRITE_INTO") {
		return Collision_policy::WRITE_INTO;
	} else if (collision_policy == "WRITE_INTO_AND_WARN") {
		return Collision_policy::WRITE_INTO | Collision_policy::WARNING;
	} else if (collision_policy == "ERROR") {
		return Collision_policy::ERROR;
	} else {
		throw Value_error{"Unknown collision policy: {}", collision_policy};
	}
}

} // namespace decl_hdf5
