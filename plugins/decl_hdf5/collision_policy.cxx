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

#include <pdi/error.h>

#include "collision_policy.h"

using PDI::Value_error;
using std::ostream;
using std::string;

namespace decl_hdf5 {

Collision_policy operator|(Collision_policy a, Collision_policy b)
{
	return static_cast<Collision_policy>(static_cast<uint8_t>(a) | static_cast<uint8_t>(b));
}

bool operator&(Collision_policy a, Collision_policy b)
{
	return static_cast<bool>(static_cast<uint8_t>(a) & static_cast<uint8_t>(b));
}

ostream& operator<<(ostream& os, Collision_policy policy)
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
	std::transform(collision_policy.begin(), collision_policy.end(), collision_policy.begin(),
	[](unsigned char c) {
		return std::toupper(c);
	}
	);
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
