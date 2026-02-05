// SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "config.h"

#include <cassert>

#include "pdi/error.h"

#include "data_descriptor_impl.h"

#include "string_tools.h"

namespace PDI {

using std::string;
using std::vector;

vector<string> string_array_parse(const string& unescaped)
{
	vector<string> result(1);
	size_t start = 0;
	for (;;) {
		size_t end = unescaped.find_first_of("\\:", start);
		if (end >= unescaped.size()) {
			// we reached the end, let's add that part to the last element of the list and leave
			result.back().append(unescaped.substr(start));
			return result;
		} else if (unescaped[end] == ':') {
			// we reached a separator, let's add the part before it to the last element of the list and start a new element
			result.back().append(unescaped.substr(start, end - start));
			result.push_back("");
		} else if (unescaped[end] == '\\') {
			// we reached an escaping backslash, first let's add the unescaped part before it to the last element of the list and start a new element
			result.back().append(unescaped.substr(start, end - start));
			// then the actual behaviour depends on the next (escaped) char
			++end;
			if (end >= unescaped.size()) {
				// the end of string was escaped, this is an error
				throw Value_error("Unable to escape content ending in \\");
			} else if (unescaped[end] == '\\' || unescaped[end] == ':') {
				// we have an expected escaped char, let's add it raw to the last element of the list
				result.back().push_back(unescaped[end]);
			} else {
				// other characters can not be escaped
				throw Value_error("Unable to escape `{}'", (result.back() + unescaped[end]));
			}
		} else { // we should never reach this
			assert(!"Only '\\', ':' or end of string should be found");
		}
		start = end + 1;
	}
}

} // namespace PDI
