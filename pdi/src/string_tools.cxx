/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "config.h"

#include <cassert>

#include "pdi/error.h"

#include "data_descriptor_impl.h"

#include "string_tools.h"


namespace PDI {

using std::string;
using std::vector;


vector<string> string_array_parse(const string& unescaped) {
	vector<string> result(1);
	size_t start = 0;
	for(;;) {
		size_t end = unescaped.find_first_of("\\:", start);
		if ( end >= unescaped.size() ) {
			// we reached the end, let's add that part to the last element of the list and leave
			result.back().append(unescaped.substr(start));
			return result;
		} else if ( unescaped[end] == ':' ) {
			// we reached a separator, let's add the part before it to the last element of the list and start a new element
			result.back().append(unescaped.substr(start, end-start));
			result.push_back("");
		} else if ( unescaped[end] == '\\' ) {
			// we reached an escaping backslash, first let's add the unescaped part before it to the last element of the list and start a new element 
			result.back().append(unescaped.substr(start, end-start));
			// then the actual behaviour depends on the next (escaped) char
			++end;
			if ( end >= unescaped.size() ) {
				// the end of string was escaped, this is an error
				throw Config_error("Unable to escape content ending in \\");
			} else if (unescaped[end] == '\\' || unescaped[end] == ':' ) {
				// we have an expected escaped char, let's add it raw to the last element of the list
				result.back().push_back(unescaped[end]);
			} else {
				// other characters can not be escaped
				throw Config_error("Unable to escape `{}'", (result.back()+unescaped[end]));
			}
		} else { // we should never reach this
			assert(!"Only '\\', ':' or end of string should be found");
		}
		start = end+1;
	}
}

}  // namespace PDI
