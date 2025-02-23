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

#ifndef PDI_STRING_TOOLS_H_
#define PDI_STRING_TOOLS_H_

#include <string>
#include <vector>

#include <pdi/plugin.h>

namespace PDI {

/** Parse and unescape array of colon separated string
 * 
 * The backslash character is used for escaping. Only the following characters can be escaped:
 * - backslash: to get a raw backslash,
 * - colon (:): to get a raw colon that is not interpreted as a separator
 * 
 * \param unescaped the colon separated string array
 * \result the actuall arra of unescaped strings
 */
std::vector<std::string> string_array_parse(const std::string& unescaped);

} // namespace PDI

#endif // PDI_STRING_TOOLS_H_
