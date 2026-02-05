/*
 * SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
