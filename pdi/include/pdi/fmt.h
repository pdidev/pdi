/*******************************************************************************
 * Copyright (C) 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

// this is a header designed to support fmt provided as either a standalone of embedded in spdlog

#include <spdlog/spdlog.h>
#if !defined(SPDLOG_FMT_EXTERNAL)
	#ifdef SPDLOG_HEADER_ONLY
		#ifndef FMT_HEADER_ONLY
			#define FMT_HEADER_ONLY
		#endif
	#endif
	#include <spdlog/fmt/bundled/core.h>
	#include <spdlog/fmt/bundled/format.h>
	#if FMT_VERSION >= 110000
		#include <spdlog/fmt/bundled/ranges.h>
	#endif
#else // SPDLOG_FMT_EXTERNAL is defined - use external fmtlib
	#include <fmt/core.h>
	#include <fmt/format.h>
	#if FMT_VERSION >= 110000
		#include <fmt/ranges.h>
	#endif
#endif
