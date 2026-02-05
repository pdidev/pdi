/*
 * SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
