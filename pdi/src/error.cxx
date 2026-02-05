// SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "config.h"

#include <cassert>
#include <memory>
#include <string>

#include <pthread.h>

#include "pdi/error.h"

namespace PDI {

Error::Error(PDI_status_t errcode)
	: m_status{errcode}
{}

Error::Error(PDI_status_t errcode, const char* fmt)
	: m_status{errcode}
	, m_what{fmt}
{
	// force inclusion of dtor
	fmt::format_error("");
}

const char* Error::what() const noexcept
{
	return m_what.c_str();
}

PDI_status_t Error::status() const noexcept
{
	return m_status;
}

} // namespace PDI
