/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

/** \file pdi.c
 * Implementation of the PDI public API functions.
 **/

#include "config.h"

#include <cstddef>
#include <exception>
#include <iomanip>
#include <iostream>
#include <list>
#include <string>
#include <sstream>
#include <type_traits>
#include <unordered_set>

#include <spdlog/spdlog.h>

#include "pdi/context.h"
#include "pdi/global_context.h"
#include "pdi/data_descriptor.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"
#include "pdi/datatype.h"


namespace {

using namespace PDI;
using std::cerr;
using std::endl;
using std::exception;
using std::list;
using std::make_shared;
using std::move;
using std::setfill;
using std::setw;
using std::stack;
using std::string;
using std::stringstream;
using std::underlying_type;
using std::unique_ptr;

struct Error_context {
	PDI_errhandler_t handler;
	
	string errmsg;
	
	
	Error_context(): handler{PDI_ASSERT_HANDLER} {}
	
	/** Return the C error and stores the message corresponding to the C++ exception
	 */
	PDI_status_t return_err(const Error& err)
	{
		errmsg = err.what();
		if (handler.func) handler.func(err.status(), errmsg.c_str(), handler.context);
		return err.status();
	}
	
	/** Return the C error and stores the message corresponding to the C++ exception
	 */
	PDI_status_t return_err(const exception& err)
	{
		errmsg = err.what();
		if (handler.func) handler.func(PDI_ERR_SYSTEM, errmsg.c_str(), handler.context);
		return PDI_ERR_SYSTEM;
	}
	
	/** Return the C error and stores the message corresponding to the C++ exception
	 */
	PDI_status_t return_err()
	{
		errmsg = "Unexpected error";
		if (handler.func) handler.func(PDI_ERR_SYSTEM, errmsg.c_str(), handler.context);
		return PDI_ERR_SYSTEM;
	}
	
}; // struct Error_context


/// The thread-local error context
thread_local Error_context g_error_context;

/// The name of the ongoing transaction or "" if none
string g_transaction;

/// Status of the ongoing transaction
PDI_status_t g_transaction_status = PDI_OK;

/// List of data that are part of the current transaction
list<string> g_transaction_data;


/** Logical operator to manipulate PDI_inout_t
 */
PDI_inout_t operator&(PDI_inout_t a, PDI_inout_t b)
{
	typedef underlying_type< PDI_inout_t >::type UL;
	return static_cast<PDI_inout_t>(static_cast<UL>(a) & static_cast<UL>(b));
}

/** An error handler that generates fatal errors
 */
void assert_status(PDI_status_t status, const char* message, void*)
{
	if (status) {
		if ( Global_context::initialized() ) {
			Global_context::context().logger()->error(message);
		} else {
			cerr << "[PDI][NOINIT] *** Fatal error: " << message << endl;
		}
		exit(status);
	}
}

/** An error handler that generates warning messages
 */
void warn_status(PDI_status_t status, const char* message, void*)
{
	if (status && Global_context::initialized() ) {
		Global_context::context().logger()->warn(message);
	}
}

} // namespace <anonymous>


const PDI_errhandler_t PDI_ASSERT_HANDLER = {
	&assert_status,
	NULL
};

const PDI_errhandler_t PDI_WARN_HANDLER = {
	&warn_status,
	NULL
};

const PDI_errhandler_t PDI_NULL_HANDLER = {
	NULL,
	NULL
};


const char* PDI_errmsg()
{
	return g_error_context.errmsg.c_str();
}

PDI_errhandler_t PDI_errhandler(PDI_errhandler_t new_handler)
{
	PDI_errhandler_t old_handler = g_error_context.handler;
	g_error_context.handler = new_handler;
	return old_handler;
}

PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm* world)
try
{
	Paraconf_wrapper fw;
	g_transaction.clear();
	g_transaction_data.clear();
	Global_context::init(conf, world);
	Global_context::context().logger()->info("Initialization successful");
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_finalize()
try
{
	Paraconf_wrapper fw;
	g_transaction.clear();
	g_transaction_data.clear();
	Global_context::context().logger()->info("Finalization");
	Global_context::finalize();
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_version(unsigned long* provided, unsigned long expected)
try
{
	Paraconf_wrapper fw;
	if ( provided ) *provided = PDI_VERSION;
	
	constexpr unsigned long MASK = (1<<8) - 1;
	unsigned long expected_major = (expected>>24)&MASK;
	unsigned long expected_minor = (expected>>16)&MASK;
	unsigned long expected_patch = (expected>>8 )&MASK;
	
	if (
	    expected
	    && (
	        expected_major != PDI_VERSION_MAJOR
	        || expected_minor > PDI_VERSION_MINOR
	    )
	) {
		throw Error{
			PDI_ERR_PLUGIN,
			"Invalid PDI API version: %lu.%lu.%lu, PDI provided version is %lu.%lu.%lu",
			expected_major,
			expected_minor,
			expected_patch,
			PDI_VERSION_MAJOR,
			PDI_VERSION_MINOR,
			PDI_VERSION_PATCH
		};
	}
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_event(const char* name)
try
{
	Paraconf_wrapper fw;
	Global_context::context().event(name);
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_share(const char* name, void* buffer, PDI_inout_t access)
try
{
	Paraconf_wrapper fw;
	Global_context::context()[name].share(buffer, access & PDI_OUT, access & PDI_IN);
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_access(const char* name, void** buffer, PDI_inout_t inout)
try
{
	Paraconf_wrapper fw;
	Data_descriptor& desc = Global_context::context()[name];
	*buffer = desc.share(desc.ref(), inout & PDI_IN, inout & PDI_OUT);
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_release(const char* name)
try
{
	Paraconf_wrapper fw;
	Global_context::context()[name].release();
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_reclaim(const char* name)
try
{
	Paraconf_wrapper fw;
	Global_context::context()[name].reclaim();
	return PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_expose(const char* name, void* data, PDI_inout_t access)
try
{
	Paraconf_wrapper fw;
	if (PDI_status_t status = PDI_share(name, data, access)) {
		if (!g_transaction.empty() && !g_transaction_status)
			g_transaction_status = status; //if it is first error in transaction, save its status
		return status;
	}
	
	if (!g_transaction.empty()) {   // defer the reclaim
		g_transaction_data.emplace_back(name);
	} else { // do the reclaim now
		if (PDI_status_t status = PDI_reclaim(name)) return status;
	}
	return PDI_OK;
} catch (const Error& e)
{
	PDI_status_t status = g_error_context.return_err(e);
	if (!g_transaction.empty() && !g_transaction_status)
		g_transaction_status = status; //if it is first error in transaction, save its status
	return status;
} catch (const exception& e)
{
	PDI_status_t status = g_error_context.return_err(e);
	if (!g_transaction.empty() && !g_transaction_status)
		g_transaction_status = status; //if it is first error in transaction, save its status
	return status;
} catch (...)
{
	PDI_status_t status = g_error_context.return_err();
	if (!g_transaction.empty() && !g_transaction_status)
		g_transaction_status = status; //if it is first error in transaction, save its status
	return status;
}

PDI_status_t PDI_multi_expose(const char* event_name, const char* name, void* data, PDI_inout_t access, ...)
try
{
	Paraconf_wrapper fw;
	va_list ap;
	list<string> transaction_data;
	PDI_status_t status;
	if (status = PDI_share(name, data, access)) return status;
	transaction_data.emplace_back(name);
	
	va_start(ap, access);
	while (const char* v_name = va_arg(ap, const char*)) {
		void* v_data = va_arg(ap, void*);
		PDI_inout_t v_access = static_cast<PDI_inout_t>(va_arg(ap, int));
		if (status = PDI_share(v_name, v_data, v_access)) {
			break;
		}
		transaction_data.emplace_back(v_name);
	}
	va_end(ap);
	
	if (!status) { //trigger event only when all data is available
		status = PDI_event(event_name);
	}
	
	for (auto&& it = transaction_data.rbegin(); it != transaction_data.rend(); it++) {
		PDI_status_t r_status = PDI_reclaim(it->c_str());
		status = !status ? r_status : status; //if it is first error, save its status (try to reclaim other desc anyway)
	}
	//the status of the first error is returned
	return status;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}


PDI_status_t PDI_transaction_begin(const char* name)
try
{
	Paraconf_wrapper fw;
	if (!g_transaction.empty()) {
		throw Error{PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"};
	}
	g_transaction = name;
	return g_transaction_status = PDI_OK;
} catch (const Error& e)
{
	return g_error_context.return_err(e);
} catch (const exception& e)
{
	return g_error_context.return_err(e);
} catch (...)
{
	return g_error_context.return_err();
}

PDI_status_t PDI_transaction_end()
try
{
	Paraconf_wrapper fw;
	if (g_transaction.empty()) {
		throw Error{PDI_ERR_STATE, "No transaction in progress, cannot end one"};
	}
	
	if (!g_transaction_status) { //trigger event only when all data is available
		g_transaction_status = PDI_event(g_transaction.c_str());
	}
	
	for (auto&& it = g_transaction_data.rbegin(); it != g_transaction_data.rend(); it++) {
		PDI_status_t r_status = PDI_reclaim(it->c_str());
		g_transaction_status = !g_transaction_status ? r_status : g_transaction_status; //if it is first error, save its status (try to reclaim other desc anyway)
	}
	g_transaction_data.clear();
	g_transaction.clear();
	
	//the status of the first error is returned
	return g_transaction_status;
} catch (const Error& e)
{
	PDI_status_t status = g_error_context.return_err(e);
	g_transaction_status = !g_transaction_status ? status : g_transaction_status; //if it is first error, save its status
	return g_transaction_status;
} catch (const exception& e)
{
	PDI_status_t status = g_error_context.return_err(e);
	g_transaction_status = !g_transaction_status ? status : g_transaction_status; //if it is first error, save its status
	return g_transaction_status;
} catch (...)
{
	PDI_status_t status = g_error_context.return_err();
	g_transaction_status = !g_transaction_status ? status : g_transaction_status; //if it is first error, save its status
	return g_transaction_status;
}
