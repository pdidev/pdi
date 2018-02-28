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
#include <cstring>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>
#include <unordered_set>

#include "pdi/context.h"
#include "pdi/data_descriptor.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/reference.h"
#include "pdi/error.h"
#include "pdi/datatype.h"


namespace {

using namespace PDI;
using std::cerr;
using std::endl;
using std::make_shared;
using std::move;
using std::stack;
using std::string;
using std::stringstream;
using std::underlying_type;
using std::unique_ptr;
using std::unordered_set;


/// The singleton context of PDI
unique_ptr<Context> g_context;

/// The name of the ongoing transaction or "" if none
string g_transaction;

/// List of data that are
unordered_set<string> g_transaction_data;


PDI_inout_t operator&(PDI_inout_t a, PDI_inout_t b)
{
	typedef underlying_type< PDI_inout_t >::type UL;
	return static_cast<PDI_inout_t>(static_cast<UL>(a) & static_cast<UL>(b));
}

}

PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm* world)
try
{
	Try_pc fw;
	g_transaction.clear();
	g_transaction_data.clear();
	g_context.reset(new Context{conf, world});
	return PDI_OK;
} catch (const Error& e)
{
	g_context.reset();
	return return_err(e);
}

PDI_status_t PDI_finalize()
try
{
	Try_pc fw;
	g_transaction.clear();
	g_transaction_data.clear();
	g_context.reset();
	return PDI_OK;
} catch (const Error& e)
{
	g_context.reset();
	return return_err(e);
}

PDI_status_t PDI_event(const char* name)
try
{
	Try_pc fw;
	g_context->event(name);
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_share(const char* name, void* buffer, PDI_inout_t access)
try
{
	Try_pc fw;
	(*g_context)[name].share(buffer, access & PDI_OUT, access & PDI_IN);
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_access(const char* name, void** buffer, PDI_inout_t inout)
try
{
	Try_pc fw;
	Data_descriptor& desc = (*g_context)[name];
	*buffer = desc.share(desc.ref(), inout & PDI_IN, inout & PDI_OUT);
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_release(const char* name)
try
{
	Try_pc fw;
	(*g_context)[name].release();
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_reclaim(const char* name)
try
{
	Try_pc fw;
	(*g_context)[name].reclaim();
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_expose(const char* name, void* data, PDI_inout_t access)
try
{
	Try_pc fw;
	if (PDI_status_t status = PDI_share(name, data, access)) return status;
	if (! g_transaction.empty()) {   // defer the reclaim
		g_transaction_data.emplace(name);
	} else { // do the reclaim now
		if (PDI_status_t status = PDI_reclaim(name)) return status;
	}
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_transaction_begin(const char* name)
try
{
	Try_pc fw;
	if (!g_transaction.empty()) {
		return return_err(Error{PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"});
	}
	g_transaction = name;
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}

PDI_status_t PDI_transaction_end()
try
{
	Try_pc fw;
	if (g_transaction.empty()) {
		return return_err(Error{PDI_ERR_STATE, "No transaction in progress, cannot end one"});
	}
	PDI_event(g_transaction.c_str());
	for (const string& data : g_transaction_data) {
		//TODO we should concatenate errors here...
		PDI_reclaim(data.c_str());
	}
	g_transaction_data.clear();
	g_transaction.clear();
	return PDI_OK;
} catch (const Error& e)
{
	return return_err(e);
}
