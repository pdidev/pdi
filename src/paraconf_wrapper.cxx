/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#include "pdi/status.h"

#include "paraconf_wrapper.h"

namespace
{

using PDI::Error;

void do_pc(PC_status_t status)
{
	if (status) {
		throw Error{PDI_ERR_CONFIG, "Configuration error #%d: %s", static_cast<int>(status), PC_errmsg()};
	}
}

} // namespace <anonymous>

namespace PDI
{

using std::string;


int len(PC_tree_t tree)
{
	int result;
	do_pc(PC_len(tree, &result));
	return result;
}

long to_long(PC_tree_t tree)
{
	long result;
	do_pc(PC_int(tree, &result));
	return result;
}

double to_double(PC_tree_t tree)
{
	double result;
	do_pc(PC_double(tree, &result));
	return result;
}

std::string to_string(PC_tree_t tree)
{
	char *cresult;
	do_pc(PC_string(tree, &cresult));
	string result = cresult;
	free(cresult);
	return result;
}

bool to_bool(PC_tree_t tree)
{
	int result;
	do_pc(PC_bool(tree, &result));
	return result;
}

} // namespace PDI
