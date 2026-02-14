/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "pdi/error.h"

#include "pdi/paraconf_wrapper.h"

namespace PDI {

using std::string;

namespace {

static constexpr char const * ERRMSG[]
	= {"Unexpected error, not from YAML",
       "invalid parameter value",
       "unexpected type for a node",
       "the requested node doen't exist in the tree",
       "the provided input is invalid",
       "system error"};

void do_pc(PC_tree_t tree, PC_status_t status)
{
	if (status && status < (sizeof(ERRMSG) / sizeof(ERRMSG[0]))) {
		throw Config_error{tree, "{}. {}", ERRMSG[status], PC_errmsg()};
	} else if (status) {
		throw Config_error{tree, "unexpected error #{}. {}", static_cast<int>(status), PC_errmsg()};
	}
}

} // namespace

Paraconf_wrapper::Paraconf_wrapper()
	: m_handler{PC_errhandler(PC_NULL_HANDLER)}
{}

Paraconf_wrapper::~Paraconf_wrapper()
{
	PC_errhandler(m_handler);
}

int len(PC_tree_t tree)
{
	int result;
	do_pc(tree, PC_len(tree, &result));
	return result;
}

int len(PC_tree_t tree, int dflt)
{
	int result;
	if (PC_len(tree, &result)) return dflt;
	return result;
}

long to_long(PC_tree_t tree)
{
	long result;
	do_pc(tree, PC_int(tree, &result));
	return result;
}

long to_long(PC_tree_t tree, long dflt)
{
	long result;
	if (PC_int(tree, &result)) return dflt;
	return result;
}

double to_double(PC_tree_t tree)
{
	double result;
	do_pc(tree, PC_double(tree, &result));
	return result;
}

double to_double(PC_tree_t tree, double dflt)
{
	double result;
	if (PC_double(tree, &result)) return dflt;
	return result;
}

string to_string(PC_tree_t tree)
{
	char* cresult;
	do_pc(tree, PC_string(tree, &cresult));
	string result = cresult;
	free(cresult);
	return result;
}

string to_string(PC_tree_t tree, const string& dflt)
{
	char* cresult;
	if (PC_string(tree, &cresult)) return dflt;
	string result = cresult;
	free(cresult);
	return result;
}

bool to_bool(PC_tree_t tree)
{
	int result;
	do_pc(tree, PC_bool(tree, &result));
	return result;
}

bool to_bool(PC_tree_t tree, bool dflt)
{
	int result;
	if (PC_bool(tree, &result)) return dflt;
	return result;
}

bool is_map(PC_tree_t tree)
{
	return PC_status(tree) == PC_OK && tree.node->type == YAML_MAPPING_NODE;
}

bool is_scalar(PC_tree_t tree)
{
	if (PC_status(tree)) return false;
	if (tree.node->type != YAML_SCALAR_NODE) return false;
	std::string strval = to_string(tree);
	return strval != "~" && strval != "";
}

bool is_seq(PC_tree_t tree)
{
	return PC_status(tree) == PC_OK && tree.node->type == YAML_SEQUENCE_NODE;
}

bool empty(PC_tree_t tree)
{
	if (PC_status(tree)) return false;
	if (tree.node->type == YAML_SCALAR_NODE) {
		std::string strval = to_string(tree);
		return strval != "~" && strval != "";
	}
	return true;
}

void each(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	if (!is_seq(tree)) {
		if (is_scalar(tree)) {
			throw Config_error{tree, "Expected a sequence, found a scalar: `{}'", to_string(tree)};
		} else if (is_map(tree)) {
			throw Config_error{tree, "Expected a sequence, found a mapping"};
		} else {
			throw Config_error{tree, "Expected a sequence, invalid element found"};
		}
	}
	int nb_elem = len(tree);
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		operation(PC_get(tree, "[%d]", elem_id));
	}
}

void each_in_map(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation)
{
	if (!is_map(tree)) {
		if (is_scalar(tree)) {
			throw Config_error{tree, "Expected a mapping, found a scalar: `{}'", to_string(tree)};
		} else if (is_seq(tree)) {
			throw Config_error{tree, "Expected a mapping, found a sequence of {} elements", len(tree)};
		} else {
			throw Config_error{tree, "Expected a mapping, invalid element found"};
		}
	}
	int nb_elem = len(tree);
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		operation(PC_get(tree, "{%d}", elem_id), PC_get(tree, "<%d>", elem_id));
	}
}

void each_in_omap(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation)
{
	if (!is_seq(tree)) {
		if (is_scalar(tree)) {
			throw Config_error{tree, "Expected an ordered mapping, found a scalar: `{}'", to_string(tree)};
		} else if (is_map(tree)) {
			throw Config_error{tree, "Expected an ordered mapping, found an (unordered) mapping"};
		} else {
			throw Config_error{tree, "Expected an ordered mapping, invalid element found"};
		}
	}
	int nb_elem = len(tree);
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		PC_tree_t elem = PC_get(tree, "[%d]", elem_id);
		if (!is_map(elem)) {
			throw Config_error{elem, "Invalid ordered mapping found (no key)"};
		} else if (len(elem) != 1) {
			throw Config_error{elem, "Invalid ordered mapping found (multiple keys)"};
		}
		operation(PC_get(elem, "{0}"), PC_get(elem, "<0>"));
	}
}

void one_or_each(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	if (is_seq(tree)) {
		each(tree, operation);
	} else {
		operation(tree);
	}
}

void opt_each(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation)
{
	opt_one(tree, [&](PC_tree_t) { each_in_map(tree, std::move(operation)); });
}

void opt_each_in_seq(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	opt_one(tree, [&](PC_tree_t) { each(tree, std::move(operation)); });
}

void opt_one(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	if (!empty(tree)) operation(tree);
}

void opt_one_or_each(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	opt_one(tree, [&](PC_tree_t) { one_or_each(tree, std::move(operation)); });
}

} // namespace PDI
