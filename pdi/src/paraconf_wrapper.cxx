/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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
#include <cstring>
#include <optional>
#include <string>

#include "pdi/error.h"

#include "pdi/paraconf_wrapper.h"

namespace PDI {

using std::nullopt;
using std::optional;
using std::strcmp;
using std::string;

namespace {

void do_pc(PC_tree_t tree, PC_status_t status)
{
	if (status) {
		throw Spectree_error{tree, "{}", PC_errmsg()};
	}
}

// since paraconf 1.2.0, PC_bool accepts every YAML 1.1 spelling of a boolean, fixing pdidev/paraconf#6
#ifdef PARACONF_VERSION
#if PARACONF_UNTYPED_VERSION >= PARACONF_UNTYPED_COMPUTE_VERSION(1, 2, 0)
#define PDI_HAS_PC_BOOL_WITH_ON_OFF
#endif
#endif

/** Read a boolean as YAML 1.1 spells it
 *
 * PC_bool before paraconf 1.2.0 accepts only the true/false and yes/no spellings, while YAML 1.1 also has y/n and on/off, so this reads them itself
 * with an older paraconf, working around pdidev/paraconf#6.
 * Drop it once PDI requires paraconf 1.2.0.
 *
 * \param[in] tree the node
 * \param[in] dflt the value to return when `tree` is in error, is not a scalar or spells no boolean, none to throw instead
 *
 * \return the boolean value of the scalar node
 *
 * \throw Spectree_error if `tree` is in error, is not a scalar or spells no boolean, and `dflt` is empty
 */
bool pc_bool_workaround(PC_tree_t tree, optional<bool> dflt = nullopt)
{
#ifdef PDI_HAS_PC_BOOL_WITH_ON_OFF
	int result;
	PC_status_t status = PC_bool(tree, &result);
	if (status && dflt) {
		return *dflt;
	} else if (status) {
		throw Spectree_error{tree, "{}", PC_errmsg()};
	} else {
		return result;
	}
#else
	if (PC_status(tree)) {
		if (dflt) return *dflt;
		throw Spectree_error(tree, "expected a boolean, but got an invalid tree");
	}

	if (!is_scalar(tree)) {
		if (dflt) return *dflt;
		static constexpr char const * nodetype[4] = {"none", "scalar", "sequence", "mapping"};
		throw Spectree_error(tree, "expected a scalar boolean, but got a {}", nodetype[tree.node->type]);
	}

	// libyaml stores the value as unsigned chars
	char const * strval = reinterpret_cast<char*>(tree.node->data.scalar.value);

	auto&& not_a_boolean = [&]() {
		if (dflt) return *dflt;
		throw Spectree_error(
			tree,
			"expected a boolean (y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF), but got `{}'",
			strval
		);
	};

	switch (strval[0]) {
	case 'y':
		switch (strval[1]) {
		case 0:
			return true;
		case 'e':
			if (strval[2] == 's' && strval[3] == 0) return true;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'Y':
		switch (strval[1]) {
		case 0:
			return true;
		case 'e':
			if (strval[2] == 's' && strval[3] == 0) return true;
			return not_a_boolean();
		case 'E':
			if (strval[2] == 'S' && strval[3] == 0) return true;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'n':
		switch (strval[1]) {
		case 0:
			return false;
		case 'o':
			if (strval[2] == 0) return false;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'N':
		switch (strval[1]) {
		case 0:
			return false;
		case 'o': // same as 'O'
		case 'O':
			if (strval[2] == 0) return false;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 't':
		if (!strcmp(strval + 1, "rue")) return true;
		return not_a_boolean();
	case 'T':
		switch (strval[1]) {
		case 'r':
			if (!strcmp(strval + 2, "ue")) return true;
			return not_a_boolean();
		case 'R':
			if (!strcmp(strval + 2, "UE")) return true;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'f':
		if (!strcmp(strval + 1, "alse")) return false;
		return not_a_boolean();
	case 'F':
		switch (strval[1]) {
		case 'a':
			if (!strcmp(strval + 2, "lse")) return false;
			return not_a_boolean();
		case 'A':
			if (!strcmp(strval + 2, "LSE")) return false;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'o':
		switch (strval[1]) {
		case 'n':
			if (strval[2] == 0) return true;
			return not_a_boolean();
		case 'f':
			if (strval[2] == 'f' && strval[3] == 0) return false;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	case 'O':
		switch (strval[1]) {
		case 'n': // same as 'N'
		case 'N':
			if (strval[2] == 0) return true;
			return not_a_boolean();
		case 'f':
			if (strval[2] == 'f' && strval[3] == 0) return false;
			return not_a_boolean();
		case 'F':
			if (strval[2] == 'F' && strval[3] == 0) return false;
			return not_a_boolean();
		default:
			return not_a_boolean();
		}
	default:
		return not_a_boolean();
	}
#endif
}

} // namespace

Yaml_region::Yaml_region(PC_tree_t tree)
	: m_file(
#ifdef PARACONF_VERSION
#if PARACONF_UNTYPED_VERSION >= PARACONF_UNTYPED_COMPUTE_VERSION(1, 1, 0)
		  PC_path(tree)
#else
		  ""
#endif
#else
		  ""
#endif
	  )
	, m_start{tree.node->start_mark.line + 1, tree.node->start_mark.column + 1}
	, m_end{tree.node->end_mark.line + 1, tree.node->end_mark.column + 1}
{
	assert(!PC_status(tree) && "building a region from an invalid tree is unspecified");
}

std::optional<Yaml_region> Yaml_region::make(PC_tree_t tree)
{
	if (PC_status(tree) == PC_OK && tree.node) {
		return Yaml_region(tree);
	} else {
		return {};
	}
}

Paraconf_wrapper::Paraconf_wrapper()
	: m_handler{PC_errhandler(PC_NULL_HANDLER)}
{}

Paraconf_wrapper::~Paraconf_wrapper()
{
	PC_errhandler(m_handler);
}

std::string to_string(Yaml_region location)
{
	return fmt::format(
		"{}({}:{} -> {}:{})",
		location.file(),
		location.start().line,
		location.start().column,
		location.end().line,
		location.end().column
	);
}

std::string to_string(std::optional<Yaml_region> location)
{
	return location ? to_string(*location) : "";
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
	return pc_bool_workaround(tree);
}

bool to_bool(PC_tree_t tree, bool dflt)
{
	return pc_bool_workaround(tree, dflt);
}

bool is_list(PC_tree_t tree)
{
	return !PC_status(tree) && tree.node && tree.node->type == YAML_SEQUENCE_NODE;
}

bool is_map(PC_tree_t tree)
{
	return !PC_status(tree) && tree.node && tree.node->type == YAML_MAPPING_NODE;
}

bool is_scalar(PC_tree_t tree)
{
	return !PC_status(tree) && tree.node && tree.node->type == YAML_SCALAR_NODE;
}

void each(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	int nb_elem = len(tree);
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		operation(PC_get(tree, "[%d]", elem_id));
	}
}

void opt_each(PC_tree_t tree, std::function<void(PC_tree_t)> operation)
{
	if (!PC_status(PC_get(tree, "[0]"))) {
		each(tree, operation);
	} else {
		operation(tree);
	}
}

void each(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation)
{
	int nb_elem = len(tree);
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		operation(PC_get(tree, "{%d}", elem_id), PC_get(tree, "<%d>", elem_id));
	}
}

void each_in_omap(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation)
{
	int nb_elem = len(tree);
	if (!is_list(tree)) {
		if (is_scalar(tree)) {
			throw Spectree_error{tree, "Expected an ordered mapping, found a scalar"};
		} else if (is_map(tree)) {
			throw Spectree_error{tree, "Expected an ordered mapping, found a (unordered) mapping"};
		} else {
			throw Spectree_error{tree, "Expected an ordered mapping, invalid element found"};
		}
	}
	for (int elem_id = 0; elem_id < nb_elem; ++elem_id) {
		PC_tree_t elem = PC_get(tree, "[%d]", elem_id);
		if (!is_map(elem)) {
			throw Spectree_error{elem, "Invalid ordered mapping found (no key)"};
		} else if (len(elem) != 1) {
			throw Spectree_error{elem, "Invalid ordered mapping found (multiple keys)"};
		}
		operation(PC_get(elem, "{0}"), PC_get(elem, "<0>"));
	}
}

} // namespace PDI
