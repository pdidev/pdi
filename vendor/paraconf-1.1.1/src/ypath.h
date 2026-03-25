/* Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
 *               root of the project or at https://github.com/pdidev/paraconf
 * 
 * SPDX-License-Identifier: MIT
 */

#ifndef YPATH_H__
#define YPATH_H__

#include "paraconf.h"

struct PC_document_s {
	/// The underlying YAML document
	yaml_document_t document;
	/// The path to the file from which the document was parsed
	const char* path;
};

PC_tree_t PARACONF_EXPORT PC_sget(PC_tree_t tree, const char* index);

#endif // YPATH_H__
