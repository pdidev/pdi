/* Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
 *               root of the project or at https://github.com/pdidev/paraconf
 * 
 * SPDX-License-Identifier: MIT
 */

#ifndef TOOLS_H__
#define TOOLS_H__

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "paraconf.h"

static inline PC_tree_t subtree(PC_tree_t tree, int key)
{
	tree.node = yaml_document_get_node(tree.document, key);
	assert(tree.node);
	return tree;
}

static inline int strlzcmp(const char *lstr, const char *zstr, size_t lstr_size)
{
	int res = strncmp(lstr, zstr, lstr_size);
	if ( res ) return res;
	if ( zstr[lstr_size] ) return -1;
	return res;
}

#endif // TOOLS_H__
