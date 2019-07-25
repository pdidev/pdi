/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "paraconf.h"

#include "status.h"
#include "tools.h"

#include "ypath.h"

static const char *nodetype[4] = {
	"none",
	"scalar",
	"sequence",
	"mapping"
};

static PC_tree_t get_seq_idx( const PC_tree_t tree, const char **req_index, const char *full_index )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	const char *index = *req_index;
	
	// read '['
	if ( *index != '[' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER, "Expected opening square bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// read int
	char *post_index;
	long seq_idx = strtol(index, &post_index, 0);
	if ( post_index == index ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected integer at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	index = post_index;
	
	// read ']'
	if ( *index != ']' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected closing square bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// check type
	if ( tree.node->type != YAML_SEQUENCE_NODE ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected sequence, found %s (ROOT)%.*s\n",
				nodetype[tree.node->type],
				(int)(*req_index-full_index),
				full_index), err0);
	}
	
	// handle index
	if ( seq_idx < 0 || seq_idx >= (tree.node->data.sequence.items.top - tree.node->data.sequence.items.start) ) {
		PC_handle_err_tree(PC_make_err(PC_NODE_NOT_FOUND,
				"Index %ld out of range [0...%ld[ in (ROOT)%.*s\n",
				seq_idx,
				(long)(tree.node->data.sequence.items.top - tree.node->data.sequence.items.start),
				(int)(*req_index-full_index),
				full_index), err0);
	}
	restree.node = yaml_document_get_node(tree.document, *(tree.node->data.sequence.items.start + seq_idx));
	assert(tree.node);
	
	*req_index = index;
	return restree;
	
err0:
	return restree;
}

static PC_tree_t get_map_key_val( const PC_tree_t tree, const char **req_index, const char *full_index )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	const char *index = *req_index;
		
	// read '.'
	if ( *index != '.' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER, "Expected dot at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// read key
	const char *key = index;
	size_t key_len = 0;
	while ( key[key_len] && key[key_len] != '.' && key[key_len] != '[' && key[key_len] != '{' && key[key_len] != '<' ) ++key_len;
	index += key_len;
	
	// check type
	if ( tree.node->type != YAML_MAPPING_NODE ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_NODE_TYPE, "Expected mapping, found %s (ROOT)%.*s\n",
				nodetype[tree.node->type],
				(int)(*req_index-full_index),
				full_index), err0);
	}
	
	// handle key
	yaml_node_pair_t *pair;
	for ( pair = tree.node->data.mapping.pairs.start; pair != tree.node->data.mapping.pairs.top; ++pair ) {
		// get the key string
		char *found_key;
		PC_handle_err_tree(PC_string(subtree(tree, pair->key), &found_key), err0);
		
		// check if we found the key, in that case, leave
		int cmp = strlzcmp(key, found_key, key_len);
		free(found_key);
		if ( !cmp ) break;
	}
	if ( pair == tree.node->data.mapping.pairs.top ) {
		PC_handle_err_tree(PC_make_err(PC_NODE_NOT_FOUND, "Key `%.*s' not found in (ROOT)%.*s\n",
				key_len,
				key,
				(int)(*req_index-full_index),
				full_index), err0);
	}
	restree.node = yaml_document_get_node(tree.document, pair->value);
	assert(tree.node);
	
	*req_index = index;
	return restree;
	
err0:
	return restree;
}

static PC_status_t get_map_idx_pair( const PC_tree_t tree, const char **req_index, const char *full_index, yaml_node_pair_t **pair )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	const char *index = *req_index;
	
	// read int
	char *post_index;
	long map_idx = strtol(index, &post_index, 0);
	if ( post_index == index ) {
		PC_handle_err(PC_make_err(PC_INVALID_PARAMETER, "Expected integer at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	index = post_index;
	
	// check type
	if ( tree.node->type != YAML_MAPPING_NODE ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE, "Expected mapping, found %s (ROOT)%.*s\n",
				nodetype[tree.node->type],
				(int)(*req_index-full_index),
				full_index), err0);
	}
	
	// handle index
	if ( map_idx < 0 || map_idx >= (tree.node->data.mapping.pairs.top - tree.node->data.mapping.pairs.start) ) {
		PC_handle_err(PC_make_err(PC_NODE_NOT_FOUND, "Index %ld out of range [0...%ld] in (ROOT)%.*s\n",
				map_idx,
				(long)(tree.node->data.mapping.pairs.top - tree.node->data.mapping.pairs.start),
				(int)(*req_index-full_index-1),
				full_index), err0);
	}
	*pair = tree.node->data.mapping.pairs.start + map_idx;
	assert(*pair);
	
	*req_index = index;
	return status;
	
err0:
	return status;
}

static PC_tree_t get_map_idx_key( const PC_tree_t tree, const char **req_index, const char *full_index )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	const char *index = *req_index;
	
	// read '{'
	if ( *index != '{' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected opening curly bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// get pair
	yaml_node_pair_t *pair = NULL; 
	PC_handle_err_tree(get_map_idx_pair(tree, &index, full_index, &pair),err0);
	
	// read '}'
	if ( *index != '}' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected closing curly bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// handle pair
	restree.node = yaml_document_get_node(tree.document, pair->key);
	assert(tree.node);
	
	*req_index = index;
	return restree;
	
err0:
	return restree;
}

static PC_tree_t get_map_idx_val( const PC_tree_t tree, const char **req_index, const char *full_index )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	const char *index = *req_index;
	
	// read '<'
	if ( *index != '<' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected opening angle bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// get pair
	yaml_node_pair_t *pair = NULL; 
	PC_handle_err_tree(get_map_idx_pair(tree, &index, full_index, &pair),err0);
	
	// read '>'
	if ( *index != '>' ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER,
				"Expected closing angle bracket at char #%ld of `%.*s'\n",
				(long int)(index-full_index),
				full_index), err0);
	}
	++index;
	
	// handle pair
	restree.node = yaml_document_get_node(tree.document, pair->value);
	assert(tree.node);
	
	*req_index = index;
	return restree;
	
err0:
	return restree;
}

PC_tree_t PC_sget( const PC_tree_t tree, const char *index )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	// check type
	if ( *index && !tree.node ) {
		PC_handle_err_tree(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	const char *full_index = index;
	
	while ( !PC_status(restree) ) {
		switch ( *index ) {
		case '[':
			restree = get_seq_idx(restree, &index, full_index);
			break;
		case '.': 
			restree = get_map_key_val(restree, &index, full_index);
			break;
		case '{': 
			restree = get_map_idx_key(restree, &index, full_index);
			break;
		case '<': 
			restree = get_map_idx_val(restree, &index, full_index);
			break;
		case 0:
			assert(restree.node);
			goto err0;
		default:
			PC_handle_err_tree(PC_make_err(PC_INVALID_PARAMETER, "Invalid character at char #%ld of `%.*s'\n",
					(long int)(index-full_index),
					full_index), err0);
		}
	}
	
	return restree;
	
err0:
	return restree;
}
