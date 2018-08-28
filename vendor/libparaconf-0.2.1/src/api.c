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

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "paraconf.h"

#include "ypath.h"
#include "status.h"

#define PC_BUFFER_SIZE 256
#define ERRBUF_SIZE 512

static const char *nodetype[4] = {
	"none",
	"scalar",
	"sequence",
	"mapping"
};

PC_tree_t PC_parse_path( const char *path )
{
	PC_tree_t restree = { PC_OK, NULL, NULL };
	
	FILE *conf_file = fopen(path, "rb");
	if ( !conf_file ) {
		char errbuf[ERRBUF_SIZE];
		strerror_r(errno, errbuf, ERRBUF_SIZE);
		PC_handle_err_tree(PC_make_err(PC_SYSTEM_ERROR, errbuf), err0);
	}
	
	PC_errhandler_t handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	restree = PC_parse_file(conf_file);
	PC_errhandler(handler);
	if ( PC_status(restree) ) { // aka PC_catch
		PC_handle_err_tree(PC_make_err(restree.status, "Error while opening file `%s`, %s", path, PC_errmsg()), err1);
	}
	
	fclose(conf_file);
	return restree;
	
err1:
	fclose(conf_file);
err0:
	return restree;
}

PC_tree_t PC_parse_string(const char* document)
{
	PC_tree_t restree = { PC_OK, NULL, NULL };
	
	yaml_parser_t conf_parser; 
	if ( !yaml_parser_initialize(&conf_parser) ) {
		PC_handle_err_tree(PC_make_err(PC_SYSTEM_ERROR, "unable to load yaml library"), err0);
	}
	
	yaml_parser_set_input_string(&conf_parser, (const unsigned char*)document, strlen(document));
	
	yaml_document_t *conf_doc = malloc(sizeof(yaml_document_t));
	if ( !conf_doc ) {
		PC_handle_err_tree(PC_make_err(PC_SYSTEM_ERROR, "unable to allocate memory"), err1);
	}
	
	if ( !yaml_parser_load(&conf_parser, conf_doc) ) {
		if ( conf_parser.context ) {
			PC_handle_err_tree(PC_make_err(PC_INVALID_FORMAT,
					"%lu:%lu: Error: %s \n%lu:%lu: Error: %s",
					(unsigned long) conf_parser.problem_mark.line,
					(unsigned long) conf_parser.problem_mark.column,
					conf_parser.problem,
					(unsigned long) conf_parser.context_mark.line,
					(unsigned long) conf_parser.context_mark.column,
					conf_parser.context), err1);
		} else {
			PC_handle_err_tree(PC_make_err(PC_INVALID_FORMAT, "%lu:%lu: Error: %s",
					(unsigned long) conf_parser.problem_mark.line,
					(unsigned long) conf_parser.problem_mark.column,
					conf_parser.problem), err1);
		}
	}
	
	yaml_parser_delete(&conf_parser);
	
	restree = PC_root(conf_doc);
	
	PC_handle_tree(err0);
	
	return restree;
err1:
	yaml_parser_delete(&conf_parser);
err0:
	return restree;
}

PC_tree_t PC_parse_file( FILE *conf_file )
{
	PC_tree_t restree = { PC_OK, NULL, NULL };
	
	yaml_parser_t conf_parser; 
	if ( !yaml_parser_initialize(&conf_parser) ) {
		PC_handle_err_tree(PC_make_err(PC_SYSTEM_ERROR, "unable to load yaml library"), err0);
	}
	
	yaml_parser_set_input_file(&conf_parser, conf_file);
	
	yaml_document_t *conf_doc = malloc(sizeof(yaml_document_t));
	if ( !conf_doc ) {
		PC_handle_err_tree(PC_make_err(PC_SYSTEM_ERROR, "unable to allocate memory"), err1);
	}
	
	if ( !yaml_parser_load(&conf_parser, conf_doc) ) {
		if ( conf_parser.context ) {
			PC_handle_err_tree(PC_make_err(PC_INVALID_FORMAT,
					"%lu:%lu: Error: %s \n%lu:%lu: Error: %s",
					(unsigned long) conf_parser.problem_mark.line,
					(unsigned long) conf_parser.problem_mark.column,
					conf_parser.problem,
					(unsigned long) conf_parser.context_mark.line,
					(unsigned long) conf_parser.context_mark.column,
					conf_parser.context), err1);
		} else {
			PC_handle_err_tree(PC_make_err(PC_INVALID_FORMAT, "%lu:%lu: Error: %s",
					(unsigned long) conf_parser.problem_mark.line,
					(unsigned long) conf_parser.problem_mark.column,
					conf_parser.problem), err1);
		}
	}
	
	yaml_parser_delete(&conf_parser);
	
	restree = PC_root(conf_doc);
	
	PC_handle_tree(err0);
	
	
	
	return restree;
err1:
	yaml_parser_delete(&conf_parser);
err0:
	return restree;
}

PC_tree_t PC_root( yaml_document_t *document )
{
	PC_tree_t restree = { PC_OK, document, yaml_document_get_root_node(document) };
	return restree;
}

PC_tree_t PC_get( const PC_tree_t tree, const char *index_fmt, ... )
{
	va_list ap;
	va_start(ap, index_fmt);
	PC_tree_t res = PC_vget(tree, index_fmt, ap);
	va_end(ap);
	return res;
}

PC_tree_t PC_vget( const PC_tree_t tree, const char *index_fmt, va_list va )
{
	PC_tree_t restree = tree;
	PC_handle_tree(err0);
	
	int index_size = PC_BUFFER_SIZE;
	char *index = malloc(index_size);
	while ( vsnprintf(index, index_size, index_fmt, va) > index_size ) {
		index_size *= 2;
		index = realloc(index, index_size);
	}

	restree = PC_sget(tree, index);
	PC_handle_tree(err1);

	free(index);
	return restree;
	
err1:
	free(index);
err0:
	return restree;
}

PC_status_t PC_len( const PC_tree_t tree, int *res )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	// check type
	if ( !tree.node ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	switch ( tree.node->type ) {
	case YAML_SEQUENCE_NODE: {
		*res = tree.node->data.sequence.items.top - tree.node->data.sequence.items.start;
	} break;
	case YAML_MAPPING_NODE: {
		*res = tree.node->data.mapping.pairs.top - tree.node->data.mapping.pairs.start;
	} break;
	case YAML_SCALAR_NODE: {
		*res = tree.node->data.scalar.length;
	} break;
	default: {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE, "Unknown yaml node type: #%d", tree.node->type), err0);
	} break;
	}
	
	return status;
	
err0:
	return status;
}

PC_status_t PC_int( const PC_tree_t tree, long *res )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	// check type
	if ( !tree.node ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	if ( tree.node->type != YAML_SCALAR_NODE ) {
		return PC_make_err(PC_INVALID_NODE_TYPE, "Expected a scalar, found %s\n", nodetype[tree.node->type]);
	}
	
	char *endptr; long result = strtol((char*)tree.node->data.scalar.value, &endptr, 0);
	if ( *endptr ) {
		char *content; PC_handle_err(PC_string(tree, &content), err0);
		status = PC_make_err(PC_INVALID_NODE_TYPE, "Expected integer, found `%s'\n", content);
		free(content);
		PC_handle_err(status, err0);
	}
	
	*res = result;
	return status;
	
err0:
	return status;
}

PC_status_t PC_double( const PC_tree_t tree, double* value )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	// check type
	if ( !tree.node ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	if ( tree.node->type != YAML_SCALAR_NODE ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected a scalar, found %s\n",
				nodetype[tree.node->type]), err0);
	}
	char *endptr; *value = strtod((char*)tree.node->data.scalar.value, &endptr);
	if ( *endptr ) {
		char *content = NULL; PC_handle_err(PC_string(tree, &content), err0);
		status = PC_make_err(PC_INVALID_PARAMETER, "Expected floating point, found `%s'\n", content);
		free(content);
		PC_handle_err(status, err0);
	}
	
	return status;
	
err0:
	return status;
}

PC_status_t PC_string( const PC_tree_t tree, char** value )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	// check type
	if ( !tree.node ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	if ( tree.node->type != YAML_SCALAR_NODE ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected a scalar, found %s\n",
				nodetype[tree.node->type]), err0);
	}

	int len=0; PC_handle_err(PC_len(tree, &len), err0);

	*value = malloc(len+1);
	strncpy(*value, (char*)tree.node->data.scalar.value, len+1);
	assert((*value)[len]==0);
	
	return status;
	
err0:
	return status;
}

PC_status_t PC_bool( const PC_tree_t tree, int *res )
{
	PC_status_t status = PC_OK;
	PC_handle_tree_err(tree, err0);
	
	// check type
	if ( !tree.node ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected node, found empty tree\n"
		), err0);
	}
	
	if ( tree.node->type != YAML_SCALAR_NODE ) {
		PC_handle_err(PC_make_err(PC_INVALID_NODE_TYPE,
				"Expected a scalar, found %s\n", nodetype[tree.node->type]), err0);
	}

	char *value = (char*)tree.node->data.scalar.value;

	if (
			   !strcmp(value,"True")
			|| !strcmp(value,"true")
			|| !strcmp(value,"TRUE")
			|| !strcmp(value,"Yes")
			|| !strcmp(value,"yes")
			|| !strcmp(value,"YES")
			) {
	  *res = 1;
	} else if (
			   !strcmp(value,"False")
			|| !strcmp(value,"false")
			|| !strcmp(value,"FALSE")
			|| !strcmp(value,"No")
			|| !strcmp(value,"no")
			|| !strcmp(value,"NO")
			) {
		*res = 0;
	} else {
		char *content = NULL; PC_handle_err(PC_string(tree, &content), err0);
		status = PC_make_err(PC_INVALID_PARAMETER, "Expected logical expression, found `%s'\n", content);
		free(content);
		PC_handle_err(status, err0);
	}
	
	return status;
	
err0:
	return status;
}

PC_status_t PC_tree_destroy( PC_tree_t* tree )
{
	yaml_document_delete(tree->document);
	free(tree->document);
	tree->node = NULL;
	return tree->status;
}
