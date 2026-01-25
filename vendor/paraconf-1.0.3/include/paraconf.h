/* Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
 *               root of the project or at https://github.com/pdidev/paraconf
 * 
 * SPDX-License-Identifier: MIT
 */

#ifndef PARACONF_H__
#define PARACONF_H__

#include <string.h>
#include <stdarg.h>
#include <yaml.h>

#include "paraconf_export.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \file paraconf.h
 */

/** Status of function execution
 */
typedef enum PC_status_e {
	/// no error
	PC_OK=0,
	/// a parameter value is invalid
	PC_INVALID_PARAMETER,
	/// unexpected type found for a node
	PC_INVALID_NODE_TYPE,
	/// The requested node doen't exist in the tree
	PC_NODE_NOT_FOUND,
	/// The provided input is invalid
	PC_INVALID_FORMAT,
	/// An error occured with the system
	PC_SYSTEM_ERROR
} PC_status_t;

/** Type of a callback function used when an error occurs
 * \param status the error code
 * \param message the human-readable error message
 * \param context a user-provided context
 */
typedef void (*PC_errfunc_f)(PC_status_t status, const char *message, void *context);

/** Definition of an error handler
 */
typedef struct PC_errhandler_s
{
	/// The function to handle the error (none if NULL)
	PC_errfunc_f func;
	
	/// the context that will be provided to the function
	void *context;
	
} PC_errhandler_t;

/** An opaque type describing a yaml tree (possibly a leaf node)
 */
typedef struct PC_tree_s
{
	/// The tree status
	PC_status_t status;
	
	/// The document containing the tree
	yaml_document_t* document;
	
	/// the node inside the tree
	yaml_node_t* node;

} PC_tree_t;

/** Prints the error message and aborts
 */
extern const PARACONF_EXPORT PC_errhandler_t PC_ASSERT_HANDLER;

/** Does nothing
 */
extern const PARACONF_EXPORT PC_errhandler_t PC_NULL_HANDLER;

/** check the status of a tree
 * \param tree the tree to check
 * \return the status
 */
static inline PC_status_t PC_status(PC_tree_t tree) { return tree.status; }

/** Return a human-readabe message describing the last error that occured in paraconf
 */
char PARACONF_EXPORT *PC_errmsg();

/** Sets the error handler to use
 *
 * PC_assert is the default handler before this function is called
 *
 * \param handler the new handler to set
 * \return the previous handler
 */
PC_errhandler_t PARACONF_EXPORT PC_errhandler(PC_errhandler_t handler);

/** Returns the tree as found in a file identified by its path
 *
 * This only supports single document files. Use yaml and PC_root to handle
 * multi-document files
 *
 * The tree created must be destroyed with PC_tree_destroy at the end.
 *
 * \param[in] path the file path as a character string
 * \return the tree, valid as long as the containing document is
 */
PC_tree_t PARACONF_EXPORT PC_parse_path(const char *path);

/** Returns the tree as found in an already open file
 *
 * This only supports single document files. Use yaml and PC_root to handle
 * multi-document files
 *
 * The tree created must be destroyed with PC_tree_destroy at the end.
 *
 * \param[in] file the file containing the tree
 * \return the tree, valid as long as the containing document is
 */
PC_tree_t PARACONF_EXPORT PC_parse_file(FILE *file);

/** Returns the tree contained in a character string
 *
 * This only supports single document strings. Use yaml and PC_root to handle
 * multi-document strings
 *
 * The tree created must be destroyed with PC_tree_destroy at the end.
 *
 * \param[in] document the document as a character string to parse
 * \return the tree, valid as long as the containing document is
 */
PC_tree_t PARACONF_EXPORT PC_parse_string(const char *document);

/** Returns the tree at the root of a document
 *
 * \param[in] document the yaml document
 * \return the tree, valid as long as the containing document is
 */
PC_tree_t PARACONF_EXPORT PC_root(yaml_document_t* document);

/** Looks for a node in a yaml document given a ypath index
 *
 * Does nothing if the provided tree is in error and returns the input tree.
 *
 * A ypath expression can contain the following
 * * access to a mapping element using the dot syntax:
 *   e.g. .map.key
 * * access to a sequence element using square brackets (indices are 0-based):
 *   e.g. .seq[1]
 * * access to a mapping element key using braces (indices are 0-based):
 *   e.g. .map{1}
 * * access to a mapping element value by index using chevrons:
 *   e.g. .map<1>
 *   PC_get(0,map,"<1>"); is similar to k=PC_get(0,map,"{1}"); PC_get(0,map,".%s",k);
 *
 * \param[in] tree a yaml tree
 * \param[in] index_fmt the ypath index, can be a printf-style format string
 * \param[in] ... the printf-style values
 * \return the subtree corresponding to the ypath index
 */
PC_tree_t PARACONF_EXPORT PC_get(PC_tree_t tree, const char *index_fmt, ...);

/** Looks for a node in a yaml document given a ypath index
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree a yaml tree
 * \param[in] index_fmt the ypath index, can be a printf-style format string
 * \param[in] va the printf-style values
 * \return the subtree corresponding to the ypath index
 */
PC_tree_t PARACONF_EXPORT PC_vget(PC_tree_t tree, const char *index_fmt, va_list va);

/** Returns the length of a node, for a sequence, the number of nodes, for a mapping, the number of pairs, for a scalar, the string length
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree the sequence or mapping
 * \param[out] value the length
 * \return the status of the execution
 */
PC_status_t PARACONF_EXPORT PC_len(PC_tree_t tree, int *value);

/** Returns the int value of a scalar node
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree the int-valued node
 * \param[out] value the int value of the scalar node
 * \return the status of the execution
 */
PC_status_t PARACONF_EXPORT PC_int(PC_tree_t tree, long *value);

/** Returns the floating point value of a scalar node
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree the floating-point-valued node
 * \param[out] value the floating point value of the scalar node
 * \return the status of the execution
 */
PC_status_t PARACONF_EXPORT PC_double(PC_tree_t tree, double *value);

/** Returns the string content of a scalar node
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree the node
 * \param[out] value the content of the scalar node as a newly allocated string that must be deallocated using free
 * \return the status of the execution
 */
PC_status_t PARACONF_EXPORT PC_string(PC_tree_t tree, char **value);

/** Returns the boolean value of a scalar node
 *
 * Does nothing if the provided tree is in error
 *
 * \param[in] tree the node
 * \param[out] value the logical value (false=0, true=1) of the scalar node
 * \return the status of the execution
 */
PC_status_t PARACONF_EXPORT PC_bool(PC_tree_t tree, int *value);

/** Destroy the tree.
 * All the trees referring to this tree will become unusable
 * Does nothing if the provided tree is in error
 *
 * \param[in,out] tree the node
 * \return the status of the tree
 */
PC_status_t PARACONF_EXPORT PC_tree_destroy(PC_tree_t* tree);

#ifdef __cplusplus
}
#endif

#endif // PARACONF_H__
