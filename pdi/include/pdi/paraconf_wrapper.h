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

/** \file pdi/paraconf_wrapper.h
 * C++ wrapper for the paraconf API
 */

#ifndef PDI_PARACONF_WRAPPER_H_
#define PDI_PARACONF_WRAPPER_H_

#include <functional>
#include <string>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>

namespace PDI {

/** Automatically installs a paraconf error-handler that ignores errors and
 *  uninstalls it on destruction.
 */
struct PDI_EXPORT Paraconf_wrapper {
	PC_errhandler_t m_handler;
	
	
	Paraconf_wrapper();
	
	~Paraconf_wrapper();
};

/** Returns the length of a node.
 *
 * - for a sequence: the number of nodes,
 * - for a mapping: the number of pairs,
 * - for a scalar: the string length.
 *
 * throws an Error if the provided tree is in error
 *
 * \param[in] tree the sequence or mapping
 * \return the length
 */
int PDI_EXPORT len(PC_tree_t tree);

/** Returns the length of a node.
 *
 * - for a sequence: the number of nodes,
 * - for a mapping: the number of pairs,
 * - for a scalar: the string length.
 *
 * returns the default value in case of error
 *
 * \param[in] tree the sequence or mapping
 * \param[in] dflt the default value in case of error
 * \return the length
 */
int PDI_EXPORT len(PC_tree_t tree, int dflt);

/** Returns the int value of a scalar node
 *
 * throws an Error if the provided tree is in error
 *
 * \param[in] tree the int-valued node
 * \return the int value of the scalar node
 */
long PDI_EXPORT to_long(PC_tree_t tree);

/** Returns the int value of a scalar node
 *
 * returns the default value in case of error
 *
 * \param[in] tree the int-valued node
 * \param[in] dflt the default value in case of error
 * \return the int value of the scalar node
 */
long PDI_EXPORT to_long(PC_tree_t tree, long dflt);

/** Returns the floating point value of a scalar node
 *
 * throws an Error if the provided tree is in error
 *
 * \param[in] tree the floating-point-valued node
 * \return the floating point value of the scalar node
 */
double PDI_EXPORT to_double(PC_tree_t tree);

/** Returns the floating point value of a scalar node
 *
 * returns the default value in case of error
 *
 * \param[in] tree the floating-point-valued node
 * \param[in] dflt the default value in case of error
 * \return the floating point value of the scalar node
 */
double PDI_EXPORT to_double(PC_tree_t tree, double dflt);

/** Returns the string content of a scalar node
 *
 * throws an Error if the provided tree is in error
 *
 * \param[in] tree the node
 * \return the content of the scalar node
 */
std::string PDI_EXPORT to_string(PC_tree_t tree);

/** Returns the string content of a scalar node
 *
 * returns the default value in case of error
 *
 * \param[in] tree the node
 * \param[in] dflt the default value in case of error
 * \return the content of the scalar node
 */
std::string PDI_EXPORT to_string(PC_tree_t tree, const std::string& dflt);

/** Returns the boolean value of a scalar node
 *
 * throws an Error if the provided tree is in error
 *
 * \param[in] tree the node
 * \return the boolean value of the scalar node
 */
bool PDI_EXPORT to_bool(PC_tree_t tree);

/** Returns the boolean value of a scalar node
 *
 * returns the default value in case of error
 *
 * \param[in] tree the node
 * \param[in] dflt the default value in case of error
 * \return the boolean value of the scalar node
 */
bool PDI_EXPORT to_bool(PC_tree_t tree, bool dflt);


/** Checks if the tree is a list
 *
 * \param[in] tree the node
 * \return true if tree is a list, false otherwise
 */
bool PDI_EXPORT is_list(PC_tree_t tree);

/** Checks if the tree is a map
 *
 * \param[in] tree the node
 * \return true if tree is a map, false otherwise
 */
bool PDI_EXPORT is_map(PC_tree_t tree);

/** Checks if the tree is a scalar
 *
 * \param[in] tree the node
 * \return true if tree is a scalar, false otherwise
 */
bool PDI_EXPORT is_scalar(PC_tree_t tree);

/** Iterates and apply the provided function to all elements of a PC list
 *
 * \param tree the tree containing the list
 * \param operation the operation to apply to each element of the tree
 */
void PDI_EXPORT each(PC_tree_t tree, std::function<void(PC_tree_t)> operation);

/** Iterates and apply the provided function to all elements of a PC list or
 * directly to the provided element if it is not a list
 *
 * \param tree the tree containing the list or the single element
 * \param operation the operation to apply to the elements
 */
void PDI_EXPORT opt_each(PC_tree_t tree, std::function<void(PC_tree_t)> operation);

/** Iterates and apply the provided function to all elements of a PC mapping
 *
 * \param tree the tree containing the mapping
 * \param operation the operation to apply to each element of the tree
 */
void PDI_EXPORT each(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation);

/** Iterates and apply the provided function to all elements of a PC ordered
 *  mapping
 *
 * \param tree the tree containing the ordered mapping
 * \param operation the operation to apply to each element of the tree
 */
void PDI_EXPORT each_in_omap(PC_tree_t tree, std::function<void(PC_tree_t, PC_tree_t)> operation);

} // namespace PDI

#endif // PDI_PARACONF_WRAPPER_H_
