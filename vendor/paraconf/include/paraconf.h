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

#ifndef PARACONF_H__
#define PARACONF_H__

#include <yaml.h>
#include <mpi.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
	/// no error
	PC_OK=0,
	/// a parameter value is invalid
	PC_INVALID_PARAMETER,
	/// unexpected type found for a node
	PC_INVALID_NODE_TYPE
} PC_status;

/** Looks for a node in a yaml document given a ypath index
 * 
 * \param[in] document the yaml document
 * \param[in] index the ypath index
 * \param[out] value the node found
 * \return error status
 */
PC_status PC_get(yaml_node_t *document, const char *index, yaml_node_t *value);

/** Looks for an integer value in a yaml document given a ypath index
 * 
 * \param[in] document the yaml document
 * \param[in] index the ypath index
 * \param[out] value the integer value found
 * \return error status
 */
PC_status PC_get_int(yaml_node_t *document, const char *index, int *value);

/** Looks for a floating point value in a yaml document given a ypath index
 * 
 * \param[in] document the yaml document
 * \param[in] index the ypath index
 * \param[out] value the floating point value found
 * \return error status
 */
PC_status PC_get_double(yaml_node_t *document, const char *index, double *value);

/** Looks for a character string value in a yaml document given a ypath index
 * 
 * \param[in] document the yaml document
 * \param[in] index the ypath index
 * \param[out] value the character string value found
 * \return error status
 */
PC_status PC_get_string(yaml_node_t *document, const char *index, char *value);

PC_status PC_broadcast(yaml_node_t *document, int count, int root, MPI_Comm comm);

#ifdef __cplusplus
}
#endif

#endif // PARACONF_H__
