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

#ifndef PDI_H__
#define PDI_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <yaml.h>
#include <mpi.h>

#include "pdi_export.h"

typedef enum PDI_status_e {
	PDI_OK=0,
	/// on an input call, no such data is available
	PDI_UNAVAILABLE,
	/// The configuration file is invalid
	PDI_CONFIG_ERROR,
	/// A value expression is invalid
	PDI_VALUE_ERROR
} PDI_status_t;

/// \{ Initialization / Finalization stuff

/** Initializes PDI
 * \param[in] conf the configuration
 * \param[in,out] world the main MPI communicator
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_init(yaml_document_t* document, yaml_node_t* conf, MPI_Comm* world);

/** Finalizes PDI
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_finalize();

/// \}

/** Triggers a PDI “event”
 * \param[in] event the event name
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_event(const char *event);

/// \{ in/out data access

/** Let the code take a look at some data. Neither PDI nor the user code
 * should modify it before a call to either PDI_release or PDI_reclaim.
 * \param[in] name the data name
 * \param[out] data the accessed data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_access(const char *name, void *data);

//TODO: a version of access where memory is allocated by PDI

/** Shares some data with PDI. Neither PDI nor the user code should modify it
 * before a call to either PDI_release or PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the shared data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_share(const char *name, const void *data);

/// \}

/// \{ memory ownership management

/** Releases ownership of a data shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 * \param[in] name name of the data to release
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_release(const char *name);

/** Exposes a value to PDI
 * \param[in] name name of the data to reclaim
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_reclaim(const char *name);

/// \}

/// \{ combined in/out data access & memory ownership

/** Exports some data to PDI. Equivalent to PDI_share + PDI_release.
 * \param[in] name the data name
 * \param[in] data the exported data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_export(const char *name, const void *data);

/** Shortly exposes some data to PDI. Equivalent to PDI_share + PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the exposed data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_expose(const char *name, const void *data);

/** Imports some data from PDI. Equivalent to PDI_access + PDI_reclaim.
 * \param[in] name the data name
 * \param[out] data the data to initialize
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_import(const char *name, void *data);

//TODO: a version of import where memory is allocated by PDI

/// \}

#ifdef __cplusplus
} // extern C
#endif

#endif // PDI_H__
