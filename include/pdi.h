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
  
//The following is used for doxygen documentation:
 /**
 * \file pdi.h
 * \brief Header of the PDI API.
 * \author J. Bigot (CEA)
 */

#ifndef PDI_H__
#define PDI_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <paraconf.h>
#include <mpi.h>

#include <pdi_export.h>

/// Error handling \{

/** Error codes of PDI
 */
typedef enum PDI_status_e {
	PDI_OK=0,
	/// on an input call, no such data is available
	PDI_UNAVAILABLE,
	/// The configuration file is invalid
	PDI_ERR_CONFIG,
	/// A value expression is invalid
	PDI_ERR_VALUE,
	/// Tried to load a non-existing plugin
	PDI_ERR_PLUGIN,
	/// Implementation limitation (typically an unimplemented feature)
	PDI_ERR_IMPL,
	/// A system error occured (OS, MPI, etc.)
	PDI_ERR_SYSTEM
} PDI_status_t;

/** Type of a callback function used when an error occurs
 * \param status the error code
 * \param message the human-readable error message
 * \param context a user-provided context
 */
typedef void (*PDI_errfunc_f)(PDI_status_t status, const char *message, void *context);

/** Definition of an error handler
 */
typedef struct PDI_errhandler_s
{
	/// The function to handle the error (none if NULL)
	PDI_errfunc_f func;
	
	/// the context that will be provided to the function
	void *context;
	
} PDI_errhandler_t;

/** Return a human-readabe message describing the last error that occured in PDI
 */
char PDI_EXPORT *PDI_errmsg();

/** Sets the error handler to use
 * 
 * PDI_asserthandler is the default handler before this function is called
 * 
 * \param handler the new handler to set
 * \return the previous handler
 */
PDI_errhandler_t PDI_errhandler(PDI_errhandler_t handler);

/** Prints the error message and aborts if the status is invalid
 */
extern const PDI_errhandler_t PDI_ASSERT_HANDLER;

/** Does nothing
 */
extern const PDI_errhandler_t PDI_NULL_HANDLER;

/// \}

/// \{ Initialization / Finalization stuff

/** Initializes PDI
 * \param[in] conf the configuration
 * \param[in,out] world the main MPI communicator
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_init(PC_tree_t conf, MPI_Comm* world);

/** Finalizes PDI
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_finalize();

/// \}

/** Triggers a PDI "event"
 * \param[in] event the event name
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_event(const char *event);

/// \{ data access

/**
 * Access directions
 */
typedef enum PDI_inout_e {
	/// data tranfer from PDI to the main code
	PDI_IN = 1,
	/// data transfer from the main code to PDI
	PDI_OUT = 2
} PDI_inout_t;

/** Shares some data with PDI. The user code should not modify it before
 * a call to either PDI_release or PDI_reclaim.
 * \param[in] name the data name
 * \param[in,out] data the accessed data
 * \param[in] access whether the data can be accessed for read or write 
 *                   by PDI
 * \return an error status
 * \pre the user code owns the data buffer
 * \post ownership of the data buffer is shared between PDI and the user code
 * 
 * the access parameter is a binary OR of PDI_IN & PDI_OUT.
 * * PDI_IN means PDI can set the buffer content
 * * PDI_OUT means the buffer contains data that can be accessed by PDI
 */
PDI_status_t PDI_EXPORT PDI_share(const char *name, void *data, int access);

/** Releases ownership of a data shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 * \param[in] name name of the data to release
 * \return an error status
 * \pre ownership of the data buffer is shared between PDI and the user code
 * \pre PDI owns the data buffer
 */
PDI_status_t PDI_EXPORT PDI_release(const char *name);

/** Reclaims ownership of a data buffer shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 * \param[in] name name of the data to reclaim
 * \return an error status
 * \pre ownership of the data buffer is shared between PDI and the user code
 * \post the user code owns the data buffer
 */
PDI_status_t PDI_EXPORT PDI_reclaim(const char *name);

/** Requests for PDI to allocate (and initialize) a data buffer.
 * \param[in] name the data name
 * \param[in,out] data the accessed data
 * \return an error status
 * \post the user code owns the data buffer
 */
// PDI_status_t PDI_EXPORT PDI_alloc(const char *name, void **data);

/// \}

/// \{ combined in/out data access & memory ownership

/** Exports some data to PDI. Equivalent to PDI_share(OUT) + PDI_release.
 * \param[in] name the data name
 * \param[in] data the exported data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_export(const char *name, const void *data);

/** Shortly exposes some data to PDI. Equivalent to PDI_share(OUT) + PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the exposed data
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_expose(const char *name, const void *data);

/** Imports some data from PDI. Equivalent to PDI_share(IN) + PDI_reclaim.
 * \param[in] name the data name
 * \param[out] data the data to initialize
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_import(const char *name, void *data);

/// \}

/// \{

/** Begin a transaction. All the ::PDI_expose will be exposed together.
 * 
 * This requires a ::PDI_transaction_end to close the transaction.
 * 
 * \param[in] name the name of the transaction (an event thus named will be
 *                 triggered when all data become available)
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_transaction_begin( const char *name );

/** Ends the previously opened transaction.
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_transaction_end();

/// \}

#ifdef __cplusplus
} // extern C
#endif

#endif // PDI_H__
