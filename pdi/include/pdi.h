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

/** \file pdi.h
 *
 * C user API
 *
 * The user facing API is the interface offered by PDI to C application
 * developers.
 *
 * \defgroup init_final Initialization and finalization
 *
 * The initialization and finalization part of the API is used to setup PDI,
 * release its resources and check version information.
 *
 * \defgroup annotation Code annotation
 *
 * The code annotation API is the main interface to use in the code.
 *
 * It offers functions that can be called from code with no side effect by
 * default and that can therefore be considered as annotations.
 *
 * \defgroup error Error handling
 *
 * The error handling API supports checking the error status of PDI.
 *
 * By default, errors in PDI C API are signaled by a return code of type
 * PDI_status_t and an error message can be retrieved with the PDI_errmsg
 * function. This default behavior can be changed by replacing the error handler
 * with the PDI_errhandler function.
 * 
 */

#ifndef PDI_H_
#define PDI_H_

#include <paraconf.h>

#include <pdi/export.h>
#include <pdi/version.h>

//#include "/gpfs/softs/spack_0.17/opt/spack/linux-centos7-cascadelake/gcc-11.2.0/openmpi-4.1.1-ujlwnrlh5sewm2rxkpio3h5mariwgetn/include/mpi.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \addtogroup error
 * \{
 */

/** Error codes of PDI
 */
typedef enum PDI_status_e {
	/// everything went well
	PDI_OK = 0,
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
	/// A system error occured (OS, etc.)
	PDI_ERR_SYSTEM,
	/** A call to a function has been made at a wrong time (e.g. closing an
	 *  unopened transaction)
	 */
	PDI_ERR_STATE,
	/// A conflict of onwership over a content has been raised
	PDI_ERR_RIGHT,
	/// Invalid type error
	PDI_ERR_TYPE

} PDI_status_t;

/** Type of a callback function used when an error occurs
 * \param status the error code
 * \param message the human-readable error message
 * \param context a user-provided context
 */
typedef void (*PDI_errfunc_f)(PDI_status_t status, const char* message, void* context);

/** Definition of an error handler
 */
typedef struct PDI_errhandler_s {
	/// The function to handle the error (none if NULL)
	PDI_errfunc_f func;

	/// the context that will be provided to the function
	void* context;

} PDI_errhandler_t;

/** Prints the error message and aborts if the status is invalid
 */
extern const PDI_errhandler_t PDI_EXPORT PDI_ASSERT_HANDLER;

/** Prints the error message and continue if the status is invalid
 */
extern const PDI_errhandler_t PDI_EXPORT PDI_WARN_HANDLER;

/** Does nothing
 */
extern const PDI_errhandler_t PDI_EXPORT PDI_NULL_HANDLER;


/** Return a human-readabe message describing the last error that occured in PDI
 */
const char PDI_EXPORT* PDI_errmsg(void);

/** Sets the error handler to use
 *
 * PDI_asserthandler is the default handler before this function is called
 *
 * \param handler the new handler to set
 * \return the previous handler
 */
PDI_errhandler_t PDI_EXPORT PDI_errhandler(PDI_errhandler_t handler);

/// \}

/** \addtogroup init_final Initialization and finalization
 *
 * The initialization and finalization part of the API is used to setup PDI,
 * release its resources and check version information.
 * \{
 */

/** Initializes PDI
 * \param[in] conf the configuration
 * \return an error status
 */
//PDI_status_t PDI_EXPORT PDI_init(PC_tree_t conf,const char* configfile, int *is_client,MPI_Comm *damaris_communicator);
PDI_status_t PDI_EXPORT PDI_init(PC_tree_t conf,const char* configfile, int *is_client);


/** Finalizes PDI
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_finalize(int);

//PDI_status_t PDI_EXPORT PDI_finalize_Damaris(int *is_client);

/** Checks PDI API version
 *
 * \param[out] provided version if non-null it is filled with the provided API version
 * \param[in] expected if non-zero the expected API version
 * \return an error status if the expected version is incompatible with the
 * provided one
 */
PDI_status_t PDI_EXPORT PDI_version(unsigned long* provided, unsigned long expected);

/// \}

/** \addtogroup annotation
 * \{
 */

/**
 * Access directions
 */
typedef enum PDI_inout_e {
	/// No data transfert
	PDI_NONE = 0,
	/// data tranfer from PDI to the main code
	PDI_IN = 1,
	/// data transfer from the main code to PDI
	PDI_OUT = 2,
	/// data transfer in both direction
	PDI_INOUT = 3

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
PDI_status_t PDI_EXPORT PDI_share(const char* name, void* data, PDI_inout_t access);

/** Requests for PDI to access a data buffer.
 * \param[in] name the data name
 * \param[in,out] buffer a pointer to the accessed data buffer
 * \param[in] inout the access properties (PDI_IN, PDI_OUT, PDI_INOUT)
 * \return an error status
 * \pre PDI owns the data buffer
 * \post ownership of the data buffer is shared between PDI and the user code
 */
PDI_status_t PDI_EXPORT PDI_access(const char* name, void** buffer, PDI_inout_t inout);

/** Releases ownership of a data shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 * \param[in] name name of the data to release
 * \return an error status
 * \pre ownership of the data buffer is shared between PDI and the user code
 * \pre PDI owns the data buffer
 */
PDI_status_t PDI_EXPORT PDI_release(const char* name);

/** Reclaims ownership of a data buffer shared with PDI. PDI does not manage
 * the buffer memory anymore.
 * \param[in] name name of the data to reclaim
 * \return an error status
 * \pre ownership of the data buffer is shared between PDI and the user code
 * \post the user code owns the data buffer
 */
PDI_status_t PDI_EXPORT PDI_reclaim(const char* name);

/** Triggers a PDI "event"
 * \param[in] event the event name
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_event(const char* event);

/** Shortly exposes some data to PDI. Equivalent to PDI_share + PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the exposed data
 * \param[in] access whether the data can be accessed for read or write
 *                   by PDI
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_expose(const char* name, void* data, PDI_inout_t access);

/** Performs multiple exposes at once. All the data is shared in order they were specified
 *  and reclaimed in reversed order after an event is triggered.
 *
 *  NULL argument indicates an end of the list.
 *
 * \param[in] event_name the name of the event that will be triggered when
 *                       all data become available
 * \param[in] name the data name
 * \param[in] data the exposed data
 * \param[in] access whether the data can be accessed for read or write by PDI
 * \param[in] ... (additional arguments) additional list of data to expose,
 *                each should contain name, data and access, NULL argument
 *                inidactes an end of the list.
 * \return an error status
 */
PDI_status_t PDI_EXPORT PDI_multi_expose(const char* event_name, const char* name, void* data, PDI_inout_t access, ...);

#ifdef PDI_WITH_DEPRECATED

/** Begin a transaction in which all PDI_expose calls are grouped.
 *
 * This requires a call to PDI_transaction_end to close the transaction.
 *
 * \deprecated the transaction part of the API is deprecated, the
 * PDI_multi_expose function should be used instead.
 *
 * \see PDI_expose the function used to expose data inside the transaction
 * \see PDI_transaction_end the function used to end the transaction
 *
 * \param[in] name the name of the transaction (an event thus named will be
 *                 triggered when all data become available)
 * \return an error status
 */
PDI_status_t PDI_DEPRECATED_EXPORT PDI_transaction_begin(const char* name);

/** Ends the previously opened transaction.
 *
 * \deprecated the transaction part of the API is deprecated, the
 * PDI_multi_expose function should be used instead.
 *
 * \see PDI_transaction_begin the function used to start the transaction
 * \see PDI_expose the function used to expose data inside the transaction
 *
 * \return an error status
 */
PDI_status_t PDI_DEPRECATED_EXPORT PDI_transaction_end(void);

#endif // PDI_WITH_DEPRECATED

/// \}

#ifdef __cplusplus
} // extern C
#endif


#endif // PDI_H_
