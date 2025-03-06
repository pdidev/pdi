/*******************************************************************************
* Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

/** \file pdi.h for deactivation
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

#ifndef PDI_NULL_HANDLER_DEFINED
#define PDI_NULL_HANDLER_DEFINED
/** Does nothing
 */
const PDI_errhandler_t PDI_NULL_HANDLER = {NULL, NULL};

/** Prints the error message and aborts if the status is invalid
 */
const PDI_errhandler_t PDI_ASSERT_HANDLER = PDI_NULL_HANDLER;

/** Prints the error message and continue if the status is invalid
 */
const PDI_errhandler_t PDI_WARN_HANDLER = PDI_NULL_HANDLER;
#endif

/** Return a human-readabe message describing the last error that occured in PDI
 */
static inline const char* PDI_errmsg(void)
{
	return "";
}

/** Sets the error handler to use
 */
static inline PDI_errhandler_t PDI_errhandler(PDI_errhandler_t)
{
	return PDI_NULL_HANDLER;
}

/// \}

/** \addtogroup init_final Initialization and finalization
 *
 * The initialization and finalization part of the API is used to setup PDI,
 * release its resources and check version information.
 * \{
 */

/** Initializes PDI
 */
static inline PDI_status_t PDI_init(PC_tree_t)
{
	return PDI_OK;
}

/** Finalizes PDI
 * \return an error status
 */
static inline PDI_status_t PDI_finalize(void)
{
	return PDI_OK;
}

/** Checks PDI API version
 */
static inline PDI_status_t PDI_version(unsigned long*, unsigned long)
{
	return PDI_OK;
}

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
 */
static inline PDI_status_t PDI_share(const char*, void*, PDI_inout_t)
{
	return PDI_OK;
}

/** Requests for PDI to access a data buffer.
 */
static inline PDI_status_t PDI_access(const char*, void**, PDI_inout_t)
{
	return PDI_OK;
}

/** Releases ownership of a data shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 */
static inline PDI_status_t PDI_release(const char*)
{
	return PDI_OK;
}

/** Reclaims ownership of a data buffer shared with PDI. PDI does not manage
 * the buffer memory anymore.
 */
static inline PDI_status_t PDI_reclaim(const char*)
{
	return PDI_OK;
}

/** Triggers a PDI "event"
 */
static inline PDI_status_t PDI_event(const char*)
{
	return PDI_OK;
}

/** Shortly exposes some data to PDI. Equivalent to PDI_share + PDI_reclaim.
 */
static inline PDI_status_t PDI_expose(const char*, void*, PDI_inout_t)
{
	return PDI_OK;
}

/** Performs multiple exposes at once. All the data is shared in order they were specified
 *  and reclaimed in reversed order after an event is triggered.
 *
 *  NULL argument indicates an end of the list.
 */
static inline PDI_status_t PDI_multi_expose(const char*, const char*, void*, PDI_inout_t, ...)
{
	return PDI_OK;
}

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
 * \return an error status
 */
static inline PDI_status_t PDI_transaction_begin(const char*)
{
	return PDI_OK;
}

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
static inline PDI_status_t PDI_transaction_end(void)
{
	return PDI_OK;
}

#endif // PDI_WITH_DEPRECATED

/// \}

#ifdef __cplusplus
} // extern C
#endif


#endif // PDI_H_
