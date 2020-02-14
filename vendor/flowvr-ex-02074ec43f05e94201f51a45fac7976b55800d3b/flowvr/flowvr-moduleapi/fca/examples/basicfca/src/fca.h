/**
 * This file is part of FCA (FlowVR C API).
 *
 * FCA is  free software:  you can  redistribute it and/or  modify it  under the
 * terms of  the GNU General  Public License as  published by the  Free Software
 * Foundation, either  version 3 of the  License, or (at your  option) any later
 * version.
 *
 * FCA  is distributed  in the  hope that  it will  be useful,  but  WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy  of the GNU General Public License along with
 * FCA.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Authors:
 *  Julien Fontanet <julien.fontanet@isonoe.net>
 *
 * @file
 */
#ifndef H_FCA
#define H_FCA

/* For compatibility with Clang. */
#ifndef __has_extension
#	define __has_extension(EXT) 0
#endif

/* Portable macro to mark something as deprecated. */
#if (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)) // GCC >= 3.1
#	define FCA_DEPRECATED(MSG) __attribute__((deprecated))
#elif __has_extension(attribute_deprecated_with_message)
#	define FCA_DEPRECATED(MSG) __attribute__((deprecated(MSG)))
#elif __has_extension(attribute_deprecated) // Clang
#	define FCA_DEPRECATED(MSG) __attribute__((deprecated))
#else
#	define FCA_DEPRECATED(MSG)
#endif

#ifdef __cplusplus
#include <cstddef>
using std::size_t;
extern "C" {
#else /* __cplusplus */
#include <stdbool.h>
#include <stddef.h>
#endif /* __cplusplus */

	/* For each  type a pointer  type to an  unspecified (in fact  not existing)
	 * struct is created. In fact, they are pointers to C++ objects.
	 *
	 * These intermediary structs are used to enforce type safety. */
#	define FCA_DECLARE_TYPE(NAME) \
	typedef struct NAME ## _inexistent *NAME

	/**
	 *
	 */
	FCA_DECLARE_TYPE(fca_message);

	/**
	 * A  module  is an  object  which  is represents  a  FlowVR  module, it  is
	 * connected to  the FlowVR daemon and  provides access to  send and receive
	 * messages.
	 */
	FCA_DECLARE_TYPE(fca_module);

	/**
	 *
	 */
	FCA_DECLARE_TYPE(fca_pool);

	/**
	 * A port is an object through which transit messages.
	 *
	 * There is two type of ports:
	 * - output ports: from which messages are sent;
	 * - input ports: from which messages are received.
	 */
	FCA_DECLARE_TYPE(fca_port);

	/**
	 *
	 */
	FCA_DECLARE_TYPE(fca_stamp);


	/* This macro is no longer needed. */
#	undef FCA_DECLARE_TYPE

	/**
	 * Indicates the type of a “fca_port”.
	 */
	typedef enum {
		fca_IN,
		fca_OUT
	} fca_port_type;

	/**
	 * Flags are used to specify additional properties to a port.
	 */
	typedef char fca_port_flag;

	/**
	 * This  flag  indicates   that  the  related  input  port   does  not  make
	 * “fca_wait()” wait for a message on it.
	 */
	static const fca_port_flag fca_NON_BLOCKING = 1;

	/**
	 * These are the available data types for stamps.
	 *
	 * Contrary to the others, fca_ARRAY is a composed type.
	 */
	typedef enum {
		fca_ARRAY,
		fca_BINARY,
		fca_FLOAT,
		fca_INT,
		fca_STRING
	} fca_stamp_type;

	/**************************************/

	/**
	 * Initializes manually the parallel parameters.
	 *
	 * /!\: This  function must  be used just  before the initialization  of the
	 *      related module.
	 *
	 * /!\: This function is not thread safe.
	 *
	 * @param rank
	 */
	void
	fca_init_parallel(size_t rank, size_t nprocs);

	/**************************************/

	/**
	 * Frees a message/module/pool.
	 *
	 * /!\ There is no type safety for this parameter.
	 */
	void
	fca_free(void *object);

	/**************************************/

	/**
	 * Creates and initialize a FlowVR module.
	 *
	 *     mod = fca_new_module("in", fca_INPUT, fca_NON_BLOCKING,
	 *                          "out", fca_OUTPUT, 0,
	 *                          NULL);
	 */
	fca_module
	fca_new_module(const char *name, ...);

	/**
	 *
	 */
	fca_message
	fca_get(fca_port port);

	/**
	 * “Sends” the message through the given port associated the given module.
	 *
	 * /!\: Port must be a valid input port for this module.
	 *
	 * @return Whether the sending succeeded.
	 */
	bool
	fca_put(fca_port port, fca_message message);

	/**
	 * Waits for a message to be  received on each connected ports of the module
	 * or an error or the request from FlowVR to terminate.
	 *
	 * @return Whether we received a message on each connected ports.
	 */
	bool
	fca_wait(fca_module module);

	/**************************************/

	/**
	 * Gets the module's port which has this name.
	 */
	fca_port
	fca_get_port(fca_module module, const char *name);

	/**
	 * Gets the type of this port.
	 */
	fca_port_type
	fca_get_port_type(fca_port port);

	/**************************************/

	/**
	 * Allocates a new message with a segment of a given size.
	 *
	 * @param allocator The allocator to use (module or buffer pool).
	 * @param size      The size of the initial segment.
	 *
	 * @return The message if the allocation succeed, NULL otherwise.
	 */
	fca_message
	fca_new_message(void *allocator, size_t size);

	/**
	 * Gets the number of segments the message is composed of.
	 */
	size_t
	fca_number_of_segments(const fca_message message);

	/**
	 * Adds a new segment of a given size at the end of the message.
	 *
	 * @param allocator The allocator to use (module or buffer pool).
	 * @param message   The message to which add a segment.
	 * @param size      The size of the segment.
	 *
	 * @return Wether the allocation succeeded.
	 */
	bool
	fca_add_segment(void *allocator, fca_message message, size_t size);

	/**
	 * Removes a segment of the message.
	 */
	bool
	fca_remove_segment(fca_message message, size_t segment);

	/**
	 * Resizes a segment of the given message.
	 *
	 * @return Whether the resizing succeeded.
	 */
	bool
	fca_resize_segment(fca_message message, size_t segment, size_t size);

	/**
	 * Gets the size of one of the message's segment.
	 */
	size_t
	fca_get_segment_size(const fca_message message, size_t segment);

	/**
	 * Returns a read-only pointer to the segment's data.
	 */
	const void *
	fca_get_read_access(const fca_message message, size_t segment);

	/**
	 * Returns  a pointer to  the segment's  data if  this message  is writable,
	 * otherwise NULL is returned.
	 */
	void *
	fca_get_write_access(fca_message message, size_t segment);

	/**************************************/

	/**
	 * Registers a new stamp to a port.
	 *
	 * Some types are parametrized:
	 * - fca_ARRAY:
	 *   - number of items,
	 *   - type of the items,
	 *   - parameters for this type;
	 * - fca_BINARY:
	 *   - data size in bytes.
	 *
	 * @param port The port to which the stamp will be registered.
	 * @param name The name of the stamp (must be unique).
	 * @param type The data type of the stamp.
	 * @param ...  Type parameters (depends of the type).
	 *
	 * @return The  new stamp if  the registration succeeded, otherwise  a “null
	 *         object”.
	 */
	fca_stamp
	fca_register_stamp(fca_port port, const char *name,
	                   fca_stamp_type type, ...);

	/**
	 * Gets the stamp registered with the port “port” with the name “name”.
	 *
	 * @param port
	 * @param name
	 *
	 * @return The matching stamp or a “null object”.
	 */
	fca_stamp
	fca_get_stamp(fca_port port, const char *name);

	/**
	 * Writes a stamp with a certain value in a message.
	 *
	 * @param message The message to which the stamp must be written.
	 * @param stamp   The stamp to write.
	 * @param value   The value to write.
	 *
	 * @return Whether the the writing succeeded.
	 */
	bool
	fca_write_stamp(fca_message message, const fca_stamp stamp,
	                void *value);

	/**
	 * Reads a stamp from a message.
	 *
	 * @param message The message from which the stamp must be read.
	 * @param stamp   The stamp to read.
	 *
	 * @return A pointer to an allocated memory containing the result or NULL if
	 *         it did not succeeded.
	 */
	void *
	fca_read_stamp(fca_message message, const fca_stamp stamp);

	/**
	 * Gets the name of a stamp.
	 *
	 * @param stamp The stamp to get the name from.
	 *
	 * @return The name of the stamp.
	 */
	const char *
	fca_get_stamp_name(const fca_stamp stamp);

	/**
	 * Gets the type of a stamp.
	 *
	 * This function only  returns the primary type of  the stamp (for instance,
	 * only fca_ARRAY for an array of integers).
	 *
	 * @param stamp The stamp to get the type from.
	 *
	 * @return The type of the stamp.
	 */
	fca_stamp_type
	fca_get_stamp_type(const fca_stamp stamp);

	/**************************************/

	/**
	 * Creates a new buffer pool.
	 *
	 * @param module The module this pool is associated to.
	 * @param capacity  The capacity, i.e. the  number of buffers  this pool can
	 *                  handles.
	 *
	 * @return The new buffer pool.
	 */
	fca_pool
	fca_new_pool(fca_module module, size_t capacity);

	/**
	 * Gets the current pool capacity.
	 *
	 * @param pool The buffer pool we want to know the capacity of.
	 *
	 * @return The current capacity.
	 */
	size_t
	fca_get_pool_capacity(const fca_pool pool);

	/**
	 * Sets a new capacity.
	 *
	 * @param pool     The buffer pool we want to set the capacity to.
	 * @param capacity The new capacity.
	 */
	void
	fca_set_pool_capacity(fca_pool pool, size_t capacity);

#undef FCA_DEPRECATED

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* H_FCA */
