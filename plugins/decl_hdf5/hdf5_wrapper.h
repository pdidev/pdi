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


#ifndef DECL_HDF5_HDF5_WRAPPER_H_
#define DECL_HDF5_HDF5_WRAPPER_H_

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
	#include <mpi.h>
#endif

#include <functional>
#include <tuple>
#include <utility>

#include <pdi/pdi_fwd.h>

namespace decl_hdf5 {

/** Our HDF5 error handling function.
 *
 * This function looks at the latest HDF5 error and throws an informative
 * PDI:Error accordingly.
 *
 * \param message a message explaining the context where the HD5 error occured
 */
[[noreturn]] void handle_hdf5_err(const char* message=NULL);

/** A RAII-style HDF5 error handler.
 *
 * Creating an instance of this class removes any HDF5 error handler.
 * The original handler is reinstalled when the instance is destroyed.
 */
class Hdf5_error_handler
{
	/// The original handler
	H5E_auto2_t m_old_func;
	
	/// The original handler data
	void* m_old_data;
	
public:
	/** The default (and only) constructor, installs the handler
	 */
	Hdf5_error_handler()
	{
		if ( 0>H5Eget_auto2(H5E_DEFAULT, &m_old_func, &m_old_data) ) handle_hdf5_err();
		if ( 0>H5Eset_auto2(H5E_DEFAULT, NULL, NULL) ) handle_hdf5_err();
	}
	
	/** The destructor
	 */
	~Hdf5_error_handler()
	{
		if ( 0>H5Eset_auto2(H5E_DEFAULT, m_old_func, m_old_data) ) handle_hdf5_err();
	}
	
};

/** A RAII-style wrapper for HDF5 hid_t.
 *
 * This calls the provided destroyer function when the hid_t goes out of scope.
 */
class Raii_hid
{
public:
	/// The type of the destroyer function
	using Destroyer = std::function<void(hid_t)>;
	
private:
	/// The wrapped hid_t
	hid_t m_value;
	
	/// The destroyer function rto call, or null if none
	Destroyer m_destroyer;
	
	/// No copy possible (unique_ptr style)
	Raii_hid(const Raii_hid&) = delete;
	
	/// No copy possible (unique_ptr style)
	Raii_hid& operator= (const Raii_hid&) = delete;
	
public:
	/** Contructs an empty Raii_hid
	 */
	Raii_hid():
		m_destroyer{NULL}
	{
	}
	
	/** Contructs an Raii_hid
	 *
	 * \param value the hid_t
	 * \param destroyer the destroyer function
	 */
	Raii_hid(hid_t value, Destroyer destroyer):
		m_value{value},
		m_destroyer{std::move(destroyer)}
	{
	}
	
	/** Moves a Raii_hid, the moved-from Raii_hid becomes empty.
	 *
	 * \param moved_from the Raii_hid to move from
	 */
	Raii_hid(Raii_hid&& moved_from):
		m_value{moved_from.m_value},
		m_destroyer{std::move(moved_from.m_destroyer)}
	{
		moved_from.m_destroyer = NULL;
	}
	
	/** Destroys a Raii_hid, calls the destroyer if non-empty and provided
	 */
	~Raii_hid()
	{
		if (m_destroyer) m_destroyer(m_value);
	}
	
	/** Moves a Raii_hid, the moved-from Raii_hid becomes empty.
	 *
	 * \param moved_from the Raii_hid to move from
	 * \return *this
	 */
	Raii_hid& operator= (Raii_hid&& moved_from)
	{
		using std::move;
		// destroy ourselves first
		if (m_destroyer) m_destroyer(m_value);
		// then move the parameter into ourselves
		m_value = moved_from.m_value;
		m_destroyer = move(moved_from.m_destroyer);
		moved_from.m_destroyer = NULL;
		return *this;
	}
	
	/** Supports using the Raii_hid as a raw hid_t.
	 *
	 * \return the raw hid_t
	 */
	operator hid_t () const
	{
		return m_value;
	}
	
};

/** Wraps the calling of a HDF5 hid_t creation function and the corresponding
 * destruction function using a Raii_hid.
 *
 * \param value the result of the creation function call
 * \param dst the destruction function
 * \param message a context message to use in case of error
 */
template<typename Destroyer>
Raii_hid make_raii_hid(hid_t value, Destroyer&& dst, const char* message=NULL)
{
	using std::move;
	if ( 0>value ) handle_hdf5_err(message);
	return Raii_hid{value, move(dst)};
}

/** builds a HDF5 dataspace that represents a PDI Datatype
 *
 * \param type the datatype to represent in HDF5
 * \param select whether to create a dense type instead of a type with a selection
 * \return a tuple containing the Raii_hid for (dataspace, datatype)
 */
std::tuple<Raii_hid, Raii_hid> space(PDI::Datatype_sptr type, bool dense=false);

} // namespace decl_hdf5

#endif // DECL_HDF5_HDF5_WRAPPER_H_
