/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#ifndef DECL_HDF5_RAII_H_
#define DECL_HDF5_RAII_H_

#include <functional>
#include <string>

#include <hdf5.h>

#include <pdi/error.h>

namespace {

herr_t raii_walker(unsigned n, const H5E_error2_t* err_desc, void* client_data)
{
	using std::string;
	string& result = *(static_cast<std::string*>(client_data));
	if ( n == 1 ) result += "; (HDF5 root causes) => ";
	if ( n > 1 ) result += "; => ";
	result += err_desc->desc;
	return 0;
}

void handle_hdf5_err(const char* message=NULL)
{
	std::string h5_errmsg;
	H5Ewalk2(H5E_DEFAULT, H5E_WALK_UPWARD, raii_walker, &h5_errmsg);
	if ( h5_errmsg.empty() ) h5_errmsg = "Unknown error";
	
	if ( !message ) message = "HDF5 ";
	throw PDI::Error{PDI_ERR_SYSTEM, "%s%s", message, h5_errmsg.c_str()};
}

struct Hdf5_error_handler {

	H5E_auto2_t m_old_func;
	
	void* m_old_data;
	
	Hdf5_error_handler()
	{
		if ( 0>H5Eget_auto2(H5E_DEFAULT, &m_old_func, &m_old_data) ) handle_hdf5_err();
		if ( 0>H5Eset_auto2(H5E_DEFAULT, NULL, NULL) ) handle_hdf5_err();
	}
	
	~Hdf5_error_handler()
	{
		if ( 0>H5Eset_auto2(H5E_DEFAULT, m_old_func, m_old_data) ) handle_hdf5_err();
	}
	
};

class Raii_hid
{
public:
	using Destroyer = std::function<void(hid_t)>;
	
private:
	hid_t m_value;
	
	Destroyer m_destroyer;
	
	Raii_hid(const Raii_hid&) = delete;
	
	Raii_hid& operator= (const Raii_hid&) = delete;
	
public:
	Raii_hid():
		m_destroyer{NULL}
	{
	}
	
	Raii_hid(hid_t value, Destroyer destroyer):
		m_value{value},
		m_destroyer{std::move(destroyer)}
	{
	}
	
	Raii_hid(Raii_hid&& moved_from):
		m_value{moved_from.m_value},
		m_destroyer{std::move(moved_from.m_destroyer)}
	{
		moved_from.m_destroyer = NULL;
	}
	
	~Raii_hid()
	{
		if (m_destroyer) m_destroyer(m_value);
	}
	
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
	
	operator hid_t () const
	{
		return m_value;
	}
	
};

template<typename Destroyer>
Raii_hid make_raii_hid(hid_t value, Destroyer&& dst, const char* message=NULL)
{
	using std::move;
	if ( 0>value ) handle_hdf5_err(message);
	return Raii_hid{value, move(dst)};
}

} // namespace <anonymous>

#endif // DECL_HDF5_RAII_H_
