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

#include "config.h"

#include <iostream>
#include <memory>

#include <dlfcn.h>

#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"

#include "pdi/context.h"


namespace PDI {

using std::move;
using std::string;
using std::unordered_map;
using std::unique_ptr;

Context::Iterator::Iterator(const unordered_map<string, unique_ptr<Data_descriptor>>::iterator& data):
	m_data(data)
{}

Context::Iterator::Iterator(unordered_map<string, unique_ptr<Data_descriptor>>::iterator&& data):
	m_data(move(data))
{}

Data_descriptor* Context::Iterator::operator-> ()
{
	return m_data->second.get();
}

Data_descriptor& Context::Iterator::operator* ()
{
	return *m_data->second;
}

Context::Iterator& Context::Iterator::operator++ ()
{
	++m_data;
	return *this;
}

bool Context::Iterator::operator!= (const Iterator& o)
{
	return (m_data != o.m_data);
}

Context::Iterator Context::get_iterator(const std::unordered_map<std::string, unique_ptr<Data_descriptor>>::iterator& data)
{
	return data;
}

Context::Iterator Context::get_iterator(std::unordered_map<std::string, unique_ptr<Data_descriptor>>::iterator&& data)
{
	return move(data);
}

Context::~Context() = default;

}
