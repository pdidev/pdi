// SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "config.h"

#include <iostream>
#include <memory>

#include <dlfcn.h>

#include "pdi/error.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"

#include "pdi/context.h"

namespace PDI {

using std::move;
using std::string;
using std::unique_ptr;
using std::unordered_map;

Context::Iterator::Iterator(const unordered_map<string, unique_ptr<Data_descriptor>>::iterator& data)
	: m_data(data)
{}

Context::Iterator::Iterator(unordered_map<string, unique_ptr<Data_descriptor>>::iterator&& data)
	: m_data(move(data))
{}

Data_descriptor* Context::Iterator::operator->()
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

bool Context::Iterator::operator== (const Iterator& o)
{
	return (m_data == o.m_data);
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

} // namespace PDI
