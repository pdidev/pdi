/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <exception>
#include <vector>

#include <spdlog/spdlog.h>

#include "pdi/callbacks.h"
#include "pdi/context.h"
#include "pdi/error.h"

using std::cref;
using std::exception;
using std::function;
using std::string;
using std::vector;

namespace PDI {

Callbacks::Callbacks(Context& ctx):
	m_context{ctx}
{}

function<void()> Callbacks::add_init_callback(const function<void()>& callback)
{
	m_init_callbacks.emplace_back(callback);
	auto it = --m_init_callbacks.end();
	return [it, this]() {
		this->m_init_callbacks.erase(it);
	};
}

function<void()> Callbacks::add_data_callback(const function<void(const string&, Ref)>& callback, const string& name)
{
	if (name.empty()) {
		m_data_callbacks.emplace_back(callback);
		auto it = --m_data_callbacks.end();
		return [it, this]() {
			this->m_data_callbacks.erase(it);
		};
	} else {
		auto it = m_named_data_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_data_callbacks.erase(it);
		};
	}
}

function<void()> Callbacks::add_data_remove_callback(const function<void(const string&, Ref)>& callback, const string& name)
{
	if (name.empty()) {
		m_data_remove_callbacks.emplace_back(callback);
		auto it = --m_data_remove_callbacks.end();
		return [it, this]() {
			this->m_data_remove_callbacks.erase(it);
		};
	} else {
		auto it = m_named_data_remove_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_data_remove_callbacks.erase(it);
		};
	}
}

function<void()> Callbacks::add_event_callback(const function<void(const string&)>& callback, const string& name)
{
	if (name.empty()) {
		m_event_callbacks.emplace_back(callback);
		auto it = --m_event_callbacks.end();
		return [it, this]() {
			this->m_event_callbacks.erase(it);
		};
	} else {
		auto it = m_named_event_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_event_callbacks.erase(it);
		};
	}
}

function<void()> Callbacks::add_empty_desc_access_callback(const function<void(const string&)>& callback, const string& name)
{
	if (name.empty()) {
		m_empty_desc_access_callbacks.emplace_back(callback);
		auto it = --m_empty_desc_access_callbacks.end();
		return [it, this]() {
			this->m_empty_desc_access_callbacks.erase(it);
		};
	} else {
		auto it = m_named_empty_desc_access_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_empty_desc_access_callbacks.erase(it);
		};
	}
}


void Callbacks::call_init_callbacks() const
{
	for (auto&& init_callback : m_init_callbacks) {
		init_callback();
	}
}

void Callbacks::call_data_callbacks(const string& name, Ref ref) const
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&, Ref)>>> data_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_data_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		data_callbacks.emplace_back(std::cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_data_callbacks.begin(); it != m_data_callbacks.end(); it++) {
		data_callbacks.emplace_back(std::cref(*it));
	}
	m_context.logger()->trace("Calling `{}' share. Callbacks to call: {}", name, data_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&, Ref)>& callback : data_callbacks) {
		try {
			callback(name, ref);
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e);
		} catch (const exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "Not std::exception based error");
		}
	}
	if (!errors.empty()) {
		if (1 == errors.size()) {
			throw Error{errors.front().status(), "Error while triggering data share `{}': {}", name, errors.front().what()};
		}
		string errmsg = "Multiple (" + std::to_string(errors.size()) + ") errors while triggering data share `" + name + "':\n";
		for (auto&& err: errors) {
			errmsg += string(err.what()) + "\n";
		}
		throw System_error{errmsg.c_str()};
	}
}

void Callbacks::call_data_remove_callbacks(const string& name, Ref ref) const
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&, Ref)>>> data_remove_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_data_remove_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		data_remove_callbacks.emplace_back(std::cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_data_remove_callbacks.begin(); it != m_data_remove_callbacks.end(); it++) {
		data_remove_callbacks.emplace_back(std::cref(*it));
	}
	m_context.logger()->trace("Calling `{}' data remove. Callbacks to call: {}", name, data_remove_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&, Ref)>& callback : data_remove_callbacks) {
		try {
			callback(name, ref);
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e);
		} catch (const exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "Not std::exception based error");
		}
	}
	if (!errors.empty()) {
		if (1 == errors.size()) {
			throw Error{errors.front().status(), "Error while triggering data share `{}': {}", name, errors.front().what()};
		}
		string errmsg = "Multiple (" + std::to_string(errors.size()) + ") errors while triggering data share `" + name + "':\n";
		for (auto&& err: errors) {
			errmsg += string(err.what()) + "\n";
		}
		throw System_error{errmsg.c_str()};
	}
}

void Callbacks::call_event_callbacks(const string& name) const
{
	vector<std::reference_wrapper<const function<void(const string&)>>> event_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_event_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		event_callbacks.emplace_back(cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_event_callbacks.begin(); it != m_event_callbacks.end(); it++) {
		event_callbacks.emplace_back(cref(*it));
	}
	m_context.logger()->trace("Calling `{}' event. Callbacks to call: {}", name, event_callbacks.size());
	//call gathered callbacks
	std::vector<Error> errors;
	for (const function<void(const string&)>& callback : event_callbacks) {
		try {
			callback(name);
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e);
		} catch (const exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "Not std::exception based error");
		}
	}
	if (!errors.empty()) {
		if (1 == errors.size()) {
			throw Error{errors.front().status(), "Error while triggering event `{}': {}", name, errors.front().what()};
		}
		string errmsg = "Multiple (" + std::to_string(errors.size()) + ") errors while triggering event `" + string(name) + "':\n";
		for (auto&& err: errors) {
			errmsg += string(err.what()) + "\n";
		}
		throw System_error{errmsg.c_str()};
	}
}

void Callbacks::call_empty_desc_access_callbacks(const string& name) const
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&)>>> empty_desc_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_empty_desc_access_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		empty_desc_callbacks.emplace_back(std::cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_empty_desc_access_callbacks.begin(); it != m_empty_desc_access_callbacks.end(); it++) {
		empty_desc_callbacks.emplace_back(std::cref(*it));
	}
	m_context.logger()->trace("Calling `{}' empty desc access. Callbacks to call: {}", name, empty_desc_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&)>& callback : empty_desc_callbacks) {
		try {
			callback(name);
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e);
		} catch (const exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "Not std::exception based error");
		}
	}
	if (!errors.empty()) {
		if (1 == errors.size()) {
			throw Error{errors.front().status(), "Error while triggering empty desc access `{}': {}", name, errors.front().what()};
		}
		string errmsg = "Multiple (" + std::to_string(errors.size()) + ") errors while triggering empty desc access `" + name + "':\n";
		for (auto&& err: errors) {
			errmsg += string(err.what()) + "\n";
		}
		throw System_error{errmsg.c_str()};
	}
}

} // namespace PDI