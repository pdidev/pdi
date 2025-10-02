/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include "pdi/context.h"
#include "pdi/data_store.h"
#include "pdi/error.h"

using std::cref;
using std::exception;
using std::function;
using std::string;
using std::vector;

namespace PDI {

function<void()> Data_store::on_init(const function<void()>& callback)
{
	m_init_callbacks.emplace_back(callback);
	auto it = --m_init_callbacks.end();
	return [it, this]() {
		this->m_init_callbacks.erase(it);
	};
}

function<void()> Data_store::on_new_data(const function<void(const string&, Ref)>& callback, const string& name)
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

function<void()> Data_store::on_data_remove(const function<void(const string&, Ref)>& callback, const string& name)
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

function<void()> Data_store::on_event(const function<void(const string&)>& callback, const string& name)
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

function<void()> Data_store::on_missing_data(const function<void(const string&)>& callback, const string& name)
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

void Data_store::call_init_callbacks() const
{
	for (auto&& init_callback: m_init_callbacks) {
		init_callback();
	}
}

void Data_store::call_data_callbacks(const string& name, Ref ref) const
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
	m_context.logger().trace("Calling `{}' share. Callbacks to call: {}", name, data_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&, Ref)>& callback: data_callbacks) {
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

void Data_store::call_data_remove_callbacks(const string& name, Ref ref) const
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
	m_context.logger().trace("Calling `{}' data remove. Callbacks to call: {}", name, data_remove_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&, Ref)>& callback: data_remove_callbacks) {
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

void Data_store::call_event_callbacks(const string& name) const
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
	m_context.logger().trace("Calling `{}' event. Callbacks to call: {}", name, event_callbacks.size());
	//call gathered callbacks
	std::vector<Error> errors;
	for (const function<void(const string&)>& callback: event_callbacks) {
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

void Data_store::call_empty_desc_access_callbacks(const string& name) const
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
	m_context.logger().trace("Calling `{}' empty desc access. Callbacks to call: {}", name, empty_desc_callbacks.size());
	//call gathered callbacks
	vector<Error> errors;
	for (const std::function<void(const std::string&)>& callback: empty_desc_callbacks) {
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

namespace {

void load_data(Context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);

	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = ctx.desc(to_string(PC_get(node, "{%d}", map_id)).c_str());
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}
	if (is_metadata) {
		ctx.logger().trace("Loaded {} metadata", map_len);
	} else {
		ctx.logger().trace("Loaded {} data", map_len);
	}
}

} // namespace

unique_ptr<Data_store> Data_store::s_context;

void Data_store::init(PC_tree_t conf)
{
	s_context.reset(new Data_store(conf));
}

bool Data_store::initialized()
{
	return static_cast<bool>(s_context);
}

Data_store& Data_store::context()
{
	if (!s_context) throw State_error{"PDI not initialized"};
	return *s_context;
}

void Data_store::finalize()
{
	s_context.reset();
}

Data_store::Data_store(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_plugins{*this, conf}
	, m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);
	// load user datatypes
	Datatype_template::load_user_datatypes(*this, PC_get(conf, ".types"));

	m_plugins.load_plugins();

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(*this);

	// no metadata is not an error
	PC_tree_t metadata = PC_get(conf, ".metadata");
	if (!PC_status(metadata)) {
		load_data(*this, metadata, true);
	} else {
		m_logger.debug("Metadata is not defined in specification tree");
	}

	// no data is spurious, but not an error
	PC_tree_t data = PC_get(conf, ".data");
	if (!PC_status(data)) {
		load_data(*this, data, false);
	} else {
		m_logger.warn("Data is not defined in specification tree");
	}


	m_callbacks.call_init_callbacks();
	m_logger.info("Initialization successful");
}

Data_descriptor& Data_store::desc(const char* name)
{
	return *(m_descriptors.emplace(name, unique_ptr<Data_descriptor>{new Data_descriptor_impl{*this, name}}).first->second);
}

Data_descriptor& Data_store::desc(const string& name)
{
	return desc(name.c_str());
}

Data_descriptor& Data_store::operator[] (const char* name)
{
	return desc(name);
}

Data_descriptor& Data_store::operator[] (const string& name)
{
	return desc(name.c_str());
}

Data_store::Iterator Data_store::begin()
{
	return Context::get_iterator(m_descriptors.begin());
}

Data_store::Iterator Data_store::end()
{
	return Context::get_iterator(m_descriptors.end());
}

PDI::Context::Iterator Data_store::find(const string& name)
{
	return Context::get_iterator(m_descriptors.find(name));
}

void Data_store::event(const char* name)
{
	m_callbacks.call_event_callbacks(name);
}

Logger& Data_store::logger()
{
	return m_logger;
}

Datatype_template_sptr Data_store::datatype(PC_tree_t node)
{
	string type;
	try {
		type = to_string(PC_get(node, ".type"));
	} catch (const Error& e) {
		type = to_string(node);
	}

	// check if someone didn't mean to create an array with the old syntax
	if (type != "array") {
		if (!PC_status(PC_get(node, ".size"))) {
			logger().warn("In line {}: Non-array type with a `size' property", node.node->start_mark.line);
		}
		if (!PC_status(PC_get(node, ".sizes"))) {
			logger().warn("In line {}: Non-array type with a `sizes' property", node.node->start_mark.line);
		}
	}

	auto&& func_it = m_datatype_parsers.find(type);
	if (func_it != m_datatype_parsers.end()) {
		return (func_it->second)(*this, node);
	}
	throw Config_error{node, "Unknown data type: `{}'", type};
}

void Data_store::add_datatype(const string& name, Datatype_template_parser parser)
{
	if (!m_datatype_parsers.emplace(name, move(parser)).second) {
		//if a datatype with the given name already exists
		throw Type_error{"Datatype already defined `{}'", name};
	}
}

Callbacks& Data_store::callbacks()
{
	return m_callbacks;
}

void Data_store::finalize_and_exit()
{
	Data_store::finalize();
	exit(0);
}

Data_store::~Data_store()
{
	m_logger.info("Finalization");
}



struct Data_descriptor_impl::Ref_holder {
	virtual Ref ref() const = 0;

	virtual ~Ref_holder() {}

	template <bool R, bool W>
	struct Impl;
};

template <bool R, bool W>
struct Data_descriptor_impl::Ref_holder::Impl: Data_descriptor_impl::Ref_holder {
	Ref_any<R, W> m_t;

	Impl(Ref t)
		: m_t(t)
	{}

	Ref ref() const override { return m_t; }
};

Data_descriptor_impl::Data_descriptor_impl(Global_context& ctx, const char* name)
	: m_context{ctx}
	, m_type{UNDEF_TYPE}
	, m_name{name}
	, m_metadata{false}
{}

Data_descriptor_impl::Data_descriptor_impl(Data_descriptor_impl&&) = default;

Data_descriptor_impl::~Data_descriptor_impl()
{
	// release metadata placeholder
	if (metadata() && m_refs.size() == 1) m_refs.pop();

	// on error, we might be destroyed while not empty.
	if (!m_refs.empty()) {
		m_context.logger().warn("Remaining {} reference(s) to `{}' in PDI after program end", m_refs.size() - (metadata() ? 1 : 0), m_name);
		// leak the remaining data
		while (!m_refs.empty()) {
			m_refs.top().release();
			m_refs.pop();
		}
	}

	assert(m_refs.empty());
}

void Data_descriptor_impl::default_type(Datatype_template_sptr type)
{
	m_type = move(type);
}

Datatype_template_sptr Data_descriptor_impl::default_type()
{
	return m_type;
}

bool Data_descriptor_impl::metadata() const
{
	return m_metadata;
}

void Data_descriptor_impl::metadata(bool metadata)
{
	assert((!m_metadata || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if (!m_refs.empty()) {
		throw State_error{"Can not change the metadata status of a non-empty descriptor"};
	}
	m_metadata = metadata;

	// for metadata, ensure we have a placeholder ref at stack bottom
	if (metadata) {
		m_refs.emplace(new Ref_holder::Impl<true, false>(Ref{}));
	}
	assert((!m_metadata || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
}

const string& Data_descriptor_impl::name() const
{
	return m_name;
}

Ref Data_descriptor_impl::ref()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if (m_refs.empty()) {
		m_context.callbacks().call_empty_desc_access_callbacks(m_name);

		//at least one plugin should share a Ref
		if (m_refs.empty()) {
			throw Value_error{"Cannot access a non shared value: `{}'", m_name};
		}
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return m_refs.top()->ref();
}

bool Data_descriptor_impl::empty()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return m_refs.empty();
}

void Data_descriptor_impl::share(void* data, bool read, bool write)
try {
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	Ref r{data, &free, m_type->evaluate(m_context), read, write};
	try {
		m_context.logger().trace("Sharing `{}' Ref with rights: R = {}, W = {}", m_name, read, write);
		share(r, false, false);
	} catch (...) {
		// on error, do not free the data as would be done automatically otherwise
		r.release();
		assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
		throw;
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
} catch (Error& e) {
	throw Error(e.status(), "Unable to share `{}', {}", name(), e.what());
}

void* Data_descriptor_impl::share(Ref data_ref, bool read, bool write)
try {
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// metadata must provide read access
	if (metadata() && !Ref_r(data_ref)) {
		throw Right_error{"Metadata sharing must offer read access"};
	}

	// make a reference and put it in the store
	void* result = nullptr;
	if (read) {
		if (write) {
			result = Ref_rw{data_ref}.get(nothrow);
			m_refs.emplace(new Ref_holder::Impl<true, true>(data_ref));
		} else {
			result = const_cast<void*>(Ref_r{data_ref}.get(nothrow));
			m_refs.emplace(new Ref_holder::Impl<true, false>(data_ref));
		}
	} else {
		if (write) {
			result = Ref_w{data_ref}.get(nothrow);
			m_refs.emplace(new Ref_holder::Impl<false, true>(data_ref));
		} else {
			m_refs.emplace(new Ref_holder::Impl<false, false>(data_ref));
		}
	}

	if (data_ref && !ref()) {
		m_refs.pop();
		throw Right_error{"Unable to grant requested rights"};
	}

	try {
		m_context.callbacks().call_data_callbacks(m_name, ref());
	} catch (const exception&) {
		m_refs.pop();
		throw;
	}

	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return result;
} catch (Error& e) {
	throw Error(e.status(), "Unable to share `{}', {}", name(), e.what());
}

void Data_descriptor_impl::release()
try {
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// move reference out of the store
	if (m_refs.empty() || (m_refs.size() == 1 && metadata())) throw State_error{"Cannot release a non shared value: `{}'", m_name};

	m_context.callbacks().call_data_remove_callbacks(m_name, ref());

	Ref oldref = ref();
	m_refs.pop();

	// if only the metadata placeholder ref remains replace it by this one
	if (metadata() && m_refs.size() == 1) {
		m_refs.pop();
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref));
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
} catch (Error& e) {
	throw Error(e.status(), "Unable to release `{}', {}", name(), e.what());
}

void* Data_descriptor_impl::reclaim()
try {
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if (m_refs.empty() || (m_refs.size() == 1 && metadata())) throw State_error{"Cannot reclaim a non shared value: `{}'", m_name};

	m_context.callbacks().call_data_remove_callbacks(m_name, ref());

	Ref oldref = ref();
	m_refs.pop();

	// if only the metadata placeholder ref remains replace it by a copy of this one
	if (metadata() && m_refs.size() == 1) {
		m_refs.pop();
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref.copy()));
	}

	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// finally release the data behind the ref
	return oldref.release();
} catch (Error& e) {
	throw Error(e.status(), "Unable to reclaim `{}', {}", name(), e.what());
}

} // namespace PDI
