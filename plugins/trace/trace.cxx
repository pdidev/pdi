/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <functional>
#include <iostream>
#include <string>
#include <unordered_set>

#include <spdlog/spdlog.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>


namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Error;
using PDI::Plugin;
using std::bind;
using std::reference_wrapper;
using std::string;
using std::unordered_set;

struct trace_plugin: Plugin {

	unordered_set<Ref> m_refs;
	
	trace_plugin(Context& ctx, PC_tree_t):
		Plugin {ctx}
	{
		ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) {
			this->data(name, ref);
		});
		ctx.callbacks().add_event_callback([this](const std::string& name) {
			this->context().logger()->info("!!!                            named event: {}", name);
		});
		context().logger()->set_pattern("[PDI][Trace-plugin] *** %^%l%$: %v");
		context().logger()->info("Welcome!");
	}
	
	~trace_plugin()
	{
		context().logger()->info("Goodbye!");
	}
	
	void data(const std::string& name, Ref ref)
	{
		// store a copy of the reference because we need to keep it for notification
		auto ref_it = m_refs.emplace(ref).first;
		// register to be notified when the reference becomes invalid (on the copy we keep)
		string sname = name; // store the name in a string to reuse it
		ref_it->on_nullify([=](Ref r) {
			this->data_end(sname, r);
		});
		context().logger()->info("=>>   data becoming available in the store: {}", name);
	}
	
	void data_end(const std::string& name, Ref r)
	{
		context().logger()->info("<<= data stop being available in the store: {}", name);
	}
	
}; // struct trace_plugin

} // namespace <anonymous>

PDI_PLUGIN(trace)
