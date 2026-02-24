/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_DATA_DESCRIPTOR_IMPL_H_
#define PDI_DATA_DESCRIPTOR_IMPL_H_

#include <functional>
#include <memory>
#include <stack>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>
#include <pdi/ref_any.h>

#include "global_context.h"

namespace PDI {

class PDI_EXPORT Data_descriptor_impl: public Data_descriptor
{
	friend class Global_context;
	friend class Delayed_data_callback;
	friend class Descriptor_test_handler;


	struct PDI_NO_EXPORT Ref_holder;

	/// The context this descriptor is part of
	Global_context& m_context;

	/// References to the values of this descriptor
	std::stack<std::unique_ptr<Ref_holder>> m_refs;

	Datatype_template_sptr m_type;

	const std::string m_name;

	bool m_metadata;

	/** Create an empty descriptor
	 */
	Data_descriptor_impl(Global_context& ctx, const char* name);

	Data_descriptor_impl(const Data_descriptor_impl&) = delete;

	Data_descriptor_impl& operator= (const Data_descriptor_impl&) = delete;

	Data_descriptor_impl& operator= (Data_descriptor_impl&&) = delete;

public:
	Data_descriptor_impl(Data_descriptor_impl&&);

	~Data_descriptor_impl() override;

	void default_type(Datatype_template_sptr) override;

	Datatype_template_sptr default_type() override;

	bool metadata() const override;

	void metadata(bool metadata) override;

	const std::string& name() const override;

	Ref ref() override;

	bool empty() override;

	void share(void* data, bool read, bool write) override;

	void share(void* data, bool read, bool write, Delayed_data_callbacks&& delayed_callbacks) override;

	void* share(Ref ref, bool read, bool write) override;

	void* share(Ref ref, bool read, bool write, Delayed_data_callbacks&& delayed_callbacks) override;

	void trigger_delayed_data_callbacks() override;

	void release() override;

	void* reclaim() override;
}; // class Data_descriptor

} // namespace PDI

#endif // PDI_DATA_DESCRIPTOR_IMPL_H_
