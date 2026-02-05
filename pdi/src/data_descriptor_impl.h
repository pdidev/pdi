/*
 * SPDX-FileCopyrightText: 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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

	void* share(Ref ref, bool read, bool write) override;

	void release() override;

	void* reclaim() override;

}; // class Data_descriptor

} // namespace PDI

#endif // PDI_DATA_DESCRIPTOR_IMPL_H_
