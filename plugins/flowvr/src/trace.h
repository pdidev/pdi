/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_FLOWVR_TRACE
#define PDI_FLOWVR_TRACE

#include <string>

#include <flowvr/trace.h>

#include <pdi/context.h>
#include <pdi/ref_any.h>
#include <spdlog/spdlog.h>

namespace  {

class Trace
{
	class Trace_base;
	
	std::string m_on_data; // descriptor name on which make write a tracve
	std::unique_ptr<Trace_base> m_trace; // trace holder
	
	/**
	 *  Reads config, set on_data
	 *
	 *  \param[in] config the configuration to read (trace root)
	 */
	void load_on_data(PC_tree_t config)
	{
		PC_tree_t on_data_node = PC_get(config, ".on_data");
		if (PC_status(on_data_node)) {
			throw PDI::Error{PDI_ERR_CONFIG, "Trace must have defined `on_data'"};
		}
		m_on_data = PDI::to_string(on_data_node);
	}
	
	/**
	 *  Reads config, create proper trace type.
	 *
	 *  \param[in] ctx the PDI context for this plugin instance
	 *  \param[in] name name of the trace
	 *  \param[in] config the configuration to read (trace root)
	 */
	void load_trace(PDI::Context& ctx, std::string name)
	{
		const PDI::Datatype_uptr trace_datatype = ctx[m_on_data].default_type()->evaluate(ctx);
		const PDI::Scalar_datatype* scalar_datatype = dynamic_cast<PDI::Scalar_datatype*>(trace_datatype.get());
		const PDI::Array_datatype* array_datatype = dynamic_cast<PDI::Array_datatype*>(trace_datatype.get());
		
		if (array_datatype) {
			scalar_datatype = dynamic_cast<const PDI::Scalar_datatype*>(&array_datatype->subtype());
		}
		
		if (scalar_datatype) {
			PDI::Scalar_kind trace_kind = scalar_datatype->kind();
			if (scalar_datatype->kind() == PDI::Scalar_kind::SIGNED && scalar_datatype->buffersize() == sizeof(int)) {
				m_trace.reset(new Trace_int(ctx, name));
				return;
			} else if (scalar_datatype->kind() == PDI::Scalar_kind::FLOAT  && scalar_datatype->buffersize() == sizeof(float)) {
				m_trace.reset(new Trace_float(ctx, name));
				return;
			} else if (array_datatype && scalar_datatype->kind() == PDI::Scalar_kind::UNSIGNED && scalar_datatype->buffersize() == sizeof(char)) {
				m_trace.reset(new Trace_string(ctx, name));
			}
		}
		throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) `%s' has not supported type for Trace", name.c_str()};
	}
	
public:
	Trace(PDI::Context& ctx, std::string name, PC_tree_t config)
	{
		load_on_data(config);
		load_trace(ctx, name);
		ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->write(name, ref);
		}, m_on_data);
		
		ctx.logger()->debug("(FlowVR) Trace ({}): On_data: {} ", name, m_on_data);
	}
	
	/**
	 *  \return flowvr::Trace pointer
	 */
	flowvr::Trace* get() const
	{
		return m_trace->get();
	}
	
	/**
	 *  Writes the trace from the reference
	 *
	 *  \param[in] ref_r Ref where from get the trace value
	 */
	void write(const std::string& data_name, const PDI::Ref_r ref_r)
	{
		if (ref_r) {
			m_trace->write(ref_r);
		} else {
			throw PDI::Error {PDI_ERR_RIGHT, "(FlowVR) Trace (%s): Unable to get read permissions for `%s'", data_name.c_str()};
		}
	}
	
private:
	class Trace_base
	{
	protected:
		PDI::Context& m_ctx;
		
		Trace_base(PDI::Context& ctx):
			m_ctx{ctx}
		{}
		
	public:
		/**
		 *  \return flowvr::Trace pointer
		 */
		virtual flowvr::Trace* get() const = 0;
		
		/**
		 *  Writes the trace from the reference
		 *
		 *  \param[in] ref_r Ref where from get the trace value
		 */
		virtual void write(const PDI::Ref_r& ref_r) = 0;
	};
	
	
	class Trace_int : public Trace_base
	{
		std::unique_ptr<flowvr::TypedTrace<int>> m_flowvr_trace;
		
	public:
		Trace_int(PDI::Context& ctx, std::string name):
			Trace_base{ctx},
			m_flowvr_trace{new flowvr::TypedTrace<int>(name)}
		{}
		
		flowvr::Trace* get() const override
		{
			return m_flowvr_trace.get();
		}
		
		void write(const PDI::Ref_r& ref_r) override
		{
			const int value = *static_cast<const int*>(ref_r.get());
			m_ctx.logger()->debug("(FlowVR) Trace int ({}): Writing {}", m_flowvr_trace->getName(), value);
			m_flowvr_trace->write(value);
		}
	};
	
	
	class Trace_float : public Trace_base
	{
		std::unique_ptr<flowvr::TypedTrace<float>> m_flowvr_trace; // FlowVR owns traces ???
		
	public:
		Trace_float(PDI::Context& ctx, std::string name):
			Trace_base{ctx},
			m_flowvr_trace{new flowvr::TypedTrace<float>(name)}
		{}
		
		flowvr::Trace* get() const override
		{
			return m_flowvr_trace.get();
		}
		
		void write(const PDI::Ref_r& ref_r) override
		{
			const float value = *static_cast<const float*>(ref_r.get());
			m_ctx.logger()->debug("(FlowVR) Trace float ({}): Writing {}", m_flowvr_trace->getName(), value);
			m_flowvr_trace->write(value);
		}
	};
	
	
	class Trace_string : public Trace_base
	{
		std::unique_ptr<flowvr::TypedTrace<std::string>> m_flowvr_trace; // FlowVR owns traces ???
		
	public:
		Trace_string(PDI::Context& ctx, std::string name):
			Trace_base{ctx},
			m_flowvr_trace{new flowvr::TypedTrace<std::string>(name)}
		{}
		
		flowvr::Trace* get() const override
		{
			return m_flowvr_trace.get();
		}
		
		void write(const PDI::Ref_r& ref_r) override
		{
			const char* value_char = static_cast<const char*>(ref_r.get());
			const std::string value{value_char};
			m_ctx.logger()->debug("(FlowVR) Trace string ({}): Writing {}", m_flowvr_trace->getName(), value);
			m_flowvr_trace->write(value);
		}
	};
	
};

} // namespace <anonymous>

#endif // PDI_FLOWVR_TRACE