/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "pdi.h"
#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"
#include "pdi/tuple_datatype.h"

#include "pdi/datatype_template_reference.h"

namespace PDI {

using std::exception;
using std::make_shared;
using std::max;
using std::move;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

class Local_context: public Context
{
	Context* m_global_context;

public:
	Local_context(Context& context)
		: m_global_context(&context)
	{}

	virtual Data_descriptor& desc(const std::string& name) override { return m_global_context->desc(name); }

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& desc(const char* name) override { return m_global_context->desc(name); }

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& operator[] (const std::string& name) override { return (*m_global_context)[name]; }

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& operator[] (const char* name) override { return (*m_global_context)[name]; }

	/** Returns an iterator on the first descriptor
	 */
	virtual Iterator begin() override { return m_global_context->begin(); }

	/** Returns an iterator past the last descriptor
	 */
	virtual Iterator end() override { return m_global_context->end(); }

	/** Find the Data_descriptor corresponding to a given name
	 *
	 * \param[in] name the Data_descriptor name
	 * \return an iterator to the requested Data_descriptor name. If no such element is found, past-the-end (see end()) iterator is returned.
	 */
	virtual Iterator find(const std::string& name) override { return m_global_context->find(name); }

	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	virtual void event(const char* name) override { return m_global_context->event(name); }

	/** Logger getter
	 * \return logger
	 */
	virtual Logger& logger() override { return m_global_context->logger(); }

	/** Callbacks of the context
	 * \return context callbacks
	 */
	virtual Callbacks& callbacks() override { return m_global_context->callbacks(); }

	/** Creates a new datatype template from a paraconf-style config
	 * \param[in] node the configuration to read
	 *
	 * \return the type generated
	 */
	virtual Datatype_template_sptr datatype(PC_tree_t node) override { return m_global_context->datatype(node); }

	/**
	 * TODO add doc
	 */
	virtual Datatype_template_definition_sptr datatype(const std::string& str) override { return m_global_context->datatype(str); }

	/** Adds new datatype parser to the context
	 *
	 * \param[in] name name of the datatype to add
	 * \param[in] parser function that creates new datatype_template from PC_tree_t
	 */
	virtual void add_datatype(const std::string& name, Datatype_template_parser parser) override
	{
		return m_global_context->add_datatype(name, parser);
	}

	/// Finalizes PDI and exits application
	virtual void finalize_and_exit() override { return m_global_context->finalize_and_exit(); }
};

Datatype_sptr Datatype_template_reference::evaluate(Context& ctx) const
{
	// need to evaluate m_definition and m_arguments
	// create a new local context, falls back to old context if method not available
	Local_context callee_context(ctx);
	
	// need to populate the callee_context
	// give the values of m_parameters
	// evaluate each element in its context
	return m_definition->m_content->evaluate(callee_context);
}

} // namespace PDI
