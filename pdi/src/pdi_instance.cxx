/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <cstdlib>
#include <functional>
#include <map>
#include <memory>
#include <vector>

#include <dlfcn.h>
#include <unistd.h>

#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/version.h"

#include "data_descriptor_impl.h"

#include "pdi_instance.h"

namespace PDI {

std::unique_ptr<Pdi_instance, void (*)(Pdi_instance*)> Pdi_instance::s_instance(nullptr, [](Pdi_instance* i) {
	if (i) delete i;
});

void Pdi_instance::init(PC_tree_t conf)
{
	s_instance.reset(new Pdi_instance(conf));
}

bool Pdi_instance::initialized()
{
	return static_cast<bool>(s_instance);
}

Pdi_instance& Pdi_instance::instance()
{
	if (!s_instance) throw State_error{"PDI not initialized"};
	return *s_instance;
}

void Pdi_instance::finalize()
{
	s_instance.reset();
}

Pdi_instance::Pdi_instance(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_data_store{conf}
	, m_plugins{data_store(), conf}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(data_store());
	
	// load user datatypes
	Datatype_template::load_user_datatypes(data_store(), PC_get(conf, ".types"));

	m_plugins.load_plugins();

	//TODO: Load data store config
	// data_store.load_config();

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(data_store());

	m_logger.info("Initialization successful");
}

Pdi_instance::~Pdi_instance()
{
	m_logger.info("Finalization");
}

Global_context& Pdi_instance::data_store()
{
	return m_data_store;
}

Logger& Pdi_instance::logger()
{
	return m_logger;
}

} // namespace PDI
