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

#ifndef PDI_PDI_INSTANCE_H_
#define PDI_PDI_INSTANCE_H_

#include <memory>

#include "pdi/pdi_fwd.h"
#include "pdi/logger.h"

#include "plugin_store.h"
#include "global_context.h"

namespace PDI {
// 
class PDI_EXPORT Pdi_instance
{
private:
	/// The singleton Context instance
	static std::unique_ptr<Pdi_instance, void(*)(Pdi_instance*)> s_instance;

	/// Global logger of PDI, should be constructed first, destroyed last
	Logger m_logger;

	Global_context m_data_store;

	/// The plugins, this should be late in the list to be destroyed early
	Plugin_store m_plugins;

	Pdi_instance(const Pdi_instance&) = delete;

	Pdi_instance(Pdi_instance&&) = delete;

	Pdi_instance(PC_tree_t conf);

	~Pdi_instance();

public:
	static void init(PC_tree_t conf);

	static bool initialized();

	static Pdi_instance& instance();

	static void finalize();

	Plugin_store& plugins();

	Logger& logger();

	Global_context& data_store();
};

} // namespace PDI

#endif // PDI_PDI_INSTANCE_H_

