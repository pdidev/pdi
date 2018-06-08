/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_DATATYPE_TEMPLATE_H_
#define PDI_DATATYPE_TEMPLATE_H_

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/logger.h>

namespace PDI {

class PDI_EXPORT Datatype_template
{
	/// Global logger of PDI
	Logger logger {spdlog::get("logger")};

public:
	/** Destroys the template
	 */
	virtual ~Datatype_template();
	
	/** Creates a new datatype as an exact copy of this one
	 *
	 * \return the dense type that is produced
	 */
	virtual Datatype_template_uptr clone() const = 0;
	
	/** Creates a new datatype by resolving the value of all metadata references
	 *
	 * \param ctx the context in which to evaluate this template
	 * \return the evaluated type that is produced
	 */
	virtual Datatype_uptr evaluate(Context& ctx) const = 0;
	
	/** Creates a new datatype from a paraconf-style config
	 * \param node the configuration to read
	 * \return the type generated
	 */
	static Datatype_template_uptr load(PC_tree_t node);
	
};

} // namespace PDI

#endif // PDI_DATATYPE_TEMPLATE_H_

