/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_DATATYPE_TEMPLATE_H_
#define PDI_DATATYPE_TEMPLATE_H_

#include <memory>
#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace PDI {

using Attributes_map = std::unordered_map<std::string, Expression>;

class PDI_EXPORT Datatype_template: public std::enable_shared_from_this<Datatype_template>
{
protected:
	Attributes_map m_attributes;
	
public:
	/** Creates datatype template with given attributes
	 *
	 * \param attributes attributes of datatype template
	 */
	Datatype_template(const Attributes_map& attributes = {});
	
	/** Creates datatype template
	 *
	 * If attributes are defined in datatype_tree, they will be included in datatype template
	 *
	 * \param datatype_tree datatype tree
	 */
	Datatype_template(PC_tree_t datatype_tree);
	
	/** Destroys the template
	 */
	virtual ~Datatype_template();
	
	/** Creates a new datatype by resolving the value of all metadata references
	 *
	 * \param ctx the context in which to evaluate this template
	 * \return the evaluated type that is produced
	 */
	virtual Datatype_sptr evaluate(Context& ctx) const = 0;
	
	/** Returns attribute of given name as Expression
	 * \param attribute_name attribute to get
	 *
	 * \return value of attribute as Expression
	 */
	Expression attribute(const std::string& attribute_name) const;
	
	/** Returns all attributes as a unordered map
	 *
	 * \return all attributes as a unordered map
	 */
	const Attributes_map& attributes() const;
	
	/**
	 * Adds to the context the basic Array, Record, C and Fortran datatypes
	 *
	 * \param[in,out] ctx the context where to add the datatypes
	 */
	static void load_basic_datatypes(Context& ctx);
	
	/**
	 * Adds to the context the user defined datatypes
	 *
	 * \param[in,out] ctx the context where to add the datatypes
	 * \param[in] types_tree with defined types
	 */
	static void load_user_datatypes(Context& ctx, PC_tree_t types_tree);
};

} // namespace PDI

#endif // PDI_DATATYPE_TEMPLATE_H_
