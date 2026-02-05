/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
