/*
 * SPDX-FileCopyrightText: 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_DATA_DESCRIPTOR_H_
#define PDI_DATA_DESCRIPTOR_H_

#include <string>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>
#include <pdi/ref_any.h>

namespace PDI {

class PDI_EXPORT Data_descriptor
{
public:
	virtual ~Data_descriptor();

	/** Set the datatype template used to type raw pointers shared
	 *  through this descriptor
	 *
	 * \param type the datatype template attached to the descriptor
	 */
	virtual void default_type(Datatype_template_sptr type) = 0;

	/** Access the datatype template used to type raw pointers shared
	 *  through this descriptor
	 *
	 * \return the datatype template attached to the descriptor
	 */
	virtual Datatype_template_sptr default_type() = 0;

	/** Return true if the data is a metadata
	 */
	virtual bool metadata() const = 0;

	/** Sets whether this describes a metadata or not
	 * \param metadata whether data shared through this descriptor should
	 *        behave as a metadata
	 */
	virtual void metadata(bool metadata) = 0;

	/** Access the name of the descriptor
	 */
	virtual const std::string& name() const = 0;

	/** Return a reference to the value of the data behind this descriptor
	 */
	virtual Ref ref() = 0;

	/** Checks whether this descriptor is empty (contains no reference)
	 *
	 * \returns true if empty.
	 */
	virtual bool empty() = 0;

	/** Shares some data with PDI
	 * \param[in,out] data the shared data
	 * \param read whether read access is granted to other references
	 * \param write whether write access is granted to other references
	 */
	virtual void share(void* data, bool read, bool write) = 0;

	/** Shares some data with PDI
	 * \param[in,out] ref a reference to the shared data
	 * \param read whether the stored reference should have read access
	 * \param write whether the stored reference should have write access
	 * \return the just shared buffer
	 */
	virtual void* share(Ref ref, bool read, bool write) = 0;

	/** Releases ownership of a data shared with PDI. PDI is then responsible to
	 * free the associated memory whenever necessary.
	 */
	virtual void release() = 0;

	/** Reclaims ownership of a data buffer shared with PDI. PDI does not manage
	 * the buffer memory anymore.
	 * \return the address of the buffer
	 */
	virtual void* reclaim() = 0;

}; // class Data_descriptor

} // namespace PDI

#endif // PDI_DATA_DESCRIPTOR_H_
