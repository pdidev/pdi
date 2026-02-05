/*
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_POINTER_DATATYPE_H_
#define PDI_POINTER_DATATYPE_H_

#include <functional>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>

namespace PDI {

class PDI_EXPORT Pointer_datatype: public Datatype
{
	// Required to make_shared due to private ctor
	struct Shared_enabler;

	/// Type of data that pointer is pointing
	Datatype_sptr m_subtype;

	/// Copy function or null for memcpy
	std::function<void*(void*, const void*)> m_copy;

	/// Destroy function or null for memcpy
	std::function<void(void*)> m_destroy;

public:
	/** Type of the pointed element
	 *
	 * \return the type of the pointed element
	 */
	Datatype_sptr subtype() const;

	Datatype_sptr densify() const override;

	Datatype_sptr evaluate(Context&) const override;

	bool dense() const override;

	size_t datasize() const override;

	size_t buffersize() const override;

	size_t alignment() const override;

	bool simple() const override;

	void* data_to_dense_copy(void* to, const void* from) const override;

	void* data_from_dense_copy(void* to, const void* from) const override;

	Datatype_sptr index(size_t index) const override;

	std::pair<void*, Datatype_sptr> index(size_t index, void* data) const override;

	Datatype_sptr slice(size_t start_index, size_t end_index) const override;

	std::pair<void*, Datatype_sptr> slice(size_t start_index, size_t end_index, void* data) const override;

	Datatype_sptr member(const char* name) const override;

	std::pair<void*, Datatype_sptr> member(const char* name, void* data) const override;

	Datatype_sptr dereference() const override;

	std::pair<void*, Datatype_sptr> dereference(void* data) const override;

	void destroy_data(void* ptr) const override;

	std::string debug_string() const override;

	bool operator== (const Datatype&) const override;

private:
	/** Creates new pointer datatype
	 *
	 * \param[in] subtype subtype of the pointer datatype
	 * \param[in] attributes attributes of the pointer datatype
	 */
	Pointer_datatype(Datatype_sptr subtype, const Attributes_map& attributes = {});

	/** Creates new pointer datatype
	 *
	 * \param[in] subtype subtype of the pointer datatype
	 * \param[in] copy function that copies data of this datatype
	 * \param[in] destroy function that destroys data of this datatype (doesn't deallocate memory)
	 * \param[in] attributes attributes of the pointer datatype
	 */
	Pointer_datatype(
		Datatype_sptr subtype,
		std::function<void*(void*, const void*)> copy,
		std::function<void(void*)> destroy,
		const Attributes_map& attributes = {}
	);

public:
	/** Creates new pointer datatype
	 *
	 * \param[in] subtype subtype of the pointer datatype
	 * \param[in] attributes attributes of the pointer datatype
	 */
	static std::shared_ptr<Pointer_datatype> make(Datatype_sptr subtype, const Attributes_map& attributes = {});

	/** Creates new pointer datatype
	 *
	 * \param[in] subtype subtype of the pointer datatype
	 * \param[in] copy function that copies data of this datatype
	 * \param[in] destroy function that destroys data of this datatype (doesn't deallocate memory)
	 * \param[in] attributes attributes of the pointer datatype
	 */
	static std::shared_ptr<Pointer_datatype> make(
		Datatype_sptr subtype,
		std::function<void*(void*, const void*)> copy,
		std::function<void(void*)> destroy,
		const Attributes_map& attributes = {}
	);
};

} // namespace PDI

#endif // PDI_POINTER_DATATYPE_H_
