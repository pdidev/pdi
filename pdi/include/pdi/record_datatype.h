/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_RECORD_DATATYPE_H_
#define PDI_RECORD_DATATYPE_H_

#include <string>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>

namespace PDI {

/** A Record_datatype is a Datatype that represents a fixed number of
 * elements of potentially different types layed out in a specific way in
 * memory. Each element is given a name to access it.
 */
class PDI_EXPORT Record_datatype: public Datatype
{
	// Required to make_shared due to private ctor
	struct Shared_enabler;

public:
	/** A Member is one of the elements inside a Record_datatype
	 */
	class Member
	{
		/// Offset or distance in byte from the Record_datatype start
		size_t m_displacement;

		/// Type of the contained member
		Datatype_sptr m_type;

		/// Name of this specific member
		std::string m_name;

	public:
		/** Construct a new member
		 *
		 * \param displacement offset or distance in byte from the Record_datatype start
		 * \param type type of the contained member
		 * \param name name of this specific member
		 */
		Member(size_t displacement, Datatype_sptr type, const std::string& name);

		/** Construct a new member by copy
		 *
		 * \param o the Member to copy
		 */
		Member(const Member& o);

		/** Access the offset or distance in byte from the Record_datatype start
		 *
		 * \return the offset or distance in byte from the Record_datatype start
		 */
		size_t displacement() const;

		/** Access the type of the contained member
		 *
		 * \return the type of the contained member
		 */
		Datatype_sptr type() const;

		/** Access the name of this specific member
		 *
		 * \return the name of this specific member
		 */
		const std::string& name() const;

		/** Tests another member for equality
		 *
		 * \param rhs the other member to compare
		 * \return true if the members are equal
		 */
		bool operator== (const Member& rhs) const;

		/** Tests another member for inequality
		 *
		 * \param rhs the other member to compare
		 * \return true if the members are different
		 */
		bool operator!= (const Member& rhs) const;
	};

private:
	/// All members in increasing displacement order
	std::vector<Member> m_members;

	/// The total size of the buffer containing all members
	size_t m_buffersize;

public:
	/** Accesses the members in increasing displacement order
	 */
	const std::vector<Member>& members() const;

	Datatype_sptr densify() const override;

	Datatype_sptr evaluate(Context&) const override;

	bool dense() const override;

	size_t datasize() const override;

	size_t buffersize() const override;

	size_t alignment() const override;

	bool simple() const override;

	void* data_to_dense_copy(void* to, const void*) const override;

	void* data_from_dense_copy(void* to, const void*) const override;

	Datatype_sptr member(const char* name) const override;

	std::pair<void*, Datatype_sptr> member(const char* name, void* data) const override;

	void destroy_data(void*) const override;

	std::string debug_string() const override;

	bool operator== (const Datatype&) const override;

private:
	/** Constructs a new Record_datatype
	 *
	 * \param members the members for the newly created Record_datatype in
	 *        increasing displacement order
	 * \param size the total size of the buffer containing all members
	 * \param attributes attributes of the record datatype
	 */
	Record_datatype(std::vector<Member>&& members, size_t size, const Attributes_map& attributes = {});

public:
	/** Constructs a new Record_datatype
	 *
	 * \param members the members for the newly created Record_datatype in
	 *        increasing displacement order
	 * \param size the total size of the buffer containing all members
	 * \param attributes attributes of the record datatype
	 */
	static std::shared_ptr<Record_datatype> make(std::vector<Member>&& members, size_t size, const Attributes_map& attributes = {});
};

} // namespace PDI

#endif // PDI_RECORD_DATATYPE_H_
