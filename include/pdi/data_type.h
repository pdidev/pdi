/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#ifndef PDI_DATA_TYPE_H_
#define PDI_DATA_TYPE_H_

#include <memory>
#include <vector>

#include <paraconf.h>

#include <pdi.h>
#include <pdi/value.h>

#include <pdi/data_type_fwd.h>

namespace PDI
{

class PDI_EXPORT Data_type
{
public:
	/** Destroys a Datatype
	 */
	virtual ~Data_type();
	
	/** Creates a new datatype as an exact copy of this one
	 *
	 * \return the dense type that is produced
	 */
	virtual Data_type_uptr clone() const = 0;
	
	/** Creates a new datatype as the dense copy of this one
	 *
	 * \return the type that is produced
	 */
	virtual Data_type_uptr densify() const = 0;
	
	/** Creates a new datatype by resolving the value of all metadata references
	 *
	 * \return the evaluated type that is produced
	 */
	virtual Data_type_uptr evaluate() const = 0;
	
	/** Indicate if the datatype is dense or not
	 *
	 * \return whether the datatype is dense
	 */
	virtual bool dense() const = 0;
	
	/** Computes the data size of a type, excluding potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t datasize() const = 0;
	
	/** Computes the data size of a type, including potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t buffersize() const = 0;


	/** Return the required alignment for a type
	 *
	 * \return the size in bytes
	 */
	virtual size_t alignment() const = 0;

	/** Creates a new datatype from a paraconf-style config
	 * \param node the configuration to read
	 * \return the type generated
	 */
	static Data_type_uptr load(PC_tree_t node);
	
};

class PDI_EXPORT Scalar_datatype:
		public Data_type
{
public:
	/** Different possible interpretations for a scalar
	 */
	enum Kind: uint8_t { UNKNOWN, SIGNED, UNSIGNED, FLOAT, ADDRESS };
	
private:
	/// Interpretation of the content
	Kind m_kind;
	
	/// Size of the content in bytes or 0 if unknown
	Value m_size;
	
	/// Size of the alignment in bytes
	Value m_align;
	
public:
	Scalar_datatype(Kind kind, const Value &size): m_kind{kind}, m_size{size}, m_align{size} {}
	
	Scalar_datatype(Kind kind, const Value &size, const Value &align): m_kind{kind}, m_size{size}, m_align{align} {}
	
	/** Interpretation of the content
	 */
	Kind kind() const { return m_kind; }
	
	/** Size of the content in bytes or 0 if unknown
	 */
	const Value& size() const { return m_size; }
	
	/** Size of the alignment in bytes or 0 if unknown
	 */
	const Value& align() const { return m_align; }
	
	Data_type_uptr clone() const override;
	
	Data_type_uptr densify() const override;
	
	Data_type_uptr evaluate() const override;
	
	bool dense() const override { return true; }
	
	size_t datasize() const override { return size(); }
	
	size_t buffersize() const override { return size(); }
	
	size_t alignment() const override { return align(); }
	
};

class PDI_EXPORT Array_datatype:
		public Data_type
{
	/// Type of the elements contained in the array.
	Data_type_uptr m_subtype;
	
	/// Number of elements the array can store
	Value m_size;
	
	/// id of the first actual element of the array
	Value m_start;
	
	/// Number of actual elements in the array
	Value m_subsize;
	
public:
	Array_datatype(Data_type_uptr subtype, Value size, Value start, Value subsize): m_subtype {std::move(subtype)}, m_size{std::move(size)}, m_start{std::move(start)}, m_subsize{std::move(subsize)} {}
	
	Array_datatype(Data_type_uptr subtype, Value size): Array_datatype{std::move(subtype), size, 0, std::move(size)} {}
	
	/** Type of the elements contained in the array.
	 */
	const Data_type & subtype() const { return *m_subtype; }
	
	/** Number of elements the array can store
	 */
	const Value& size() const { return m_size; }
	
	/** id of the first actual element of the array
	 */
	const Value& start() const { return m_start; }
	
	/** Number of actual elements in the array
	 */
	const Value& subsize() const { return m_subsize; }
	
	Data_type_uptr clone() const override;
	
	Data_type_uptr densify() const override;
	
	Data_type_uptr evaluate() const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
};

class PDI_EXPORT Record_datatype:
		public Data_type
{
public:
	struct Member
	{
	private:
		/// Offset or distance in byte from the Record_datatype start
		Value m_displacement;
		
		/// Type of the contained member
		Data_type_uptr m_type;
		
		std::string m_name;
		
	public:
		Member( Value displacement, Data_type_uptr type, const std::string& name ): m_displacement{std::move(displacement)}, m_type{std::move(type)}, m_name{name} {}
		
		Member( const Member& o ): m_displacement{o.m_displacement}, m_type{o.m_type->clone()}, m_name{o.m_name} {}
		
		const Value& displacement() const { return m_displacement; }
		
		const Data_type& type() const { return *m_type; }
		
		const std::string& name() const { return m_name; }
		
	};
	
private:
	/// All members in increasing displacement order
	std::vector<Member> m_members;
	
	/// The total size of the buffer containing all members
	Value m_buffersize;
	
public:
	Record_datatype(std::vector<Member> &&members, Value &&size): m_members{move(members)}, m_buffersize{std::move(size)} {}
	
	const std::vector<Member>& members() const { return m_members; }
	
	Data_type_uptr clone() const override;
	
	Data_type_uptr densify() const override;
	
	Data_type_uptr evaluate() const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override { return m_buffersize; }
	
	size_t alignment() const override;
	
};

const Scalar_datatype UNDEF_TYPE = {Scalar_datatype::UNKNOWN, 0};

} // namespace PDI

#endif // PDI_DATA_TYPE_H_
