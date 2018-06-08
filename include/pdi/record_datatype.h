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

#ifndef PDI_RECORD_DATATYPE_H_
#define PDI_RECORD_DATATYPE_H_

#include <vector>

#include <pdi/logger.h>
#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>


namespace PDI {

class PDI_EXPORT Record_datatype:
	public Datatype
{
public:
	class Member
	{
		/// Offset or distance in byte from the Record_datatype start
		size_t m_displacement;
		
		/// Type of the contained member
		Datatype_uptr m_type;
		
		/// Name of this specific member
		std::string m_name;
		
	public:
		Member(size_t displacement, Datatype_uptr type, const std::string& name);
		
		Member(const Member& o);
		
		size_t displacement() const;
		
		const Datatype& type() const;
		
		const std::string& name() const;
		
	};
	
private:
	/// All members in increasing displacement order
	std::vector<Member> m_members;
	
	/// The total size of the buffer containing all members
	size_t m_buffersize;

	/// Global logger of PDI
	Logger m_logger {spdlog::get("logger")};
	
public:
	Record_datatype(std::vector<Member>&& members, size_t size);
	
	const std::vector<Member>& members() const;
	
	Datatype_template_uptr clone() const override;
	
	Datatype_uptr clone_type() const override;
	
	Datatype_uptr densify() const override;
	
	Datatype_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
};

} // namespace PDI

#endif // PDI_RECORD_DATATYPE_H_
