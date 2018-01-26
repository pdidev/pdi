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

#ifndef PDI_DATA_DESCRIPTOR_H_
#define PDI_DATA_DESCRIPTOR_H_

#include <functional>
#include <memory>
#include <stack>

#include <paraconf.h>

#include <pdi.h>
#include <pdi/data_type.h>
#include <pdi/data_reference.h>

namespace PDI
{

class PDI_EXPORT Data_descriptor
{
public:
	/** Create an empty descriptor
	 */
	Data_descriptor(const char *name);
	
	Data_descriptor(const Data_descriptor &) = delete;
	
	Data_descriptor(Data_descriptor &&) = delete;
	
	~Data_descriptor();
	
	Data_descriptor &operator= (const Data_descriptor &) = delete;
	
	Data_descriptor &operator= (Data_descriptor &&) = delete;
	
	/** Sets the creation template used to type raw pointers shared through this descriptor
	 * \param type the type template that will be attached to raw pointers shared through this descriptor
	 * \param config the full configuration attached to the descriptor
	 */
	void creation_template(Data_type_uptr type, PC_tree_t config) {
		m_type = move(type);
		m_config = config; 
	}
	
	/** Returns the PC_tree_t config
	 * \todo remove this and attach this config to the type
	 * \return the full configuration attached to the descriptor
	 */
	PC_tree_t config() const { return m_config; }
	
	/** Return true if the data is a metadata
	 */
	bool metadata() const { return m_metadata; }
	
	/** Sets whether this describes a metadata or not
	 * \param metadata whether data shared through this descriptor should behave as a metadata
	 */
	void metadata(bool metadata) { m_metadata = metadata; }
	
	const std::string &name() const
	{
		return m_name;
	}
	
	/** Return a reference to the value of the data behind this descriptor
	 */
	Data_ref ref()
	{
		if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot access a non shared value"};
		return m_refs.top()->ref();
	}
	
	/** Shares some data with PDI. The user code should not modify it before
	 * a call to either PDI_release or PDI_reclaim.
	 * \param[in,out] data the accessed data
	 * \param[in] access whether the data can be accessed for read or write
	 *                   by PDI
	 * \pre the user code owns the data buffer
	 * \post ownership of the data buffer is shared between PDI and the user code
	 *
	 * the access parameter is a binary OR of PDI_IN & PDI_OUT.
	 * * PDI_IN means PDI can set the buffer content
	 * * PDI_OUT means the buffer contains data that can be accessed by PDI
	 */
	void share(void *data, std::function<void(void *)> freefunc, bool read, bool write);
	
	/** Requests for PDI to access a data buffer.
	* \param[in,out] buffer a pointer to the accessed data buffer
	* \param[in] inout the access properties (PDI_IN, PDI_OUT, PDI_INOUT)
	* \pre PDI owns the data buffer
	* \post ownership of the data buffer is shared between PDI and the user code
	*/
	void share(Data_ref ref, bool read, bool write);
	
	/** Releases ownership of a data shared with PDI. PDI is then responsible to
	* free the associated memory whenever necessary.
	* \param[in] name name of the data to release
	* \pre ownership of the data buffer is shared between PDI and the user code
	* \pre PDI owns the data buffer
	*/
	void release();
	
	/** Reclaims ownership of a data buffer shared with PDI. PDI is then responsible to
	* free the associated memory whenever necessary.
	* \param[in] name name of the data to reclaim
	* \pre ownership of the data buffer is shared between PDI and the user code
	* \post the user code owns the data buffer
	*/
	void reclaim();
	
private:
	class Ref_holder
	{
	public:
		virtual Data_ref ref() const = 0;
		virtual ~Ref_holder() {}
	};
	
	template<bool R, bool W> class Ref_A_holder: public Ref_holder
	{
	public:
		Ref_A_holder(void *data, std::function<void(void *)> freefunc, Data_type_uptr type, bool readable, bool writable): m_t(data, freefunc, std::move(type), readable, writable) {}
		Ref_A_holder(Data_ref t) : m_t(t) {}
		Data_ref ref() const override
		{
			return m_t;
		}
		Data_A_ref<R, W> m_t;
	};
	
	/// References to the values of this descriptor
	std::stack<std::unique_ptr<Ref_holder>> m_refs;
	
	PC_tree_t m_config;
	
	bool m_metadata;
	
	Data_type_uptr m_type;
	
	const std::string m_name;
	
}; // class Data_descriptor

} // namespace PDI

#endif // PDI_DATA_DESCRIPTOR_H_
