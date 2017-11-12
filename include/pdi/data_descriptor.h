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

#ifndef DATA_DESCRIPTOR_H__
#define DATA_DESCRIPTOR_H__

#include <memory>

#include <paraconf.h>
#include <pdi.h>

#include "pdi/datatype.h"

namespace PDI {

/** Describe the content of a buffer.
 */
class Data_descriptor
{
public:
	/** Create empty descriptor
	 */
	Data_descriptor() = default;
	
	Data_descriptor(const Data_descriptor &);
	
	~Data_descriptor();
	
	PDI_status_t init(PC_tree_t config, bool is_metadata, const PDI_datatype_t &type);  ///< initialized descriptor
	
	Data_descriptor &operator= (const Data_descriptor &);  ///< Copy operator
	
	/** Return the datatype
	 */
	const PDI_datatype_t &get_type() const;
	
	/** Return the PC_tree_t config
	 */
	PC_tree_t get_config() const;
	
	/** Return true if the data is a metadata
	 */
	bool is_metadata() const;
	
private:
	PC_tree_t m_config;
	
	bool m_metadata;
	
	PDI_datatype_t m_type;
	
}; // class Data_descriptor

} // namespace PDI 

#endif // DATA_DESCRIPTOR_H__
