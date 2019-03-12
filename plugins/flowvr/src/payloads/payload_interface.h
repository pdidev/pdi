/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_FLOWVR_PAYLOAD_INTERFACE
#define PDI_FLOWVR_PAYLOAD_INTERFACE

#include <flowvr/module.h>
#include <pdi/ref_any.h>

namespace  {

// struct Payload_interface {

// };

struct Input_payload {
	/**
	 *  Called if user accessing empty descriptor
	 *
	 *  \param[in] data_name empty descriptor name
	 */
	virtual void share(const char* data_name) = 0;
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	virtual bool data(const char* data_name, const PDI::Ref_w& ref) const = 0;
	
	/**
	 *  Receives a message payload
	 *
	 *  \return flowvr::Stamps from received message
	 */
	virtual flowvr::Stamps get_message() = 0;
	
	virtual ~Input_payload() = default;
};

struct Output_payload {
	/**
	 *  Called if user accessing empty descriptor
	 *
	 *  \param[in] data_name empty descriptor name
	 */
	virtual void share(const char* data_name) = 0;
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	virtual bool data(const char* data_name, const PDI::Ref_r& ref) = 0;
	
	/**
	 *  Send a message with payload
	 *
	 *  \param[in] stamps_write stamps to add to a message
	 *  \return flowvr::Stamps from sent message
	 */
	virtual flowvr::Stamps put_message(const flowvr::StampsWrite&) = 0;
	
	virtual ~Output_payload() = default;
};

} // namespace <anonymous>

#endif // PDI_FLOWVR_PAYLOAD_INTERFACE