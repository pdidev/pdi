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

//The following is used for doxygen documentation:
/**
* \file data_fwd.h
* \brief public PDI data declaration
* \author J. Bigot (CEA)
*/

#ifndef PDI_DATA_FWD_H__
#define PDI_DATA_FWD_H__

/** the possible kind of data
 */
typedef enum PDI_datakind_e {
	PDI_DK_DATA = 0,
	PDI_DK_METADATA
} PDI_datakind_t;


/** the possible kind of data
 */
typedef enum PDI_memmode_e {
	PDI_MM_NONE = 0,
	/// PDI is responsible for freeing the memory
	PDI_MM_FREE = 0x08, // start at 8 so as to be ORable w. PDI_inout_t
	/** The data is a copy stored in the compact (dense) format.
	 * Implies PDI_MM_FREE.
	 */
	PDI_MM_COPY = 0x10
} PDI_memmode_t;


/** Data as described in the configuration and its shared value if not reclaimed
 *  yet
 */
typedef struct PDI_data_s PDI_data_t;


/** The value of some data
 */
typedef struct PDI_data_value_s PDI_data_value_t;

#endif // PDI_DATA_FWD_H__

