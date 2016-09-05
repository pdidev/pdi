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

#ifndef PDI_STATE_FWD_H__
#define PDI_STATE_FWD_H__

/** Describes the state of a PDI instanciation, its configuration, the
 *  currently exposed data, etc...
 */
typedef struct PDI_state_s PDI_state_t;

/** This is a parameter (a.k.a. metadata) as decribed in the configuration and
 *  its last exposed value if available
 */
typedef struct PDI_param_s PDI_param_t;

/** This is a variable (a.k.a. data) as decribed in the configuration and
 *  its shared value if not reclaimed yet
 */
typedef struct PDI_variable_s PDI_variable_t;

/** This is a variable (a.k.a. data) as decribed in the configuration and
 *  its shared value if not reclaimed yet
 */
typedef struct PDI_variable_value_s PDI_variable_value_t;

/** This is the information about a plugin loaded by the implementation
 */
typedef struct loaded_plugin_s loaded_plugin_t;

#endif // PDI_STATE_FWD_H__
