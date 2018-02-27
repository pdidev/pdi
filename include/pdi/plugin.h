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

#ifndef PDI_PLUGIN_H_
#define PDI_PLUGIN_H_

#include <pdi/fwd.h>


namespace PDI {

class PDI_EXPORT Plugin
{
private:
	Context& m_context;
	
public:
	Plugin(Context& ctx);
	
	virtual ~Plugin();
	
	/** Skeleton of the function called to notify an event
	 * \param[in] event the event name
	 * \return an exit status code
	 */
	virtual void event(const char* event);
	
	/** Skeleton of the function called to notify that some data becomes available
	 * \param name the name of the data made available
	 * \param ref available data
	 * \return an exit status code
	 */
	virtual void data(const char* name, Data_ref ref);
	
protected:
	Context& context();
	
}; // class Plugin

} // namespace PDI

/** Declares a plugin.
 *
 * This should be called after having implemeted the five required functions
 * for a PDI plugin:
 * - PDI_&lt;name&gt;_finalize;\
 * - PDI_&lt;name&gt;_event;\
 * - PDI_&lt;name&gt;_data_start;\
 * - PDI_&lt;name&gt;_data_end;\
 * - PDI_&lt;name&gt;_init(conf, world);\
 *
 * \param name the name of the plugin
 */
#define PDI_PLUGIN(name)\
	extern "C" ::std::unique_ptr<::PDI::Plugin> PDI_EXPORT PDI_plugin_##name##_loader(::PDI::Context& ctx, PC_tree_t conf, MPI_Comm *world) \
	{\
		return ::std::unique_ptr<name##_plugin>{new name##_plugin{ctx, conf, world}};\
	}

#endif // PDI_PLUGIN_H_
