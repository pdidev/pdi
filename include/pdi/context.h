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

#ifndef PDI_STATE_H_
#define PDI_STATE_H_

#include <mpi.h>

#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/fwd.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>


namespace PDI
{

class PDI_EXPORT Context
{
public:
	friend class Data_descriptor;
	
	/** An iterator used to go through the descriptor store.
	 *
	 * Implemented as a wrapper for a map iterator that hides the key part.
	 */
	class Iterator
	{
	public:
		friend class Context;
		Data_descriptor &operator-> ();
		Data_descriptor &operator* ();
		Iterator &operator++ ();
		bool operator!= (const Iterator &o);
	private:
		/// The iterator this wraps
		std::unordered_map<std::string, Data_descriptor>::iterator m_data;
		Iterator(const std::unordered_map<std::string, Data_descriptor>::iterator &data): m_data(data) {}
		Iterator(std::unordered_map<std::string, Data_descriptor>::iterator &&data): m_data(std::move(data)) {}
	};
	
private:
	/// The loaded plugins
	std::unordered_map<std::string, std::unique_ptr<Plugin>> plugins;
	
	/// Descriptors of the data
	std::unordered_map<std::string, Data_descriptor> m_descriptors;
	
	Context(const Context &) = delete;
	
	Context(Context &&) = delete;
	
public:
	Context(PC_tree_t conf, MPI_Comm *world);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor &desc(const std::string &name);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor &desc(const char *name);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor &operator[](const std::string &name);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor &operator[](const char *name);
	
	/** Returns an iterator on the first descriptor
	 */
	Iterator begin();
	
	/** Returns an iterator past the last descriptor
	 */
	Iterator end();
	
	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	void event(const char *name);
	
};

} // namespace PDI

#endif // PDI_STATE_H_
