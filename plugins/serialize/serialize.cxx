/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <functional>
#include <map>
#include <memory>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

namespace {

using std::dynamic_pointer_cast;

struct serialize_plugin: PDI::Plugin {
	/// Map of deserialized and serialized data dependency <deserialized desc_name, serialized desc_name>
	std::unordered_map<std::string, std::string> m_desc_to_serialize;

	/** Binds serialized desc with the callback remove function.
	 *  Cannot be a map - one data can be serialized mulitple times
	 *  Cannot be a multimap - need to release the last serialized data (stack)
	 *
	 *  Tuple: <serialized desc name, remove callback function, right of deserialized share>
	 */
	std::vector<std::tuple<std::string, std::function<void()>, PDI_inout_t>> m_serialized_remove_callback;

	/** Serializes data type
	 *
	 * \param type type to serialize (convert all sparse arrays and evaluate pointers)
	 * \return serialized data type
	 */
	PDI::Datatype_sptr serialize_type(PDI::Datatype_sptr type)
	{
		if (auto&& scalar_type = dynamic_pointer_cast<const PDI::Scalar_datatype>(type)) {
			return type;
		} else if (auto&& array_type = dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
			return PDI::Array_datatype::make(serialize_type(array_type->subtype()), array_type->subsize(), array_type->attributes());
		} else if (auto&& record_type = dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
			std::vector<PDI::Record_datatype::Member> serialized_members;
			size_t offset = 0;
			size_t alignment = 0;
			size_t serialized_buffersize = 0;
			for (auto&& member: record_type->members()) {
				PDI::Datatype_sptr serialized_type = serialize_type(member.type());

				size_t member_alignment = serialized_type->alignment();
				size_t spacing = (member_alignment - (offset % member_alignment)) % member_alignment;

				// add space to offset and buffersize
				offset += spacing;
				serialized_buffersize += spacing;

				serialized_members.emplace_back(offset, serialized_type, member.name());

				// move offset by the buffersize
				offset += serialized_type->buffersize();
				serialized_buffersize += serialized_type->buffersize();

				// serialized alignment (for final spacing)
				alignment = std::max(alignment, member_alignment);
			}

			// check the spacing at the end of record
			size_t spacing = (alignment - (offset % alignment)) % alignment;
			serialized_buffersize += spacing;

			return PDI::Record_datatype::make(move(serialized_members), serialized_buffersize, record_type->attributes());
		} else if (auto&& pointer_type = dynamic_pointer_cast<const PDI::Pointer_datatype>(type)) {
			return serialize_type(pointer_type->subtype());
		} else if (auto&& tuple_type = dynamic_pointer_cast<const PDI::Tuple_datatype>(type)) {
			std::vector<PDI::Tuple_datatype::Element> serialized_elements;
			size_t offset = 0;
			size_t alignment = 0;
			size_t serialized_buffersize = 0;
			for (auto&& element: tuple_type->elements()) {
				PDI::Datatype_sptr serialized_type = serialize_type(element.type());

				size_t element_alignment = serialized_type->alignment();
				size_t spacing = (element_alignment - (offset % element_alignment)) % element_alignment;

				// add space to offset and buffersize
				offset += spacing;
				serialized_buffersize += spacing;

				serialized_elements.emplace_back(offset, serialized_type);

				// move offset by the buffersize
				offset += serialized_type->buffersize();
				serialized_buffersize += serialized_type->buffersize();

				// serialized alignment (for final spacing)
				alignment = std::max(alignment, element_alignment);
			}

			// check the spacing at the end of tuple
			size_t spacing = (alignment - (offset % alignment)) % alignment;
			serialized_buffersize += spacing;

			return PDI::Tuple_datatype::make(move(serialized_elements), serialized_buffersize, tuple_type->attributes());
		} else {
			throw PDI::Type_error{"Serialize plugin: Unsupported type: {}", type->debug_string()};
		}
	}

	/** Make a serialize copy (from serialized data to serialized)
	 *
	 * \param type type of the deserialized data
	 * \param to pointer where data will be copied (serialized)
	 * \param from pointer from where get the data to copy (deserialized)
	 * \return count of copied bytes
	 */
	size_t serialize_copy(const PDI::Datatype_sptr type, void* to, const void* from)
	{
		if (auto&& scalar_type = dynamic_pointer_cast<const PDI::Scalar_datatype>(type)) {
			memcpy(to, from, scalar_type->buffersize());
			return scalar_type->buffersize();
		} else if (auto&& array_type = dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
			size_t subtype_buffersize = array_type->subtype()->buffersize();
			from = static_cast<const uint8_t*>(from) + (array_type->start() * subtype_buffersize);

			PDI::Datatype_sptr serialized_subtype = serialize_type(array_type->subtype());
			size_t subtype_alignment = serialized_subtype->alignment();

			//space_to_align is set to alignment(), because we always find the alignment in the size of alignment
			size_t space_to_align = subtype_alignment;
			to = std::align(subtype_alignment, 0, to, space_to_align);

			size_t all_bytes_copied = 0;
			for (size_t subtype_no = 0; subtype_no < array_type->subsize(); subtype_no++) {
				size_t bytes_copied = serialize_copy(array_type->subtype(), to, from);
				all_bytes_copied += bytes_copied;
				to = static_cast<uint8_t*>(to) + bytes_copied;
				from = static_cast<const uint8_t*>(from) + subtype_buffersize;
			}
			return all_bytes_copied;
		} else if (auto&& record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
			auto&& record_serialized = dynamic_pointer_cast<const PDI::Record_datatype>(serialize_type(record_type));

			int member_no = 0;
			size_t all_bytes_copied = 0;
			uint8_t* original_to = static_cast<uint8_t*>(to);
			for (auto&& member: record_type->members()) {
				//size = 0, because we know that to points to allocated memory
				to = original_to + record_serialized->members()[member_no].displacement();
				const uint8_t* member_from = static_cast<const uint8_t*>(from) + member.displacement();

				size_t bytes_copied = serialize_copy(member.type(), to, member_from);
				all_bytes_copied += bytes_copied;
				member_no++;
			}
			return all_bytes_copied;
		} else if (auto&& pointer_type = std::dynamic_pointer_cast<const PDI::Pointer_datatype>(type)) {
			return serialize_copy(pointer_type->subtype(), to, reinterpret_cast<void*>(*static_cast<const uintptr_t*>(from)));
		} else if (auto&& tuple_type = std::dynamic_pointer_cast<const PDI::Tuple_datatype>(type)) {
			auto&& tuple_serialized = dynamic_pointer_cast<const PDI::Tuple_datatype>(serialize_type(tuple_type));

			int element_no = 0;
			size_t all_bytes_copied = 0;
			uint8_t* original_to = static_cast<uint8_t*>(to);
			for (auto&& element: tuple_type->elements()) {
				//size = 0, because we know that to points to allocated memory
				to = original_to + tuple_serialized->elements()[element_no].offset();
				const uint8_t* element_from = static_cast<const uint8_t*>(from) + element.offset();

				size_t bytes_copied = serialize_copy(element.type(), to, element_from);
				all_bytes_copied += bytes_copied;
				element_no++;
			}
			return all_bytes_copied;
		} else {
			throw PDI::Type_error{"Serialize plugin: Unsupported type: {}", type->debug_string()};
		}
	}

	/** Make a deserialize copy (from serialized data to deserialized)
	 *
	 * \param type type of the deserialized data
	 * \param to pointer where data will be copied (deserialized)
	 * \param from pointer from where get the data to copy (serialized)
	 * \return count of copied bytes
	 */
	size_t deserialize_copy(const PDI::Datatype_sptr type, void* to, const void* from)
	{
		if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type)) {
			memcpy(to, from, scalar_type->buffersize());
			return scalar_type->buffersize();
		} else if (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
			PDI::Datatype_sptr serialized_subtype = serialize_type(array_type->subtype());

			to = static_cast<uint8_t*>(to) + (array_type->start() * array_type->subtype()->buffersize());
			size_t all_bytes_copied = 0;
			for (int subtype_no = 0; subtype_no < array_type->subsize(); subtype_no++) {
				size_t bytes_copied = deserialize_copy(array_type->subtype(), to, from);
				to = static_cast<uint8_t*>(to) + array_type->subtype()->buffersize();
				from = static_cast<const uint8_t*>(from) + bytes_copied;
				all_bytes_copied += bytes_copied;
			}
			return all_bytes_copied;
		} else if (auto&& record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
			auto&& record_serialized = dynamic_pointer_cast<const PDI::Record_datatype>(serialize_type(record_type));

			int member_no = 0;
			size_t all_bytes_copied = 0;
			for (auto&& member: record_type->members()) {
				uint8_t* member_to = static_cast<uint8_t*>(to) + member.displacement();
				const uint8_t* member_from = static_cast<const uint8_t*>(from) + record_serialized->members()[member_no].displacement();

				size_t bytes_copied = deserialize_copy(member.type(), member_to, member_from);
				all_bytes_copied += bytes_copied;
				member_no++;
			}
			return all_bytes_copied;
		} else if (auto&& pointer_type = std::dynamic_pointer_cast<const PDI::Pointer_datatype>(type)) {
			return deserialize_copy(pointer_type->subtype(), reinterpret_cast<void*>(*static_cast<const uintptr_t*>(to)), from);
		} else if (auto&& tuple_type = std::dynamic_pointer_cast<const PDI::Tuple_datatype>(type)) {
			auto&& tuple_serialized = dynamic_pointer_cast<const PDI::Tuple_datatype>(serialize_type(tuple_type));

			int element_no = 0;
			size_t all_bytes_copied = 0;
			for (auto&& element: tuple_type->elements()) {
				uint8_t* element_to = static_cast<uint8_t*>(to) + element.offset();
				const uint8_t* element_from = static_cast<const uint8_t*>(from) + tuple_serialized->elements()[element_no].offset();

				size_t bytes_copied = deserialize_copy(element.type(), element_to, element_from);
				all_bytes_copied += bytes_copied;
				element_no++;
			}
			return all_bytes_copied;
		} else {
			throw PDI::Type_error{"Serialize plugin: Unsupported type: {}", type->debug_string()};
		}
	}

	/** Serialize or deserialize data depending on access rights
	 *
	 * \param desc_name name of the descriptor to serialize/deserialize
	 * \param ref reference to data to serialize/deserialize
	 */
	void share_serialized(const std::string& desc_name, PDI::Ref ref)
	{
		std::string serialized_name = m_desc_to_serialize[desc_name];
		context().logger().debug("Serializing `{}` as `{}`", desc_name, serialized_name);
		PDI::Datatype_sptr serialized_type = serialize_type(ref.type());
		context().logger().debug("Type after serialization:\n {}", serialized_type->debug_string());

		if (PDI::Ref_rw ref_rw = ref) {
			context().logger().trace("PDI_INOUT -> allocate memory, serialize_copy, share PDI_INOUT, deserialize_copy on reclaim");

			context().logger().trace("Allocating memory: {} B", serialized_type->buffersize());
			PDI::Ref serialized_ref{operator new (serialized_type->buffersize()), [](void* p) { operator delete (p); }, serialized_type, true, true};

			context().logger().trace("Copy data to `{}' descriptor", serialized_name);
			size_t bytes_copied = serialize_copy(ref.type(), PDI::Ref_w{serialized_ref}.get(), ref_rw.get());
			if (bytes_copied != serialized_type->datasize()) {
				throw PDI::Value_error{"Serialize plugin: `{}' Serialized {} B of {} B", desc_name, bytes_copied, serialized_type->buffersize()};
			}

			context().logger().trace("Sharing `{}' PDI_INOUT", serialized_name);
			context().desc(serialized_name).share(serialized_ref, false, false);
			std::function<void()> remove_callback = context().callbacks().add_data_remove_callback(
				[this](const std::string& desc_name, PDI::Ref ref) { release_serialized(desc_name, ref); },
				desc_name
			);
			m_serialized_remove_callback.emplace_back(serialized_name, remove_callback, PDI_INOUT);

		} else if (PDI::Ref_r ref_r = ref) {
			context().logger().trace("PDI_OUT -> allocate memory, serialize_copy, then share PDI_OUT");

			// allocate memory
			context().logger().trace("Allocating memory: {} B", serialized_type->buffersize());
			PDI::Ref serialized_ref{operator new (serialized_type->buffersize()), [](void* p) { operator delete (p); }, serialized_type, true, true};

			// copy
			context().logger().trace("Copy data to `{}' descriptor", serialized_name);
			size_t bytes_copied = serialize_copy(ref.type(), PDI::Ref_w{serialized_ref}.get(), ref_r.get());
			if (bytes_copied != serialized_type->datasize()) {
				throw PDI::Value_error{"Serialize plugin: `{}' Serialized {} B of {} B ", desc_name, bytes_copied, serialized_type->buffersize()};
			}
			context().logger().trace("Sharing `{}' PDI_OUT", serialized_name);
			context().desc(serialized_name).share(serialized_ref, true, false);
			std::function<void()> remove_callback = context().callbacks().add_data_remove_callback(
				[this](const std::string& desc_name, PDI::Ref ref) { release_serialized(desc_name, ref); },
				desc_name
			);
			m_serialized_remove_callback.emplace_back(serialized_name, remove_callback, PDI_OUT);

		} else if (PDI::Ref_w ref_w{ref}) {
			context().logger().trace("PDI_IN -> allocate memory, share PDI_IN, then deserialize_copy on reclaim");

			// allocate memory
			context().logger().trace("Allocating memory: {} B", serialized_type->buffersize());
			PDI::Ref serialized_ref{operator new (serialized_type->buffersize()), [](void* p) { operator delete (p); }, serialized_type, false, true};

			context().logger().trace("Sharing `{}' PDI_IN", serialized_name);
			context().desc(serialized_name).share(serialized_ref, false, false);
			std::function<void()> remove_callback = context().callbacks().add_data_remove_callback(
				[this](const std::string& desc_name, PDI::Ref ref) { release_serialized(desc_name, ref); },
				desc_name
			);
			m_serialized_remove_callback.emplace_back(serialized_name, remove_callback, PDI_IN);
		}
	}

	void release_serialized(const std::string& desc_name, PDI::Ref ref)
	{
		std::string serialized_name = m_desc_to_serialize[desc_name];
		int i;
		for (i = m_serialized_remove_callback.size() - 1; i >= 0; i--) {
			if (serialized_name == std::get<0>(m_serialized_remove_callback[i])) {
				// remove callback from PDI
				std::get<1>(m_serialized_remove_callback[i])();
				break;
			}
		}
		if (i < 0) {
			throw PDI::Value_error{"Serialize plugin: Cannot release not shared serialized data: {}", serialized_name};
		}

		if (std::get<2>(m_serialized_remove_callback[i]) & PDI_IN) {
			// need to make a deserialize copy
			PDI::Ref_w ref_w{ref};
			if (!ref_w) {
				throw PDI::Right_error{"Serialize plugin: Cannot get write access to data: {}", desc_name};
			}

			// get write access, because it's for sure PDI_IN
			PDI::Ref_w serialized_ref{context().desc(serialized_name).ref()};
			if (!serialized_ref) {
				throw PDI::Right_error{"Serialize plugin: Cannot get write access to serialized data: {}", serialized_name};
			}

			size_t bytes_copied = deserialize_copy(ref.type(), ref_w.get(), serialized_ref.get());
			if (bytes_copied != serialized_ref.type()->datasize()) {
				throw PDI::Value_error{
					"Serialize plugin: `{}' Deserialized {} B of {} B",
					desc_name,
					bytes_copied,
					serialized_ref.type()->datasize()
				};
			}
		}

		context().desc(serialized_name).release();
		m_serialized_remove_callback.erase(m_serialized_remove_callback.begin() + i);
	}

	/** Reads config PC_tree_t and adds serialize callbacks
	 *
	 * \param config plugin configuration PC_tree_t
	 */
	void load_config(PC_tree_t config)
	{
		PDI::each(config, [this](PC_tree_t key, PC_tree_t value) mutable {
			std::string desc_name = PDI::to_string(key);
			m_desc_to_serialize.emplace(desc_name, PDI::to_string(value));
			context().logger().trace("`{}' will be serialized", desc_name);
			context().callbacks().add_data_callback(
				[this](const std::string& desc_name, PDI::Ref ref) { share_serialized(desc_name, ref); },
				desc_name
			);
		});
	}

	serialize_plugin(PDI::Context& ctx, PC_tree_t config)
		: PDI::Plugin{ctx}
	{
		load_config(config);
		context().logger().info("Plugin loaded successfully");
	}

	~serialize_plugin() { context().logger().info("Closing plugin"); }

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Serialize"; }
};

} // namespace

PDI_PLUGIN(serialize)
