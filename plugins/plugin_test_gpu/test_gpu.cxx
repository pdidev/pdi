/*******************************************************************************
 * Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

//
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>

//
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>

namespace test_gpu {

using std::dynamic_pointer_cast;
using std::fstream;
using std::ofstream, std::ios;
using std::pair, std::make_pair, std::tie;
using std::string, std::to_string;
using std::unordered_map;

//
using PDI::Datatype_sptr, PDI::Record_datatype, PDI::Array_datatype, PDI::Tuple_datatype;
using PDI::Expression;
using PDI::Logger;
using PDI::opt_each, PDI::each;
using PDI::Ref, PDI::Ref_r, PDI::Ref_r_gpu;
using PDI::Ref_w, PDI::Ref_w_gpu;
using PDI::Scalar_datatype, PDI::Scalar_kind;

class test_gpu_plugin: public PDI::Plugin
{
	enum request_mode_e {
		UNKNOWN = 0,
		CPU = 1,
		GPU = 2,
		BOTH = 3
		    // BOTH = CPU | GPU
	};
	
	unordered_map<string, request_mode_e> var_request_mode;
	
public:
	test_gpu_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		read_config_tree(ctx.logger(), spec_tree);
		
		ctx.callbacks().add_data_callback([&ctx, this](const string& data_name, Ref ref) {
			ctx.logger().info("ADD DATA for \"{}\" !", data_name);
			
			if (!ref.get_content()) ctx.logger().debug("No ref.get_content() ?!");
			
			// Ref_r ref_r_cpu = Ref_r{ref};
			// if (!ref_r_cpu)
			//     ctx.logger().debug("ref_r_cpu(cpu) not available ??\n");
			// else {
			//     const char* value_ptr = reinterpret_cast<const char*>(ref_r_cpu.get());
			//     ctx.logger().debug("value (cpu) is {}", to_string(*reinterpret_cast<const uint16_t*>(value_ptr)));
			// }
			
			// Ref_r_gpu ref_r_gpu = Ref_r_gpu{ref};
			// if (!ref_r_gpu)
			//     ctx.logger().debug("ref_r_gpu(gpu) not available ??\n");
			// else {
			//     const char* value_ptr = reinterpret_cast<const char*>(ref_r_gpu.get());
			//     ctx.logger().debug("value (gpu) is {}", to_string(*reinterpret_cast<const uint16_t*>(value_ptr)));
			// }
			
			//
			//////// WRITE var_out BACK INTO var_in
			
			const char endchar = data_name.back();
			if (endchar == 'r') {
				ctx.logger().warn("IS a xxxr variable {}", data_name);
				Ref_w reference_w = Ref_w{ref};
				
				// if (!reference_w) {
				//     ctx.logger().warn("NO write permissions for {}", data_name);
				// } else {
				//     ctx.logger().warn("okay I get write permissions for {}", data_name);
				
				
				//     // PDI::Data_descriptor& desc = ctx[data_name];
				
				//     // Ref_r ref_r = desc.ref();
				//     // if (!ref_r) ctx.logger().info("No ref_r\n");
				
				//     // ctx.logger().info("ref_r !\n");
				
				//     // const char* value_ptr = reinterpret_cast<const char*>(ref_r.get());
				//     // int value = *reinterpret_cast<const uint16_t*>(value_ptr);
				
				//     // ctx.logger().info("value (cpu) is " << to_string(value) << "\n");
				
				//     // std::unique_ptr<char> data_copy{new char[ref_w.type()->datasize()]};
				//     // memcpy(data_copy.get(), &value, sizeof(value));
				
				//     // ref_w.type()->data_from_dense_copy(ref_w.get(), data_copy.get());
				
				//     int value = 12;
				//     // ctx.logger().warn("ok1");
				//     std::unique_ptr<char> data_copy{new char[reference_w.type()->datasize()]};
				//     // ctx.logger().warn("ok2");
				//     memcpy(data_copy.get(), &value, sizeof(value));
				//     // ctx.logger().warn("ok3");
				
				//     reference_w.type()->data_from_dense_copy(reference_w.get(), data_copy.get());
				//     // ctx.logger().warn("ok4");
				// }
			}
			
			this->write_data(data_name, ref);
		});
		
		// ctx.callbacks().add_data_remove_callback([&ctx, this](const string &data_name,
		//                                                       Ref ref) {
		//     ctx.logger().info("REMOVE DATA " << data_name << " !\n");
		//     //
		//     //////// DIRECT PRINT VALUE
		//     Ref_r ref_r_cpu = Ref_r(ref);
		//     if (!ref_r_cpu)
		//         ctx.logger().info("ref_r_cpu(cpu) not available ??\n");
		//     else {
		//         const char *value_ptr = reinterpret_cast<const char *>(ref_r_cpu.get());
		//         ctx.logger().info("value (cpu) is )"
		//                   << to_string(*reinterpret_cast<const uint16_t *>(value_ptr)) << "\n";
		//     }
		
		//     Ref_r_gpu ref_r_gpu = Ref_r_gpu(ref);
		//     if (!ref_r_gpu)
		//         ctx.logger().info("ref_r_gpu(gpu) not available ??\n");
		//     else {
		//         const char *value_ptr = reinterpret_cast<const char *>(ref_r_gpu.get());
		//         ctx.logger().info("value (gpu) is )"
		//                   << to_string(*reinterpret_cast<const uint16_t *>(value_ptr)) << "\n";
		//     }
		// });
		ctx.logger().info("Plugin loaded successfully");
	}
	
	~test_gpu_plugin()
	{
		context().logger().info("Closing plugin");
	}
	
private:
	/** Read the configuration file
	 *
	 * \param logger PDI's logger instance
	 * \param spec_tree the yaml tree
	 */
	void read_config_tree(Logger& logger, PC_tree_t spec_tree)
	{
		if (PC_status(spec_tree)) {
			logger.error("Error in read_config_tree");
			return;
		}
		
		// Read yaml entries
		opt_each(spec_tree, [&](PC_tree_t elem_tree) {
			string var = PDI::to_string(PC_get(elem_tree, "{%d}", 0));
			string request_mode = PDI::to_string(PC_get(elem_tree, "<%d>", 0));
			
			auto iter = var_request_mode.find(var);
			if (iter != var_request_mode.end()) {
				logger.error("FOR THIS PLUGIN, VAR SHOULD'NT HAVE TWO REQUEST MODES");
			} else {
				request_mode_e mode = UNKNOWN;
				if (request_mode == "cpu")
					mode = CPU;
				else if (request_mode == "gpu")
					mode = GPU;
				else if (request_mode == "both")
					mode = BOTH;
					
				var_request_mode.insert(make_pair(var, mode));
			}
		});
	}
	
	/** Convert a scalar value pointer to a string
	 *
	 * \param logger PDI's logger instance
	 * \param reference A reference to a scalar datatype
	 */
	string scalar_ref_to_string(Logger& logger, const Ref_r reference)
	{
		const char* value_ptr = reinterpret_cast<const char*>(reference.get());
		const Datatype_sptr type = reference.type();
		
		auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type);
		if (!scalar_type) {
			logger.error("Reference is not of Scalar datatype ?");
			return "not a scalar";
		}
		// if (!scalar_type->buffersize()) logger.error("Buffersize is 0 ... invalid configuration file");
		
		switch (scalar_type->kind()) {
		case Scalar_kind::UNSIGNED:
			switch (scalar_type->buffersize()) {
			case 1L: { // by default char
				string str(1, *reinterpret_cast<const char*>(value_ptr));
				return "\"" + str + "\"";
			}
			case 2L:
				return to_string(*reinterpret_cast<const uint16_t*>(value_ptr));
			case 4L:
				return to_string(*reinterpret_cast<const uint32_t*>(value_ptr));
			case 8L:
				return to_string(*reinterpret_cast<const uint64_t*>(value_ptr));
			default:
				logger.error("Unknown size of unsigned integer datatype");
			}
			break;
		case Scalar_kind::SIGNED:
			switch (scalar_type->buffersize()) {
			case 1L:
				return to_string(*reinterpret_cast<const int8_t*>(value_ptr));
			case 2L:
				return to_string(*reinterpret_cast<const int16_t*>(value_ptr));
			case 4L:
				return to_string(*reinterpret_cast<const int32_t*>(value_ptr));
			case 8L:
				return to_string(*reinterpret_cast<const int64_t*>(value_ptr));
			default:
				logger.error("Unknown size of signed integer datatype");
			}
			break;
		case Scalar_kind::FLOAT:
			switch (scalar_type->buffersize()) {
			case 4L:
				return to_string(*reinterpret_cast<const float*>(value_ptr));
			case 8L:
				return to_string(*reinterpret_cast<const double*>(value_ptr));
			default:
				logger.error("Unknown size of float integer datatype");
			}
			break;
		case Scalar_kind::UNKNOWN:
		default:
			logger.error("Unknown scalar type");
		}
		
		return "unknown";
	}
	
	/** Read the configuration file
	 *
	 * \param logger PDI's logger instance
	 * \param reference the reference with read permissions
	 * \param cur_indent the current indentation size. Needed for JSON formating
	 * \param indent_size the indentation size. Needed for JSON formating
	 */
	string ref_to_string(Logger& logger, Ref_r reference, size_t cur_indent, size_t indent_size = 2)
	{
		const Datatype_sptr type = reference.type();
		
		if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
			// if (!scalar_type->buffersize()) logger.error("buffersize is 0 ... invalid configuration file");
			return scalar_ref_to_string(logger, reference);
		} else if (dynamic_pointer_cast<const Array_datatype>(type) || dynamic_pointer_cast<const Tuple_datatype>(type)) {
			int const size = [&]() -> int {
				if (auto local_type = dynamic_pointer_cast<const Array_datatype>(type))
				{
					return local_type->size();
				} else if (auto local_type = dynamic_pointer_cast<const Tuple_datatype>(type))
				{
					return local_type->size();
				}
				return 0;
			}();
			
			string array = "[\n";
			for (int i = 0; i < size; i++) {
				array += string(cur_indent + indent_size, ' ') + ref_to_string(logger, reference[i], cur_indent + indent_size, indent_size);
				array += (i < size - 1 ? ",\n" : "\n" + string(cur_indent, ' ') + "]");
			}
			return array;
		} else if (auto&& record_type = dynamic_pointer_cast<const Record_datatype>(type)) {
			string map = "{";
			
			for (auto const& member: record_type->members()) {
				map += "\n" + string(cur_indent + indent_size, ' ') + "\"" + member.name() + "\": ";
				map += ref_to_string(logger, reference[member.name()], cur_indent + indent_size, indent_size) + ",";
			}
			map.back() = '\n';
			map += string(cur_indent, ' ') + "}";
			return map;
			// } else if (auto&& pointer_type = dynamic_pointer_cast<const PDI::Pointer_datatype>(type)) {
			//     Ref dereferenced_ref = reference.dereference();
			//     if (!dereferenced_ref) return logger.warn("Can't dereference with read permissions"), "";
			//     return ref_to_string(logger, dereferenced_ref, cur_indent + indent_size, indent_size);
		} else {
			return logger.error("unknown Datatype"), "";
		}
		return "";
	}
	
	/* Write data in JSON format to file */
	void write_data(const string& data_name, Ref ref)
	{
		Logger& logger = context().logger();
		request_mode_e mode = var_request_mode[data_name];
		
		// logger.debug("var : {}", string(mode));
		if (var_request_mode.find(data_name) == var_request_mode.end() || mode == UNKNOWN) request_mode_e mode = BOTH;
		
		// string const filepath = "test_" + data_name + ".out";
		string const filepath = "test_var.out";
		// logger.debug("Writing data of {} for {}", data_name, filepath);
		
		string str = "----------\n";
		if (mode & CPU) {
			logger.warn("CPU");
			Ref_r ref_cpu = Ref_r{ref};
			if (ref_cpu)
				str += data_name + " | CPU : " + ref_to_string(logger, ref_cpu, 0, 2) + "\n";
			else
				logger.error("CAN'T read ref_cpu for {} for {}", data_name, filepath);
		}
		
		if (mode & GPU) {
			logger.warn("GPU");
			Ref_r_gpu ref_gpu = Ref_r_gpu{ref};
			if (ref_gpu)
				str += data_name + " | GPU : " + ref_to_string(logger, ref_gpu, 0, 2) + "\n";
			else
				logger.error("CAN'T read ref_gpu for {} for {}", data_name, filepath);
		}
		
		
		string data_name_w = data_name + "r";
		logger.warn("SET VALUE : CPU MODE");
		
		PDI::Context::Iterator it = context().begin();
		for (; it != context().end(); ++it) {
			if ((*it).name() == data_name_w) break;
		}
		
		// auto member_it = find_if(context().begin(), members().end(), [=](const std::string& name) { return name == member.name(); });
		// PDI::Context::Iterator it = std::find_if(context().begin(), context().end(), [&](const auto& data_desc) {
		//  return data_desc->name() == data_name_w;
		// });
		
		// PDI::Context::Iterator it = std::find_if(context().begin(), context().end(), [&](const auto& elem){ return elem->name() == data_name + "r"; });
		
		// if (context().find(data_name_w)) {
		if (it != context().end()) {
			logger.info("Variable {} FOUND", data_name_w);
			PDI::Ref ref_to_write{context().desc(data_name_w).ref()};
			logger.warn("REF_W");
			
			// int buffersize;
			// if (mode & CPU) {
			//     Ref_r ref_cpu = Ref_r{ref};
			//     int const buffersize = ref_cpu
			// }
			// if (mode & GPU) Ref_r_gpu ref_gpu = Ref_r_gpu{ref};
			
			// int const buffersize = [&]() -> int {
			//     if (mode & CPU)
			//         return ref_cpu.type()->buffersize();
			//     else if (mode & GPU)
			//         return ref_gpu.type()->buffersize();
			//     return 0;
			// }();
			
			if (PDI::Ref_w ref_to_write_w{ref_to_write}) {
				logger.warn("GOT WRITE PERMISSION");
				
				int buffersize;
				const void* data_ptr;
				
				if (mode & CPU) {
					Ref_r ref_cpu = Ref_r{ref};
					buffersize = ref_cpu.type()->buffersize();
					data_ptr = const_cast<void*>(ref_cpu.get());
					// const void* base_scalar_ptr = ref_cpu.get();
					// reinterpret_cast<int*>(const_cast<void*>(base_scalar_ptr));
					
				} else if (mode & GPU) {
					Ref_r_gpu ref_gpu = Ref_r_gpu{ref};
					buffersize = ref_gpu.type()->buffersize();
					data_ptr = const_cast<void*>(ref_gpu.get());
				}
				
				if (ref_to_write_w.type()->buffersize() != buffersize) {
					throw PDI::Value_error{
						"Cannot set value to existing reference. Existing buffersize = {}, value buffersize = {}",
						ref_to_write_w.type()->buffersize(),
						buffersize};
				}
				logger.trace("Copy value to {} with size {} B", data_name_w, buffersize);
				memcpy(ref_to_write_w.get(), data_ptr, ref_to_write_w.type()->buffersize());
			} else {
				logger.warn("NOOOOO WRITE PERMISSION");
				throw PDI::Right_error{"Cannot get write access for `{}' to set values", data_name_w};
			}
		} else {
			logger.warn("Variable {} NOT FOUND", data_name_w);
		}
		
		// if (ref_r_cpu) logger.debug("Can read ref_r_cpu for {} for {}", data_name, filepath);
		// else logger.error("CAN'T read ref_r_cpu for {} for {}", data_name, filepath);
		// if (ref_r_gpu) logger.debug("Can read ref_r_gpu for {} for {}", data_name, filepath);
		// else logger.error("CAN'T read ref_r_gpu for {} for {}", data_name, filepath);
		
		fstream file(filepath, ios::in | ios::out | ios::ate);
		
		if (!file.is_open()) { // Create file if doesn't exist
			file.open(filepath, ios::out);
			file << "[\n";
		} else {
			file.seekp(-1, ios::end);
			if (file.tellp() > 1) {
				file.seekp(-1, ios::cur); // Seek to second last char
				file.trunc;
			}
			// file << ",\n";
		}
		
		file << str << "]\n";
		
		file.flush();
		file.close();
		
		logger.debug("Done ! {} ", data_name);
	}
};

PDI_PLUGIN(test_gpu)

} // namespace test_gpu
