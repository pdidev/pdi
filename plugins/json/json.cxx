/*******************************************************************************
 * Copyright (C) 2023-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

namespace {

using std::dynamic_pointer_cast;
using std::fstream, std::ios;
using std::pair, std::make_pair, std::tie;
using std::string, std::to_string;
using std::unordered_map;
using std::filesystem::path;

using PDI::Config_error;
using PDI::Datatype_sptr, PDI::Record_datatype, PDI::Array_datatype, PDI::Tuple_datatype;
using PDI::Expression;
using PDI::Logger;
using PDI::opt_each, PDI::each;
using PDI::Ref, PDI::Ref_r;
using PDI::Scalar_datatype, PDI::Scalar_kind;

/** The json plugin 
*/
class json_plugin: public PDI::Plugin
{
	// Map between data variables and a pair between condition and the output filenames
	unordered_map<string, std::vector<pair<Expression, Expression>>> m_data_to_path_map;

public:
	json_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		// initialize m_data_to_path_map from config.yml
		read_config_tree(ctx.logger(), spec_tree);

		for (const auto& data_path_pair: m_data_to_path_map) {
			ctx.callbacks().add_data_callback([this](const string& data_name, Ref ref) { this->write_data(data_name, ref); }, data_path_pair.first);
		}

		ctx.logger().info("Plugin loaded successfully");
	}

	~json_plugin() { context().logger().info("Closing plugin"); }

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
		if (!scalar_type->buffersize()) logger.error("Buffersize is 0 ... invalid configuration file");

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
			if (PDI::to_string(PC_get(elem_tree, "{%d}", 0)) == "file") {
				Expression filepath = PDI::to_string(PC_get(elem_tree, ".file"));

				Expression default_when = 1L;
				each(elem_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
					string key = PDI::to_string(key_tree);

					if (key == "when") {
						default_when = PDI::to_string(value_tree);
					} else if (key != "file" && key != "write") {
						throw Config_error{key_tree, "Unknown keyword '{}' encountered while expecting file, when, or write", key};
					}
				});

				each(elem_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
					string key = PDI::to_string(key_tree);

					if (key == "write") {
						PC_tree_t write_tree = PC_get(elem_tree, ".write");

						if (!PC_status(PC_get(write_tree, "[0]"))) { // it's a list of names only
							each(write_tree, [&](PC_tree_t tree) {
								string dset_string = PDI::to_string(tree);

								// Append to list if key exist, else create it
								auto iter = m_data_to_path_map.find(dset_string);
								if (iter != m_data_to_path_map.end()) {
									iter->second.push_back(make_pair(default_when, filepath));
								} else {
									m_data_to_path_map.insert(
										make_pair(dset_string, std::vector<pair<Expression, Expression>>{make_pair(default_when, filepath)})
									);
								}
							});
						} else {
							throw Config_error{key_tree, "Unknown write method. Please use [var1, var2, ...]"};
						}
					}
				});
			} else { // it's "var: filename" format
				string dset_string = PDI::to_string(PC_get(elem_tree, "{%d}", 0));
				Expression filepath = PDI::to_string(PC_get(elem_tree, "<%d>", 0));

				auto iter = m_data_to_path_map.find(dset_string);
				Expression default_when = 1L;
				if (iter != m_data_to_path_map.end()) {
					iter->second.push_back(make_pair(default_when, filepath));
				} else {
					m_data_to_path_map.insert(make_pair(dset_string, std::vector<pair<Expression, Expression>>{make_pair(default_when, filepath)}));
				}
			}
		});
	}

	/** Convert reference to string for writing to file
	 *
	 * \param logger PDI's logger instance
	 * \param reference the reference with read permissions
	 * \param cur_indent the current indentation size. Needed for JSON formating
	 * \param indent_size the indentation size. Needed for JSON formating
	 */
	string ref_to_string(Logger& logger, Ref_r reference, size_t cur_indent, size_t indent_size = 2)
	{
		const Datatype_sptr type = reference.type();

		if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
			return scalar_ref_to_string(logger, reference);
		} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(type)) {
			// If the array subtype is a char : interpret it as a string
			if (auto array_of_scalars_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(array_type->subtype())) {
				if (array_of_scalars_type->kind() == Scalar_kind::UNSIGNED && array_of_scalars_type->buffersize() == 1L) {
					std::string str = "\"";
					for (int i = 0; i < array_type->size(); i++) {
						str += *reinterpret_cast<const char*>(reinterpret_cast<const char*>(Ref_r{reference[i]}.get()));
					}
					return str + "\"";
				}
			}
			string array = "[\n";

			for (int i = 0; i < array_type->size(); i++) {
				array += string(cur_indent + indent_size, ' ') + ref_to_string(logger, reference[i], cur_indent + indent_size, indent_size);
				array += (i < array_type->size() - 1 ? ",\n" : "\n" + string(cur_indent, ' ') + "]");
			}
			return array;
		} else if (const auto&& tuple_type = dynamic_pointer_cast<const Tuple_datatype>(type)) {
			string tuple = "[\n";

			for (int i = 0; i < tuple_type->size(); i++) {
				tuple += string(cur_indent + indent_size, ' ') + ref_to_string(logger, reference[i], cur_indent + indent_size, indent_size);
				tuple += (i < tuple_type->size() - 1 ? ",\n" : "\n" + string(cur_indent, ' ') + "]");
			}
			return tuple;
		} else if (const auto&& record_type = dynamic_pointer_cast<const Record_datatype>(type)) {
			string map = "{";

			for (const auto& member: record_type->members()) {
				map += "\n" + string(cur_indent + indent_size, ' ') + "\"" + member.name() + "\": ";
				map += ref_to_string(logger, reference[member.name()], cur_indent + indent_size, indent_size) + ",";
			}
			map.back() = '\n';
			map += string(cur_indent, ' ') + "}";
			return map;
		} else if (auto&& pointer_type = dynamic_pointer_cast<const PDI::Pointer_datatype>(type)) {
			Ref dereferenced_ref = reference.dereference();
			if (!dereferenced_ref) return logger.warn("Can't dereference with read permissions"), "";
			return ref_to_string(logger, dereferenced_ref, cur_indent + indent_size, indent_size);
		} else {
			return logger.error("unknown Datatype"), "";
		}
		return "";
	}

	/** Write the variable to a JSON file
	 *
	 * \param data_name the variable name shared from PDI
	 * \param indent_size reference The reference with read permission to this variable
	 */
	void write_data(const string& data_name, Ref_r reference)
	{
		// TODO: use a json library instead of native fstream
		Logger& logger = context().logger();

		for (const auto& [condition, fpath]: m_data_to_path_map[data_name]) {
			if (!condition.to_long(context())) {
				logger.debug("Condition for {} isn't verified !", data_name);
				continue;
			}

			string const filepath = fpath.to_string(context());

			logger.debug("Writing data of {} for {}", data_name, filepath);
			if (!reference) {
				logger.error("Reading permissions were not granted for {}", data_name);
			}

			string const str = "{\"" + data_name + "\": " + ref_to_string(logger, reference, 0, 2) + "}\n]";

			path fp(filepath);
			fstream file(fp, ios::in | ios::out | ios::ate);

			if (!file.is_open()) { // Create file if doesn't exist
				file.open(fp, ios::out);
				if (!file) {
					throw std::runtime_error("Error: Could not create or open the file.");
				}
				file << "[\n";
				file.close();
				file.open(fp, std::ios::in | std::ios::out | std::ios::ate); // Reopen in read/write mode
			}

			if (file.tellp() > 2) { // check if the file has existing entries
				file.seekp(-2, ios::end); // get to the very end of the last entry
				file << ",\n";
			} else {
				file.seekp(0, ios::end);
			}

			file << str;

			file.flush();
			file.close();

			logger.debug("Done ! {} ", data_name);
		}
	}
};

} // namespace
PDI_PLUGIN(json)
