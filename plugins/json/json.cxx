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
#include <iostream>
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

#include <nlohmann/json.hpp>

namespace {

using std::dynamic_pointer_cast;
using std::fstream, std::ios;
using std::pair, std::make_pair;
using std::string;
using std::unordered_map;
using std::filesystem::path;

using namespace PDI;

/** The json plugin 
*/
class json_plugin: public PDI::Plugin
{
	// Map between data variables and a pair between condition and the output filenames
	unordered_map<string, std::vector<pair<Expression, Expression>>> m_data_to_path_map;

public:
	json_plugin(Context& ctx, PC_tree_t spec_tree)
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
			if (to_string(PC_get(elem_tree, "{%d}", 0)) == "file") {
				Expression filepath = to_string(PC_get(elem_tree, ".file"));

				Expression default_when = 1L;
				each(elem_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
					string key = to_string(key_tree);

					if (key == "when") {
						default_when = to_string(value_tree);
					} else if (key != "file" && key != "write") {
						throw Config_error{key_tree, "Unknown keyword '{}' encountered while expecting file, when, or write", key};
					}
				});

				each(elem_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
					string key = to_string(key_tree);

					if (key == "write") {
						PC_tree_t write_tree = PC_get(elem_tree, ".write");

						if (!PC_status(PC_get(write_tree, "[0]"))) { // it's a list of names only
							each(write_tree, [&](PC_tree_t tree) {
								string dset_string = to_string(tree);

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
				string dset_string = to_string(PC_get(elem_tree, "{%d}", 0));
				Expression filepath = to_string(PC_get(elem_tree, "<%d>", 0));

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

	void write_scalar_to_json(nlohmann::json& json_data, Ref_r reference)
	{
		auto scalar_type = dynamic_pointer_cast<const Scalar_datatype>(reference.type());
		if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED) {
			if (scalar_type->buffersize() == 1L) {
				json_data = std::string(1, reference.scalar_value<char>());
			} else if (scalar_type->buffersize() == 2L) {
				json_data = reference.scalar_value<uint16_t>();
			} else if (scalar_type->buffersize() == 4L) {
				json_data = reference.scalar_value<uint32_t>();
			} else if (scalar_type->buffersize() == 8L) {
				json_data = reference.scalar_value<uint64_t>();
			} else {
				throw Type_error{"Unknown size of unsigned integer datatype"};
			}
		} else if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
			if (scalar_type->buffersize() == 1L) {
				json_data = reference.scalar_value<int8_t>();
			} else if (scalar_type->buffersize() == 2L) {
				json_data = reference.scalar_value<int16_t>();
			} else if (scalar_type->buffersize() == 4L) {
				json_data = reference.scalar_value<int32_t>();
			} else if (scalar_type->buffersize() == 8L) {
				json_data = reference.scalar_value<int64_t>();
			} else {
				throw Type_error{"Unknown size of signed integer datatype"};
			}
		} else if (scalar_type->kind() == PDI::Scalar_kind::FLOAT) {
			if (scalar_type->buffersize() == 4L) {
				json_data = reference.scalar_value<float>();
			} else if (scalar_type->buffersize() == 8L) {
				json_data = reference.scalar_value<double>();
			} else {
				throw Type_error{"Unknown size of float datatype"};
			}
		} else {
			throw Type_error{"Unknown datatype to get value"};
		}
	}

	void push_scalar_array_to_json(nlohmann::json& json_data, Ref_r reference)
	{
		auto array_type = dynamic_pointer_cast<const Array_datatype>(reference.type());
		auto sub_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(array_type->subtype());
		if (sub_type->kind() == PDI::Scalar_kind::UNSIGNED) {
			if (sub_type->buffersize() == 1L) {
				std::string str = "";
				for (int i = 0; i < array_type->size(); i++) {
					str += *reinterpret_cast<const char*>(reinterpret_cast<const char*>(Ref_r{reference[i]}.get()));
				}
				json_data.push_back(str);
			} else if (sub_type->buffersize() == 2L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint16_t>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 4L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint32_t>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 8L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint64_t>());
				}
				json_data.push_back(jsonArray);
			} else {
				throw Type_error{"Unknown size of unsigned integer datatype"};
			}
		} else if (sub_type->kind() == PDI::Scalar_kind::SIGNED) {
			if (sub_type->buffersize() == 1L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int8_t>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 2L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int16_t>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 4L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int32_t>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 8L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int64_t>());
				}
				json_data.push_back(jsonArray);
			} else {
				throw Type_error{"Unknown size of signed integer datatype"};
			}
		} else if (sub_type->kind() == PDI::Scalar_kind::FLOAT) {
			if (sub_type->buffersize() == 4L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<float>());
				}
				json_data.push_back(jsonArray);
			} else if (sub_type->buffersize() == 8L) {
				nlohmann::json jsonArray = nlohmann::json::array();
				for (int i = 0; i < array_type->size(); i++) {
					jsonArray.push_back(Ref_r{reference[i]}.scalar_value<double>());
				}
				json_data.push_back(jsonArray);
			} else {
				throw Type_error{"Unknown size of float datatype"};
			}
		} else {
			throw Type_error{"Unknown datatype to get value"};
		}
	}

	void write_array_to_json(nlohmann::json& json_data, Ref_r reference)
	{
		auto array_type = dynamic_pointer_cast<const Array_datatype>(reference.type());
		if (const auto&& sub_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(array_type->subtype())) {
			if (sub_type->kind() == PDI::Scalar_kind::UNSIGNED) {
				if (sub_type->buffersize() == 1L) {
					std::string str = "";
					for (int i = 0; i < array_type->size(); i++) {
						str += *reinterpret_cast<const char*>(reinterpret_cast<const char*>(Ref_r{reference[i]}.get()));
					}
					json_data = str;
				} else if (sub_type->buffersize() == 2L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint16_t>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 4L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint32_t>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 8L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<uint64_t>());
					}
					json_data = jsonArray;
				} else {
					throw Type_error{"Unknown size of unsigned integer datatype"};
				}
			} else if (sub_type->kind() == PDI::Scalar_kind::SIGNED) {
				if (sub_type->buffersize() == 1L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int8_t>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 2L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int16_t>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 4L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int32_t>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 8L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<int64_t>());
					}
					json_data = jsonArray;
				} else {
					throw Type_error{"Unknown size of signed integer datatype"};
				}
			} else if (sub_type->kind() == PDI::Scalar_kind::FLOAT) {
				if (sub_type->buffersize() == 4L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<float>());
					}
					json_data = jsonArray;
				} else if (sub_type->buffersize() == 8L) {
					nlohmann::json jsonArray = nlohmann::json::array();
					for (int i = 0; i < array_type->size(); i++) {
						jsonArray.push_back(Ref_r{reference[i]}.scalar_value<double>());
					}
					json_data = jsonArray;
				} else {
					throw Type_error{"Unknown size of float datatype"};
				}
			} else {
				throw Type_error{"Unknown datatype to get value"};
			}
		} else if (const auto&& sub_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(array_type->subtype())) {
			nlohmann::json jsonArray = nlohmann::json::array();
			for (int i = 0; i < array_type->size(); i++) {
				push_scalar_array_to_json(jsonArray, Ref_r{reference[i]});
			}
			json_data = jsonArray;
		} else if (const auto&& sub_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(array_type->subtype())) {
			nlohmann::json jsonArray = nlohmann::json::array();
			for (int i = 0; i < array_type->size(); i++) {
				nlohmann::json jsonStruct;
				for (const auto& member: sub_type->members()) {
					if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(member.type())) {
						write_scalar_to_json(jsonStruct[member.name()], Ref_r{reference[i]}[member.name()]);
					} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(member.type())) {
						write_array_to_json(jsonStruct[member.name()], Ref_r{reference[i]}[member.name()]);
					} else if (const auto&& struct_type = dynamic_pointer_cast<const Record_datatype>(member.type())) {
						write_struct_to_json(jsonStruct[member.name()], Ref_r{reference[i]}[member.name()]);
					} else {
						throw Type_error{"Unknown member datatype passed to json"};
					}
				}
				jsonArray.push_back(jsonStruct);
			}
			json_data = jsonArray;
		} else if (const auto&& sub_type = std::dynamic_pointer_cast<const PDI::Pointer_datatype>(array_type->subtype())) {
			nlohmann::json jsonArray = nlohmann::json::array();
			for (int i = 0; i < array_type->size(); i++) {
				nlohmann::json jsonElement;
				Ref_r dereferenced_ref = reference[i].dereference();
				if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(dereferenced_ref.type())) {
					write_scalar_to_json(jsonElement, dereferenced_ref);
				} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(dereferenced_ref.type())) {
					write_array_to_json(jsonElement, dereferenced_ref);
				} else if (const auto&& struct_type = dynamic_pointer_cast<const Record_datatype>(dereferenced_ref.type())) {
					write_struct_to_json(jsonElement, dereferenced_ref);
				} else {
					throw Type_error{"Unknown member datatype passed to json"};
				}
				jsonArray.push_back(jsonElement);
			}
			json_data = jsonArray;
		} else {
			throw Type_error{"Unknown subtype for array"};
		}
	}

	void write_struct_to_json(nlohmann::json& json_data, Ref_r reference)
	{
		auto record_type = dynamic_pointer_cast<const Record_datatype>(reference.type());
		for (const auto& member: record_type->members()) {
			if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(member.type())) {
				write_scalar_to_json(json_data[member.name()], reference[member.name()]);
			} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(member.type())) {
				write_array_to_json(json_data[member.name()], reference[member.name()]);
			} else {
				throw Type_error{"Unknown member datatype passed to json"};
			}
		}
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

			const string filepath = fpath.to_string(context());

			logger.debug("Writing data of {} for {}", data_name, filepath);
			if (!reference) {
				logger.error("Reading permissions were not granted for {}", data_name);
			}

			nlohmann::json json_data;
			if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(reference.type())) {
				write_scalar_to_json(json_data[data_name], reference);
			} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(reference.type())) {
				write_array_to_json(json_data[data_name], reference);
			} else if (const auto&& record_type = dynamic_pointer_cast<const Record_datatype>(reference.type())) {
				write_struct_to_json(json_data[data_name], reference);
			} else if (const auto&& pointer_type = dynamic_pointer_cast<const Pointer_datatype>(reference.type())) {
				Ref dereferenced_ref = reference.dereference();
				if(!dereferenced_ref) throw Value_error{"Can't dereference with read permissions"};

				if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(dereferenced_ref.type())) {
					write_scalar_to_json(json_data[data_name], dereferenced_ref);
				} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(dereferenced_ref.type())) {
					write_array_to_json(json_data[data_name], dereferenced_ref);
				} else if (const auto&& record_type = dynamic_pointer_cast<const Record_datatype>(dereferenced_ref.type())) {
					write_struct_to_json(json_data[data_name], dereferenced_ref);
				} else {
					throw Type_error{"Unknown dereferenced datatype passed to json"};
				}
			} else {
				throw Type_error{"Unknown datatype passed to json"};
			}

			path fp(filepath);
			fstream json_file(fp, ios::in | ios::out | ios::ate);
			if (!json_file.is_open()) {
				json_file.open(fp, ios::out);
				json_file << "[\n" << json_data.dump(4) << "]"; // Append the new entry and close the array
			} else {
				// Step 2: Move to the end of the file and adjust the JSON structure
				json_file.seekg(-1, std::ios::end); // Move to the last character
				char lastChar;
				json_file.get(lastChar);

				if (lastChar == ']') {
					// The file ends with a valid array, so we can append
					json_file.seekp(-1, std::ios::end); // Move one character back to overwrite the closing ']'
					json_file << ",\n" << json_data.dump(4) << "\n]"; // Append the new entry and close the array
				} else {
					std::cerr << "File does not end with a valid JSON array. Cannot append.\n";
					json_file.close();
				}
				json_file.close();
			}
			logger.debug("Done ! {} ", data_name);
		}
	}
};

} // namespace
PDI_PLUGIN(json)
