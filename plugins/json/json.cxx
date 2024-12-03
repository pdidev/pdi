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

	/** Write to json a scalar data 
	 *
	 * \param json_data A json data to which we write a scalar data
	 * \param reference A reference to a scalar datatype
	 * \param logger PDI's logger instance
	 */
	void write_scalar_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		auto scalar_type = dynamic_pointer_cast<const Scalar_datatype>(reference.type());
		if (scalar_type->kind() == Scalar_kind::UNSIGNED) {
			if (scalar_type->buffersize() == 1L) {
				json_data = string(1, reference.scalar_value<char>());
			} else if (scalar_type->buffersize() == 2L) {
				json_data = reference.scalar_value<uint16_t>();
			} else if (scalar_type->buffersize() == 4L) {
				json_data = reference.scalar_value<uint32_t>();
			} else if (scalar_type->buffersize() == 8L) {
				json_data = reference.scalar_value<uint64_t>();
			} else {
				throw Type_error{"Unknown size of unsigned integer datatype"};
			}
		} else if (scalar_type->kind() == Scalar_kind::SIGNED) {
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
		} else if (scalar_type->kind() == Scalar_kind::FLOAT) {
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

	/** Write to json an array data
	 *
	 * \param json_data A json data to which we write an array data
	 * \param reference A reference to an array datatype
	 * \param logger PDI's logger instance
	 */
	void write_array_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		auto array_type = dynamic_pointer_cast<const Array_datatype>(reference.type());
		if (const auto&& sub_type = dynamic_pointer_cast<const Scalar_datatype>(array_type->subtype())) {
			if (sub_type->kind() == Scalar_kind::UNSIGNED && sub_type->buffersize() == 1L) {
				string str = "";
				for (int i = 0; i < array_type->size(); i++) {
					str += *reinterpret_cast<const char*>(reinterpret_cast<const char*>(Ref_r{reference[i]}.get()));
				}
				json_data.push_back(str);
				return;
			}
		}
		for (int i = 0; i < array_type->size(); i++) {
			if (const auto&& sub_type = dynamic_pointer_cast<const Scalar_datatype>(array_type->subtype())) { // an array of scalars
				nlohmann::json result;
				write_scalar_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Array_datatype>(array_type->subtype())) { // an array of arrays
				nlohmann::json result = nlohmann::json::array();
				write_array_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Record_datatype>(array_type->subtype())) { // an array of arrays
				nlohmann::json result;
				write_record_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Pointer_datatype>(array_type->subtype())) {
				nlohmann::json result = nlohmann::json::array();
				write_pointer_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Tuple_datatype>(array_type->subtype())) {
				nlohmann::json result = nlohmann::json::array();
				write_pointer_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			}
		}
	}

	/** Write to json a record data
	 *
	 * \param json_data A json data to which we write a record data
	 * \param reference A reference to a record datatype
	 * \param logger PDI's logger instance
	 */
	void write_record_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		auto record_type = dynamic_pointer_cast<const Record_datatype>(reference.type());
		for (const auto& member: record_type->members()) {
			if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(member.type())) { // scalar member of the record
				write_scalar_to_json(json_data[member.name()], reference[member.name()], logger);
			} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(member.type())) { // array member of the record
				write_array_to_json(json_data[member.name()], Ref_r{reference[member.name()]}, logger);
			} else if (const auto&& array_type = dynamic_pointer_cast<const Pointer_datatype>(member.type())) { // array member of the record
				write_pointer_to_json(json_data[member.name()], Ref_r{reference[member.name()]}, logger);
			} else if (const auto&& array_type = dynamic_pointer_cast<const Tuple_datatype>(member.type())) { // array member of the record
				logger.warn("record of tuple not yet tested");
				write_tuple_to_json(json_data[member.name()], Ref_r{reference[member.name()]}, logger);
			} else {
				throw Type_error{"Unknown member datatype passed to json"};
			}
		}
	}

	/** Write to json a tuple data
	 *
	 * \param json_data A json data to which we write a tuple data
	 * \param reference A reference to a tuple datatype
	 * \param logger PDI's logger instance
	 */
	void write_tuple_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		auto tuple_type = dynamic_pointer_cast<const Tuple_datatype>(reference.type());
		for (int i = 0; i < tuple_type->size(); i++) {
			if (const auto&& sub_type = dynamic_pointer_cast<const Scalar_datatype>(tuple_type->elements()[i].type()))
			{ // scalar member of the record
				nlohmann::json result;
				write_scalar_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Array_datatype>(tuple_type->elements()[i].type()))
			{ // scalar member of the record
				nlohmann::json result;
				write_array_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Pointer_datatype>(tuple_type->elements()[i].type()))
			{ // scalar member of the record
				nlohmann::json result;
				write_pointer_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else if (const auto&& sub_type = dynamic_pointer_cast<const Record_datatype>(tuple_type->elements()[i].type()))
			{ // scalar member of the record
				nlohmann::json result;
				write_record_to_json(result, Ref_r{reference[i]}, logger);
				json_data.push_back(result);
			} else {
				throw Type_error{"Unknown tuple subtype passed to json, currently supprting scalar and array subtypes."};
			}
		}
	}

    /** Write to json a pointer data
	 *
	 * \param json_data A json data to which we write a pointer data
	 * \param reference A reference to a tuple datatype
	 * \param logger PDI's logger instance
	 */
	void write_pointer_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		Ref dereferenced_ref = reference.dereference();
		if (!dereferenced_ref) throw Value_error{"Can't dereference with read permissions"};
		choose_type_and_dump_to_json(json_data, dereferenced_ref, logger);
	}

	/** Call different json dump function accordint to the datatype
	 *
	 * \param json_data A json data to which we write various types of data
	 * \param reference A reference to a datatype
	 * \param logger PDI's logger instance
	 */
	void choose_type_and_dump_to_json(nlohmann::json& json_data, Ref_r reference, Logger& logger)
	{
		if (const auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(reference.type())) { // a scalar type
			write_scalar_to_json(json_data, reference, logger);
		} else if (const auto&& array_type = dynamic_pointer_cast<const Array_datatype>(reference.type())) { // an array type
			write_array_to_json(json_data, reference, logger);
		} else if (const auto&& record_type = dynamic_pointer_cast<const Record_datatype>(reference.type())) { // a record type
			write_record_to_json(json_data, reference, logger);
		} else if (const auto&& pointer_type = dynamic_pointer_cast<const Tuple_datatype>(reference.type())) {
			write_tuple_to_json(json_data, reference, logger);
		} else if (const auto&& pointer_type = dynamic_pointer_cast<const Pointer_datatype>(reference.type())) { // a pointer type
			write_pointer_to_json(json_data, reference, logger);
		} else {
			throw Type_error{"Unknown datatype passed to json"};
		}
	}

	/** Write the variable to a JSON file
	 *
	 * \param data_name the variable name shared from PDI
	 * \param indent_size reference The reference with read permission to this variable
	 */
	void write_data(const string& data_name, Ref_r reference)
	{
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
			choose_type_and_dump_to_json(json_data[data_name], reference, logger);

			path fp(filepath);
			fstream json_file(fp, ios::in | ios::out | ios::ate);
			if (!json_file.is_open()) {
				// Case 1: File doesn't exist
				// Create the file
				// Write the json data between brackets
				json_file.open(fp, ios::out);
				json_file << "[\n" << json_data.dump(4) << "]";
			} else {
				// Case 2: File exist
				// Move to the end of the file
				// Remove the closing bracket
				// Write the json data
				// Add the closing bracket
				json_file.seekg(-1, ios::end); // Move to the last character
				char lastChar;
				json_file.get(lastChar);
				if (lastChar == ']') {
					json_file.seekp(-1, ios::end); // Move one character back to overwrite the closing ']'
					json_file << ",\n" << json_data.dump(4) << "\n]";
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
