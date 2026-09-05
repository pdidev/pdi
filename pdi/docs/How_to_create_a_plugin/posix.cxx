/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the names of CEA, nor the names of the contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/

#include <sys/stat.h>

#include <cstdio>
#include <fstream>
#include <memory>
#include <string>
#include <unordered_map>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

/// The specification tree this plugin reads:
const char* CONFIG_YAML =
	R"PDIYAML(
#! [yaml]
data:
  some_data: {type: array, subtype: int, size: 64}
  can_recover_data: int

plugins:
  posix:
    data:
      some_data: /file_path/
    can_recover_all: can_recover_data
#! [yaml]
)PDIYAML";

namespace {

class posix_plugin: public PDI::Plugin
{
	//! [members]
	std::string m_can_recover_data;
	std::unordered_map<std::string, std::string> m_data_to_path_map;

	//! [members]

	//! [read_recover_tree]
	void read_recover_tree(PC_tree_t spec_tree)
	{
		PC_tree_t recover_tree = PC_get(spec_tree, ".can_recover_all");
		if (!PC_status(recover_tree)) {
			m_can_recover_data = PDI::to_string(recover_tree);
		}
	}

	//! [read_recover_tree]

	//! [read_data_tree]
	void read_data_tree(PC_tree_t spec_tree)
	{
		PC_tree_t data_tree = PC_get(spec_tree, ".data");
		if (!PC_status(data_tree)) {
			int data_tree_size = PDI::len(data_tree);
			for (int i = 0; i < data_tree_size; i++) {
				PC_tree_t key = PC_get(data_tree, "{%d}", i);
				PC_tree_t value = PC_get(data_tree, "<%d>", i);
				m_data_to_path_map.emplace(PDI::to_string(key), PDI::to_string(value));
			}
		}
	}

	//! [read_data_tree]

	//! [write_data]
	void write_data(const std::string& data_name, PDI::Ref_r ref_r)
	{
		if (!ref_r) {
			return;
		}
		std::string tmp_path = m_data_to_path_map[data_name] + ".tmp";
		std::ofstream file{tmp_path, std::ios::binary};
		if (ref_r.type()->buffersize() == ref_r.type()->datasize()) {
			// dense data
			file.write(static_cast<const char*>(ref_r.get()), ref_r.type()->buffersize());
		} else {
			// sparse data
			std::unique_ptr<char[]> data_copy{new char[ref_r.type()->datasize()]};
			ref_r.type()->data_to_dense_copy(data_copy.get(), ref_r.get());
			file.write(data_copy.get(), ref_r.type()->datasize());
		}
		file.close();

		// replace the old file
		struct stat status;
		if (!stat(tmp_path.c_str(), &status) && status.st_size == static_cast<off_t>(ref_r.type()->datasize())) {
			if (!stat(m_data_to_path_map[data_name].c_str(), &status) && std::remove(m_data_to_path_map[data_name].c_str())) {
				throw PDI::System_error{"Cannot remove old file {}", m_data_to_path_map[data_name]};
			}
			if (std::rename(tmp_path.c_str(), m_data_to_path_map[data_name].c_str())) {
				throw PDI::System_error{"Cannot rename temporary file {}", tmp_path};
			}
		} else {
			throw PDI::System_error{"Data write not complete"};
		}
	}

	//! [write_data]

	//! [read_data]
	void read_data(const std::string& data_name, PDI::Ref_w ref_w)
	{
		if (!ref_w) {
			return;
		}
		std::ifstream file{m_data_to_path_map[data_name], std::ios::binary};
		if (ref_w.type()->buffersize() == ref_w.type()->datasize()) {
			// dense data
			file.read(static_cast<char*>(ref_w.get()), ref_w.type()->buffersize());
		} else {
			// sparse data
			std::unique_ptr<char[]> data_copy{new char[ref_w.type()->datasize()]};
			file.read(data_copy.get(), ref_w.type()->datasize());
			ref_w.type()->data_from_dense_copy(ref_w.get(), data_copy.get());
		}
	}

	//! [read_data]

	//! [can_recover]
	void can_recover(const std::string& data_name, PDI::Ref_w ref_w)
	{
		if (!ref_w) {
			throw PDI::Permission_error{"Cannot write to `can_recover_all' data"};
		}
		for (const auto& data_path_pair: m_data_to_path_map) {
			struct stat status;
			if (stat(data_path_pair.second.c_str(), &status)) {
				*static_cast<int*>(ref_w.get()) = 0;
				return;
			}
		}
		*static_cast<int*>(ref_w.get()) = 1;
	}

	//! [can_recover]

public:
	//! [constructor]
	posix_plugin(PDI::Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		read_recover_tree(spec_tree);
		read_data_tree(spec_tree);
		for (const auto& data_path_pair: m_data_to_path_map) {
			ctx.on_data([this](const std::string& data_name, PDI::Ref ref) { this->write_data(data_name, ref); }, data_path_pair.first);
			ctx.on_data([this](const std::string& data_name, PDI::Ref ref) { this->read_data(data_name, ref); }, data_path_pair.first);
		}
		if (!m_can_recover_data.empty()) {
			ctx.on_data([this](const std::string& data_name, PDI::Ref ref) { this->can_recover(data_name, ref); }, m_can_recover_data);
		}
	}

	//! [constructor]
};

} // namespace

PDI_PLUGIN(posix)
