/*******************************************************************************
 * Copyright (C) 2008-2016 Forschungszentrum Juelich, Juelich Supercomputing Centre
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

#include <mpi.h>

#include <cinttypes>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <spdlog/spdlog.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/data_descriptor.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/expression.h>

#include <sion.h>


namespace {

using PDI::Context;
using PDI::Data_descriptor;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Error;
using PDI::len;
using PDI::Plugin;
using PDI::to_string;
using PDI::Expression;
using std::move;
using std::string;
using std::unordered_map;
using std::vector;

struct Variable_event {

	Expression file;
	
	Expression select;
	
	Expression n_files;
	
	
	Variable_event(PC_tree_t entry, const string& name);
};

struct Named_event {

	Expression file;
	
	Expression select;
	
	Expression n_files;
	
	vector<string> vars;
	
	Named_event(PC_tree_t entry, const string& name);
};


Expression parse_property(PC_tree_t conf, const char* entry_name, const char* property_name, const char* default_value)
{
	try {
		if ( default_value ) {
			return to_string(PC_get(conf, property_name), default_value);
		} else {
			return to_string(PC_get(conf, property_name));
		}
	} catch (...) {
		throw Error {PDI_ERR_CONFIG, "Property '%s' not found for entry '%s'.\n", property_name, entry_name};
	}
}

Variable_event::Variable_event(PC_tree_t entry, const string& name):
	file {parse_property(entry, name.c_str(), ".file", NULL)},
	select {parse_property(entry, name.c_str(), ".select", "1")},
	n_files {parse_property(entry, name.c_str(), ".n_files", "1")}
{
}

unordered_map<string, Variable_event> parse_vars(PC_tree_t conf)
{
	if (PC_status(conf)) return {};
	
	unordered_map<string, Variable_event> result;
	int nentry = len(conf);
	for (int i = 0; i < nentry; ++i) {
		PC_tree_t entry = PC_get(conf, "[%d]", i);
		// only for variables
		if (!PC_status(PC_get(entry, ".variable"))) {
			string name = to_string(PC_get(entry, ".variable"));
			result.emplace(name, Variable_event {entry, name});
		}
	}
	
	return result;
}

Named_event::Named_event(PC_tree_t entry, const string& name):
	file{parse_property(entry, name.c_str(), ".file", NULL)},
	select{parse_property(entry, name.c_str(), ".select", "1")},
	n_files{parse_property(entry, name.c_str(), ".n_files", "1")}
{
	PC_tree_t entry_vars = PC_get(entry, ".vars");
	int nvar = len(entry_vars);
	if (nvar <= 0) throw Error{PDI_ERR_CONFIG, "No variables specified for event '%s'.\n", name.c_str()};
	
	for (int i = 0; i < nvar; ++i) {
		vars.emplace_back(to_string(PC_get(entry_vars, "[%d]", i)));
	}
}

unordered_map<string, Named_event> parse_events(PC_tree_t conf)
{
	if (PC_status(conf)) return {};    // if no subtree found
	
	unordered_map<string, Named_event> result;
	int nevent = len(conf);
	for (int i = 0; i < nevent; ++i) {
		PC_tree_t entry = PC_get(conf, "[%d]", i);
		// only in case entry for an event
		if (!PC_status(PC_get(entry, ".event"))) {
			string name = to_string(PC_get(entry, ".event"));
			result.emplace(name, Named_event {entry, name});
		}
	}
	
	return result;
}

// This is FNV-1a
static uint64_t hash(const uint8_t* data, size_t len)
{
	uint64_t hash = UINT64_C(0xcbf29ce484222325);
	for (size_t i = 0; i < len; ++i) {
		hash ^= static_cast<uint64_t>(data[i]);
		hash *= UINT64_C(0x100000001b3);
	}
	return hash;
}

struct decl_sion_plugin: Plugin {

	string comm_name;
	
	MPI_Comm comm;
	
	unordered_map<string, Variable_event> output_vars;
	
	unordered_map<string, Variable_event> input_vars;
	
	unordered_map<string, Named_event> output_events;
	
	unordered_map<string, Named_event> input_events;
	
	
	decl_sion_plugin(Context& ctx, PC_tree_t conf):
		Plugin{ctx},
		comm_name{to_string(PC_get(conf, ".communicator"))},
		comm{MPI_COMM_NULL},
		output_vars{parse_vars(PC_get(conf, ".outputs"))},
		input_vars{parse_vars(PC_get(conf, ".inputs"))},
		output_events{parse_events(PC_get(conf, ".outputs"))},
		input_events{parse_events(PC_get(conf, ".inputs"))}
	{
		if (PC_status(conf)) throw Error {PDI_ERR_CONFIG, "Configuration is invalid"};
		Data_descriptor& comm_desc = ctx.desc(comm_name);
		if ( !comm_desc.empty() ) {
			if (MPI_Comm_dup(*(static_cast<const MPI_Comm*>(Ref_r{comm_desc.ref()}.get())), &comm)) {
				throw Error {PDI_ERR_SYSTEM, "Cannot duplicate MPI communicator"};
			}
		}
		ctx.add_data_callback([this](const std::string& name, Ref ref) {
			this->data(name, ref);
		});
		
		ctx.add_event_callback([this](const std::string& name) {
			this->event(name);
		});
		set_up_logger(PC_get(conf, ".logging"));
		ctx.logger()->info("Plugin loaded successfully");
	}
	
	void set_up_logger(PC_tree_t logging_tree)
	{
		context().logger()->set_pattern("[PDI][Decl'SION][%T] *** %^%l%$: %v");
		
		int mpi_init = 0;
		MPI_Initialized(&mpi_init);
		if (mpi_init) {
			//set up format
			int world_rank;
			MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
			char format[64];
			snprintf(format, 64, "[PDI][Decl'SION][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
			context().logger()->set_pattern(string(format));
		}
	}
	
	void write_event(const Named_event& event)
	{
		// check that data is available and data type is dense
		for (auto&& var : event.vars) {
			if (Ref_r ref = context().desc(var).ref()) {
				if (!ref.type().dense()) {
					throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", var.c_str()};
				}
			} else {
				throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
			}
		}
		
		int n_files = static_cast<int>(event.n_files.to_long(context()));
		
		sion_int64 chunksize = 0;
		for (auto&& var : event.vars) {
			const Ref& ref = context().desc(var).ref();
			if (!ref) {
				throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
			}
			chunksize += ref.type().datasize();
		}
		
		sion_int32 blksize = -1;
		int rank; MPI_Comm_rank(comm, &rank);
		
		int sid = sion_paropen_mpi(event.file.to_string(context()).c_str(), "w,keyval=inline", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
		
		for (auto&& var : event.vars) {
			Ref_r ref = context().desc(var).ref();
			if (!ref) {
				throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
			}
			
			uint64_t key = hash(reinterpret_cast<const uint8_t*>(var.c_str()), var.size());
			
			uint64_t name_size = var.size();
			if (SION_SUCCESS != sion_fwrite_key(&name_size, key, sizeof(name_size), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while writing name size in SION file"};
			}
			
			if (SION_SUCCESS != sion_fwrite_key(var.c_str(), key, var.size(), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while writing name in SION file"};
			}
			
			uint64_t data_size = ref.type().datasize();
			if (SION_SUCCESS != sion_fwrite_key(&data_size, key, sizeof(data_size), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while writing data size in SION file"};
			}
			
			if (SION_SUCCESS != sion_fwrite_key(ref.get(), key, data_size, 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while writing data in SION file"};
			}
		}
		
		if (SION_SUCCESS != sion_parclose_mpi(sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
		}
	}
	
	void read_event(const Named_event& event)
	{
		// check that data type is dense
		for (auto&& var : event.vars) {
			Ref cref = context().desc(var).ref();
			if (Ref_w ref = cref) {
				if (!ref.type().dense()) {
					throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", var.c_str()};
				}
			} else {
				throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
			}
		}
		
		int n_files = 1;
		sion_int64 chunksize = 0;
		sion_int32 blksize = -1;
		int rank; MPI_Comm_rank(comm, &rank);
		
		string file = event.file.to_string(context());
		int sid = sion_paropen_mpi(event.file.to_string(context()).c_str(), "r,keyval=unknown", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
		
		for (auto&& var : event.vars) {
			Ref_w ref = context().desc(var).ref();
			if (!ref) {
				throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
			}
			
			uint64_t key = hash(reinterpret_cast<const uint8_t*>(var.c_str()), var.size());
			
			for (int j = 0; ; ++j) {
				if (SION_SUCCESS != sion_seek_key(sid, key, 4 * j, 0)) {
					sion_parclose_mpi(sid);
					throw Error {PDI_ERR_SYSTEM, "Could not find variable '%s' for reading in file '%s' on try #%d", var.c_str(), file.c_str(), j + 1};
				}
				
				uint64_t name_size;
				if (SION_SUCCESS != sion_fread_key(&name_size, key, sizeof(uint64_t), 1, sid)) {
					sion_parclose_mpi(sid);
					throw Error {PDI_ERR_SYSTEM, "Error while reading name size in SION file"};
				}
				// Collision (size of name does not match), this is not the data you are looking for.
				if (var.size() != name_size) continue;
				
				string name(var.size(), '*');
				if (SION_SUCCESS != sion_fread_key(&name[0], key, var.size(), 1, sid)) {
					sion_parclose_mpi(sid);
					throw Error {PDI_ERR_SYSTEM, "Error while reading name in SION file"};
				}
				// Collision (names do not match), this is not the data you are looking for.
				if (var != name) continue;
				
				// data found, leaving the for
				break;
			}
			
			size_t data_size = ref.type().datasize();
			uint64_t data_size_from_file;
			if (SION_SUCCESS != sion_fread_key(&data_size_from_file, key, sizeof(data_size_from_file), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while reading data size in SION file"};
			}
			
			if (data_size != data_size_from_file) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Size of data for variable '%s' in file '%s' does not match memory size (%" PRIu64 " (file) vs. %zu (memory)).\n", var.c_str(), file.c_str(), data_size_from_file, data_size};
			}
			
			if (SION_SUCCESS != sion_fread_key(ref.get(), key, data_size, 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while reading data in SION file"};
			}
		}
		
		if (SION_SUCCESS != sion_parclose_mpi(sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
		}
	}
	
	void write_var(Ref cref, const char* name, const Variable_event& var)
	{
		Ref_r ref = cref;
		if (!ref) {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", name};
		}
		
		// check that data type is dense
		if (!ref.type().dense()) {
			throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", name};
		}
		
		// open file
		int n_files = static_cast<int>(var.n_files.to_long(context()));
		size_t data_size = ref.type().datasize();
		sion_int64 chunksize = static_cast<sion_int64>(data_size);
		sion_int32 blksize = -1;
		int rank; MPI_Comm_rank(comm, &rank);
		int sid = sion_paropen_mpi(var.file.to_string(context()).c_str(), "w", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
		
		// write data to file
		if (SION_SUCCESS != sion_fwrite(ref.get(), data_size, 1, sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while writing data to SION file"};
		}
		
		// close file
		if (SION_SUCCESS != sion_parclose_mpi(sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while writing data to SION file"};
		}
	}
	
	void read_var(Ref cref, const char* name, const Variable_event& var)
	{
		Ref_w ref = cref;
		if (!ref) {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", name};
		}
		
		// check that data type is dense
		if (!ref.type().dense()) {
			throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported"};
		}
		
		// open file
		int n_files = 1;
		size_t data_size = ref.type().datasize();
		sion_int64 chunksize = 0;
		sion_int32 blksize = -1;
		int rank; MPI_Comm_rank(comm, &rank);
		int sid = sion_paropen_mpi(var.file.to_string(context()).c_str(), "r", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
		
		// read data from file
		if (SION_SUCCESS != sion_fread(ref.get(), data_size, 1, sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while reading data from SION file"};
		}
		
		// close file
		if (SION_SUCCESS != sion_parclose_mpi(sid)) {
			throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
		}
	}
	
	void event(const std::string& event)
	{
		auto&& outevit = output_events.find(event);
		if (outevit != output_events.end()) {
			long select;
			try {
				select = outevit->second.select.to_long(context());
			} catch (Error&) {
				select = 0;
			}
			if (select) write_event(outevit->second);
		}
		
		auto&& inevit = input_events.find(event);
		if (inevit != input_events.end()) {
			long select;
			try {
				select = inevit->second.select.to_long(context());
			} catch (Error&) {
				select = 0;
			}
			if (select) read_event(inevit->second);
		}
	}
	
	void data(const std::string& name, Ref cref)
	{
		if ( name == comm_name ) {
			if (MPI_Comm_dup(*(static_cast<const MPI_Comm*>(Ref_r{context().desc(name).ref()}.get())), &comm)) {
				throw Error {PDI_ERR_SYSTEM, "Cannot duplicate MPI communicator"};
			}
		}
		auto&& outvarit = output_vars.find(name);
		if (outvarit != output_vars.end()) {
			long select;
			try {
				select = outvarit->second.select.to_long(context());
			} catch (Error&) {
				select = 0;
			}
			if (select) write_var(cref, name.c_str(), outvarit->second);
		}
		
		auto&& invarit = input_vars.find(name);
		if (invarit != input_vars.end()) {
			long select;
			try {
				select = invarit->second.select.to_long(context());
			} catch (Error&) {
				select = 0;
			}
			if (select) read_var(cref, name.c_str(), invarit->second);
		}
	}
	
	~decl_sion_plugin()
	{
		context().logger()->info("Closing plugin");
	}
	
}; // struct decl_sion_plugin

} // namespace <anonymous>

PDI_PLUGIN(decl_sion)
