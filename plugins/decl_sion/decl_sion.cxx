/*******************************************************************************
 * Copyright (c) 2017, Benedikt Steinbusch - FZJ (b.steinbusch@fz-juelich.de)
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
#include <unordered_map>
#include <vector>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/data_type.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/value.h>

#include <sion.h>

namespace
{
	
using PDI::Context;
using PDI::Data_ref;
using PDI::Data_r_ref;
using PDI::Data_w_ref;
using PDI::Error;
using PDI::len;
using PDI::to_string;
using PDI::Value;
using std::move;
using std::string;
using std::unordered_map;
using std::vector;

struct Variable_event {
	Value file;
	Value select;
	Value n_files;

	Variable_event(PC_tree_t entry, const string &name);
};

struct Named_event {
	Value file;
	Value select;
	Value n_files;
	vector<string> vars;

	Named_event(PC_tree_t entry, const string &name);
};


MPI_Comm comm;
unordered_map<string, Variable_event> output_vars;
unordered_map<string, Variable_event> input_vars;
unordered_map<string, Named_event> output_events;
unordered_map<string, Named_event> input_events;


Value parse_property(PC_tree_t conf, const char *entry_name, const char *property_name, const char *default_value)
{
	try {
		return to_string(PC_get(conf, property_name));
	}
	catch(const Error &) {
		if(default_value) {
			return default_value;
		}
		else {
			throw Error {PDI_ERR_CONFIG, "Property '%s' not found for entry '%s'.\n", property_name, entry_name};
		}
	}
}

Variable_event::Variable_event(PC_tree_t entry, const string &name):
	file {parse_property(entry, name.c_str(), ".file", NULL)},
		 select {parse_property(entry, name.c_str(), ".select", "1")},
n_files {parse_property(entry, name.c_str(), ".n_files", "1")} {
}

unordered_map<string, Variable_event> parse_vars(PC_tree_t conf)
{
	if(PC_status(conf)) return {};

	unordered_map<string, Variable_event> result;
	int nentry = len(conf);
	for(int i = 0; i < nentry; ++i) {
		PC_tree_t entry = PC_get(conf, "[%d]", i);
		// only for variables
		if(!PC_status(PC_get(entry, ".variable"))) {
			string name = to_string(PC_get(entry, ".variable"));
			result.emplace(name, Variable_event {entry, name});
		}
	}

	return result;
}

Named_event::Named_event(PC_tree_t entry, const string &name):
	file {parse_property(entry, name.c_str(), ".file", NULL)},
		 select {parse_property(entry, name.c_str(), ".select", "1")},
n_files {parse_property(entry, name.c_str(), ".n_files", "1")} {
	PC_tree_t entry_vars = PC_get(entry, ".vars");
	int nvar = len(entry_vars);
	if(nvar <= 0) throw Error{PDI_ERR_CONFIG, "No variables specified for event '%s'.\n", name.c_str()};

	for(int i = 0; i < nvar; ++i)
	{
		vars.emplace_back(to_string(PC_get(entry_vars, "[%d]", i)));
	}
}

unordered_map<string, Named_event> parse_events(PC_tree_t conf)
{
	if(PC_status(conf)) return {};     // if no subtree found

	unordered_map<string, Named_event> result;
	int nevent = len(conf);
	for(int i = 0; i < nevent; ++i) {
		PC_tree_t entry = PC_get(conf, "[%d]", i);
		// only in case entry for an event
		if(!PC_status(PC_get(entry, ".event"))) {
			string name = to_string(PC_get(entry, ".event"));
			result.emplace(name, Named_event {entry, name});
		}
	}

	return result;
}

void PDI_decl_sion_init(Context& ctx, PC_tree_t conf, MPI_Comm *world)
{
	if(PC_status(conf)) throw Error {PDI_ERR_CONFIG, "Configuration is invalid"};
	if(MPI_Comm_dup(*world, &comm)) {
		throw Error {PDI_ERR_SYSTEM, "Cannot duplicate MPI communicator"};
	}

	output_vars = parse_vars(PC_get(conf, ".outputs"));
	output_events = parse_events(PC_get(conf, ".outputs"));
	input_vars = parse_vars(PC_get(conf, ".inputs"));
	input_events = parse_events(PC_get(conf, ".inputs"));
}

void PDI_decl_sion_finalize(Context& ctx)
{
	output_vars.clear();
	input_vars.clear();
	output_events.clear();
	input_events.clear();
}

// This is FNV-1a
static uint64_t hash(const uint8_t *data, size_t len)
{
	uint64_t hash = UINT64_C(0xcbf29ce484222325);
	for(size_t i = 0; i < len; ++i) {
		hash ^= (uint64_t) data[i];
		hash *= UINT64_C(0x100000001b3);
	}
	return hash;
}

void write_event(Context& ctx, const Named_event &event)
{
	// check that data is available and data type is dense
	for(auto && var : event.vars) {
		if(Data_r_ref ref = ctx.desc(var).ref()) {
			if(!ref.type().dense()) {
				throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", var.c_str()};
			}
		}
		else {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
		}
	}

	int n_files = event.n_files.to_long(ctx);

	sion_int64 chunksize = 0;
	for(auto && var : event.vars) {
		const Data_ref &ref = ctx.desc(var).ref();
		if(!ref) {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
		}
		chunksize += ref.type().datasize();
	}

	sion_int32 blksize = -1;
	int rank; MPI_Comm_rank(comm, &rank);

	int sid = sion_paropen_mpi(event.file.to_string(ctx).c_str(), "w,keyval=inline", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

	for(auto && var : event.vars) {
		Data_r_ref ref = ctx.desc(var).ref();
		if(!ref) {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
		}

		uint64_t key = hash(reinterpret_cast<const uint8_t *>(var.c_str()), var.size());

		uint64_t name_size = var.size();
		if(SION_SUCCESS != sion_fwrite_key(&name_size, key, sizeof(name_size), 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while writing name size in SION file"};
		}

		if(SION_SUCCESS != sion_fwrite_key(var.c_str(), key, var.size(), 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while writing name in SION file"};
		}

		uint64_t data_size = ref.type().datasize();
		if(SION_SUCCESS != sion_fwrite_key(&data_size, key, sizeof(data_size), 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while writing data size in SION file"};
		}

		if(SION_SUCCESS != sion_fwrite_key(ref.get(), key, data_size, 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while writing data in SION file"};
		}
	}

	if(SION_SUCCESS != sion_parclose_mpi(sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
	}
}

void read_event(Context& ctx, const Named_event &event)
{
	// check that data type is dense
	for(auto && var : event.vars) {
		Data_ref cref = ctx.desc(var).ref();
		if(Data_w_ref ref = cref) {
			if(!ref.type().dense()) {
				throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", var.c_str()};
			}
		}
		else {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
		}
	}

	int n_files = 1;
	sion_int64 chunksize = 0;
	sion_int32 blksize = -1;
	int rank; MPI_Comm_rank(comm, &rank);

	string file = event.file.to_string(ctx);
	int sid = sion_paropen_mpi(event.file.to_string(ctx).c_str(), "r,keyval=unknown", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

	for(auto && var : event.vars) {
		Data_w_ref ref = ctx.desc(var).ref();
		if(!ref) {
			throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", var.c_str()};
		}

		uint64_t key = hash(reinterpret_cast<const uint8_t *>(var.c_str()), var.size());

		for(int j = 0; ; ++j) {
			if(SION_SUCCESS != sion_seek_key(sid, key, 4 * j, 0)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Could not find variable '%s' for reading in file '%s' on try #%d", var.c_str(), file.c_str(), j + 1};
			}

			uint64_t name_size;
			if(SION_SUCCESS != sion_fread_key(&name_size, key, sizeof(uint64_t), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while reading name size in SION file"};
			}
			// Collision (size of name does not match), this is not the data you are looking for.
			if(var.size() != name_size) continue;

			string name(var.size(), '*');
			if(SION_SUCCESS != sion_fread_key(&name[0], key, var.size(), 1, sid)) {
				sion_parclose_mpi(sid);
				throw Error {PDI_ERR_SYSTEM, "Error while reading name in SION file"};
			}
			// Collision (names do not match), this is not the data you are looking for.
			if(var != name) continue;

			// data found, leaving the for
			break;
		}

		size_t data_size = ref.type().datasize();
		uint64_t data_size_from_file;
		if(SION_SUCCESS != sion_fread_key(&data_size_from_file, key, sizeof(data_size_from_file), 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while reading data size in SION file"};
		}

		if(data_size != data_size_from_file) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Size of data for variable '%s' in file '%s' does not match memory size (%" PRIu64 " (file) vs. %zu (memory)).\n", var.c_str(), file.c_str(), data_size_from_file, data_size};
		}

		if(SION_SUCCESS != sion_fread_key(ref.get(), key, data_size, 1, sid)) {
			sion_parclose_mpi(sid);
			throw Error {PDI_ERR_SYSTEM, "Error while reading data in SION file"};
		}
	}

	if(SION_SUCCESS != sion_parclose_mpi(sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
	}
}

void PDI_decl_sion_event(Context& ctx, const char *event)
{
	auto &&outevit = output_events.find(event);
	if(outevit != output_events.end()) {
		int select;
		try {
			select = outevit->second.select.to_long(ctx);
		}
		catch(Error &) {
			select = 0;
		}
		if(select) write_event(ctx, outevit->second);
	}

	auto &&inevit = input_events.find(event);
	if(inevit != input_events.end()) {
		int select;
		try {
			select = inevit->second.select.to_long(ctx);
		}
		catch(Error &) {
			select = 0;
		}
		if(select) read_event(ctx, inevit->second);
	}
}

void write_var(Context& ctx, Data_ref cref, const string &name, const Variable_event &var)
{
	Data_r_ref ref = cref;
	if(!ref) {
		throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", name.c_str()};
	}

	// check that data type is dense
	if(!ref.type().dense()) {
		throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", name.c_str()};
	}

	// open file
	int n_files = var.n_files.to_long(ctx);
	size_t data_size = ref.type().datasize();
	sion_int64 chunksize = data_size;
	sion_int32 blksize = -1;
	int rank; MPI_Comm_rank(comm, &rank);
	int sid = sion_paropen_mpi(var.file.to_string(ctx).c_str(), "w", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

	// write data to file
	if(SION_SUCCESS != sion_fwrite(ref.get(), data_size, 1, sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while writing data to SION file"};
	}

	// close file
	if(SION_SUCCESS != sion_parclose_mpi(sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while writing data to SION file"};
	}
}

void read_var(Context& ctx, Data_ref cref, const string &name, const Variable_event &var)
{
	Data_w_ref ref = cref;
	if(!ref) {
		throw Error {PDI_ERR_RIGHT, "Dataset unavailable '%s'", name.c_str()};
	}

	// check that data type is dense
	if(!ref.type().dense()) {
		throw Error {PDI_ERR_IMPL, "Sparse data type of variable '%s' is not supported", name.c_str()};
	}

	// open file
	int n_files = 1;
	size_t data_size = ref.type().datasize();
	sion_int64 chunksize = 0;
	sion_int32 blksize = -1;
	int rank; MPI_Comm_rank(comm, &rank);
	int sid = sion_paropen_mpi(var.file.to_string(ctx).c_str(), "r", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

	// read data from file
	if(SION_SUCCESS != sion_fread(ref.get(), data_size, 1, sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while reading data from SION file"};
	}

	// close file
	if(SION_SUCCESS != sion_parclose_mpi(sid)) {
		throw Error {PDI_ERR_SYSTEM, "Error while closing SION file"};
	}
}

void PDI_decl_sion_data(Context& ctx, const char* name, Data_ref cref)
{
	auto &&outvarit = output_vars.find(name);
	if(outvarit != output_vars.end()) {
		int select;
		try {
			select = outvarit->second.select.to_long(ctx);
		}
		catch(Error &) {
			select = 0;
		}
		if(select) write_var(ctx, cref, name, outvarit->second);
	}

	auto &&invarit = input_vars.find(name);
	if(invarit != input_vars.end()) {
		int select;
		try {
			select = invarit->second.select.to_long(ctx);
		}
		catch(Error &) {
			select = 0;
		}
		if(select) read_var(ctx, cref, name, invarit->second);
	}
}

} // namespace <anonymous>

PDI_PLUGIN(decl_sion)
