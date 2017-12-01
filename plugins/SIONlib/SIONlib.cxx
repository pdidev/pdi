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

#include "config.h"

#ifdef STRDUP_WORKS
#define _POSIX_C_SOURCE 200809L
#endif

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mpi.h>

#include <pdi.h>
#include <pdi/datatype.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/value.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>

#include <paraconf.h>

#include <sion.h>

using PDI::Data_ref;
using PDI::Data_r_ref;
using PDI::Data_w_ref;
using PDI::Value;
using std::move;
using std::string;

typedef struct
{
  char *name;
  PDI_value_t file;
  PDI_value_t select;
  PDI_value_t n_files;
} SIONlib_var_t;

typedef struct
{
  char *name;
  PDI_value_t file;
  PDI_value_t select;
  PDI_value_t n_files;
  size_t n_vars;
  char **vars;
} SIONlib_event_t;


static MPI_Comm comm;

static size_t n_output_vars = 0;
static SIONlib_var_t *output_vars = NULL;

static size_t n_input_vars = 0;
static SIONlib_var_t *input_vars = NULL;

static size_t n_output_events = 0;
static SIONlib_event_t *output_events = NULL;

static size_t n_input_events = 0;
static SIONlib_event_t *input_events = NULL;


#ifndef STRDUP_WORKS
static char *strdup(const char *s)
{
  char *p = (char*) malloc(strlen(s)+1);
  if ( p ) strcpy(p, s);
  return p;
}
#endif

static PDI_status_t parse_property(PC_tree_t conf, const char *entry_name, const char *property_name, const char *default_value, PDI_value_t *property)
{
  char *strv = NULL;
  if (PC_string(PC_get(conf, property_name), &strv)) {
    free(strv);
    if (default_value) {
      strv = strdup(default_value);
      if (!strv) return PDI_ERR_SYSTEM;
    } else {
      fprintf(stderr, "[PDI/SIONlib] Property '%s' not found for entry '%s'.\n", property_name, entry_name);
      return PDI_ERR_CONFIG;
    }
  }
  *property = Value{strv};
  free(strv);
  return PDI_OK;
}

static PDI_status_t parse_var(PC_tree_t entry, SIONlib_var_t *var)
{
  char *name = NULL;
  if (PC_string(PC_get(entry, ".variable"), &name)) {
    free(name);
    return PDI_ERR_SYSTEM;
  }

  PDI_status_t status;
  // set file
  PDI_value_t file;
  if ((status = parse_property(entry, name, ".file", NULL, &file))) {
    // property 'file' is mandatory
    free(name);
    return status;
  }

  // set select
  PDI_value_t select;
  if ((status = parse_property(entry, name, ".select", "1", &select))) {
    // parse with default value should not fail
    free(name);
    return status;
  }

  // set n_files
  PDI_value_t n_files;
  if ((status = parse_property(entry, name, ".n_files", "1", &n_files))) {
    // parse with default value should not fail
    free(name);
    return status;
  }

  *var = (SIONlib_var_t) {
    .name = name,
    .file = move(file),
    .select = move(select),
    .n_files = move(n_files)
  };

  return PDI_OK;
}

static PDI_status_t parse_vars(PC_tree_t conf, SIONlib_var_t *vars[], size_t *n_vars)
{
  int len;
  if ( PC_len(conf, &len) ) { // if no subtree found
          *n_vars = 0;
          *vars = NULL;
          return PDI_OK;
  }
  // conf tree contains both vars and events, so len >= *n_vars
  *vars = (SIONlib_var_t*) calloc(sizeof(SIONlib_var_t), len);
  if (!vars) return PDI_ERR_SYSTEM;

  *n_vars = 0;
  for (int i = 0; i < len; ++i) {
    PC_tree_t entry = PC_get(conf, "[%zd]", i);
    if (PC_status(PC_get(entry, ".variable"))) {
      // not an entry for a variable
      continue;
    }

    PDI_status_t status = parse_var(entry, &(*vars)[*n_vars]);
    if (status) return status;

    *n_vars += 1;
  }

  return PDI_OK;
}

static PDI_status_t parse_event_vars(PC_tree_t conf, const char *event_name, size_t *n_vars, char ***vars)
{
  int len;
  PC_status_t status = PC_len(conf, &len);
  if (status || len <= 0) {
    fprintf(stderr, "[PDI/SIONlib] No variables specified for event '%s'.\n", event_name);
    return PDI_ERR_CONFIG;
  }

  *vars = (char**) calloc(sizeof(char *), len);
  if (!*vars) return PDI_ERR_SYSTEM;

  for (int i = 0; i < len; ++i) {
    if (PC_string(PC_get(conf, "[%zd]", i), *vars + i)) {
      free(*vars);
      return PDI_ERR_SYSTEM;
    }
  }

  *n_vars = (size_t) len;
  return PDI_OK;
}

static PDI_status_t parse_event(PC_tree_t entry, SIONlib_event_t *event)
{
  char *name = NULL;
  if (PC_string(PC_get(entry, ".event"), &name)) {
    free(name);
    return PDI_ERR_SYSTEM;
  }

  PDI_status_t status;
  // set path
  PDI_value_t file;
  if ((status = parse_property(entry, name, ".file", NULL, &file))) {
    // property 'file' is mandatory
    free(name);
    return status;
  }

  // set select
  PDI_value_t select;
  if ((status = parse_property(entry, name, ".select", "1", &select))) {
    // parse with default value should not fail
    free(name);
    return status;
  }

  // set n_files
  PDI_value_t n_files;
  if ((status = parse_property(entry, name, ".n_files", "1", &n_files))) {
    // parse with default value should not fail
    free(name);
    return status;
  }

  // set variables
  PC_tree_t vars_conf = PC_get(entry, ".vars");
  size_t n_vars;
  char **vars;
  if ((status = parse_event_vars(vars_conf, name, &n_vars, &vars))) {
    free(name);
    return status;
  }

  *event = (SIONlib_event_t) {
    .name = name,
    .file = move(file),
    .select = move(select),
    .n_files = move(n_files),
    .n_vars = n_vars,
    .vars = vars
  };

  return PDI_OK;
}

static PDI_status_t parse_events(PC_tree_t conf, SIONlib_event_t *events[], size_t *n_events)
{
  int len;
  if (PC_len(conf, &len)) { // if no subtree found
    *n_events = 0;
    *events = NULL;
    return PDI_OK;
  }
  *events = (SIONlib_event_t*) calloc(sizeof(SIONlib_event_t), len);
  if (!events) return PDI_ERR_SYSTEM;

  *n_events = 0;
  for (int i = 0; i < len; ++i) {
    PC_tree_t entry = PC_get(conf, "[%zd]", i);
    if (PC_status(PC_get(entry, ".event"))) {
      // not an entry for an event
      continue;
    }

    PDI_status_t status = parse_event(entry, &(*events)[*n_events]);
    if (status) return status;

    *n_events += 1;
  }

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_init(PC_tree_t conf, MPI_Comm* world)
{
  if (MPI_Comm_dup(*world, &comm)) {
    fprintf(stderr, "[PDI/SIONlib] Cannot duplicate MPI communicator.\n");
    return PDI_ERR_SYSTEM;
  }

  if (PC_status(conf)) {
    fprintf(stderr, "[PDI/SIONlib] Configuration is invalid.\n");
    return PDI_ERR_CONFIG;
  }

  PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);

  PC_tree_t outputs_cfg; outputs_cfg = PC_get(conf, ".outputs");
  PDI_status_t status = parse_vars(outputs_cfg, &output_vars, &n_output_vars);
  if (status) goto err0;

  PC_tree_t inputs_cfg; inputs_cfg = PC_get(conf, ".inputs");
  status = parse_vars(inputs_cfg, &input_vars, &n_input_vars);
  if (status) goto err0;

  status = parse_events(outputs_cfg, &output_events, &n_output_events);
  if (status) goto err0;

  status = parse_events(inputs_cfg, &input_events, &n_input_events);

err0:
  PC_errhandler(errh);
  return status;
}

static void destroy_var(SIONlib_var_t *var) {
  free(var->name);
  var->file.~Value();
  var->select.~Value();
  var->n_files.~Value();
}

static void destroy_vars(size_t *n_vars, SIONlib_var_t *vars) {
  for (size_t i = 0; i < *n_vars; ++i) {
    destroy_var(&vars[i]);
  }
  free(vars);
  *n_vars = 0;
}

static void destroy_event(SIONlib_event_t *event) {
    free(event->name);
    event->file.~Value();
    event->select.~Value();
    event->n_files.~Value();
    for (size_t j = 0; j < event->n_vars; ++j) {
      free(event->vars[j]);
    }
    event->n_vars = 0;
    free(event->vars);
}

static void destroy_events(size_t *n_events, SIONlib_event_t *events)
{
  for (size_t i = 0; i < *n_events; ++i) {
    destroy_event(&events[i]);
  }
  free(events);
  *n_events = 0;
}

PDI_status_t PDI_SIONlib_finalize()
{
  destroy_vars(&n_output_vars, output_vars);
  destroy_vars(&n_input_vars, input_vars);

  destroy_events(&n_output_events, output_events);
  destroy_events(&n_input_events, input_events);

  return PDI_OK;
}

// This is FNV-1a
static uint64_t hash(const uint8_t *data, size_t len)
{
  uint64_t hash = UINT64_C(0xcbf29ce484222325);
  for (size_t i = 0; i < len; ++i) {
    hash ^= (uint64_t) data[i];
    hash *= UINT64_C(0x100000001b3);
  }
  return hash;
}

static PDI_status_t write_event(const SIONlib_event_t *event)
{
  PDI_status_t status;
  // check that data is available and data type is dense
  for (size_t i = 0; i < event->n_vars; ++i) {
    Data_ref cref = PDI_state.desc(event->vars[i]).value();
    if ( Data_r_ref ref = cref ) {
      int is_dense=0;
      if ((status = PDI_datatype_is_dense(&ref.type(), &is_dense))) return status;
      if (!is_dense) {
        fprintf(stderr, "[PDI/SIONlib] Sparse data type of variable '%s' is not supported.\n", event->vars[i]);
        return PDI_ERR_IMPL;
      }
    } else {
      fprintf(stderr, "[PDI/SIONlib] Dataset unavailable '%s'.\n", event->vars[i]);
      return PDI_UNAVAILABLE;
    }
  }

  int n_files = event->n_files.to_long();

  sion_int64 chunksize = 0;
  for (size_t i = 0; i < event->n_vars; ++i) {
    const Data_ref & ref = PDI_state.desc(event->vars[i]).value();
    if (!ref) {
      fprintf(stderr, "[PDI/SIONlib] Dataset unavailable '%s'.\n", event->vars[i]);
      return PDI_UNAVAILABLE;
    }
    size_t data_size;
    if ((status = PDI_datatype_datasize(&ref.type(), &data_size))) return status;
    chunksize += data_size;
  }

  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);

  int sid = sion_paropen_mpi(event->file.to_str().c_str(), "w,keyval=inline", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

  for (size_t i = 0; i < event->n_vars; ++i) {
    const Data_ref & ref = PDI_state.desc(event->vars[i]).value();

    size_t data_size;
    if ((status = PDI_datatype_datasize(&ref.type(), &data_size))) {
      sion_parclose_mpi(sid);
      return status;
    }

    size_t name_size = strlen(event->vars[i]);
    uint64_t key = hash((uint8_t*)event->vars[i], name_size);

    uint64_t name_size_to_file = name_size;
    if (1 != sion_fwrite_key(&name_size_to_file, key, sizeof(name_size_to_file), 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    if (1 != sion_fwrite_key(event->vars[i], key, name_size, 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    uint64_t data_size_to_file = data_size;
    if (1 != sion_fwrite_key(&data_size_to_file, key, sizeof(data_size_to_file), 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    if (1 != sion_fwrite_key(ref.get(), key, data_size, 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_event(const SIONlib_event_t *event)
{
  // check that data type is dense
  PDI_status_t status;
  for (size_t i = 0; i < event->n_vars; ++i) {
    Data_ref cref = PDI_state.desc(event->vars[i]).value();
    if ( Data_w_ref ref = cref ) {
      int is_dense;
      if ((status = PDI_datatype_is_dense(&ref.type(), &is_dense))) return status;
      if (!is_dense) {
        fprintf(stderr, "[PDI/SIONlib] Sparse data type of variable '%s' is not supported.\n", event->vars[i]);
        return PDI_ERR_IMPL;
      }
    } else {
      fprintf(stderr, "[PDI/SIONlib] Dataset unavailable '%s'.\n", event->vars[i]);
      return PDI_UNAVAILABLE;
    }
  }

  int n_files = 1;
  sion_int64 chunksize = 0;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);

  string file = event->file.to_str();
  int sid = sion_paropen_mpi(file.c_str(), "r,keyval=unknown", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

  for (size_t i = 0; i < event->n_vars; ++i) {
    if ( Data_w_ref ref = PDI_state.desc(event->vars[i]).value() ) {
      size_t data_size;
      if ((status = PDI_datatype_datasize(&ref.type(), &data_size))) {
        sion_parclose_mpi(sid);
        return status;
      }
  
      size_t name_size = strlen(event->vars[i]);
      uint64_t key = hash((uint8_t*)event->vars[i], name_size);
  
      for (int j = 0; ; ++j) {
        if (SION_SUCCESS != sion_seek_key(sid, key, 4 * j, 0)) {
          fprintf(stderr, "[PDI/SIONlib] Could not find variable '%s' for reading in file '%s'.\n", event->vars[i], file.c_str());
          sion_parclose_mpi(sid);
          return PDI_ERR_SYSTEM;
        }
  
        uint64_t name_size_from_file;
        if (1 != sion_fread_key(&name_size_from_file, key, sizeof(uint64_t), 1, sid)) {
          sion_parclose_mpi(sid);
          return PDI_ERR_SYSTEM;
        }
  
        if (name_size != name_size_from_file)
          // Collision (size of name does not match), this is not the data you are looking for.
          continue;
  
        char *name_from_file = (char*) malloc(name_size + 1);
        if (NULL == name_from_file) {
          sion_parclose_mpi(sid);
          return PDI_ERR_SYSTEM;
        }
  
        if (1 != sion_fread_key(name_from_file, key, name_size, 1, sid)) {
          free(name_from_file);
          sion_parclose_mpi(sid);
          return PDI_ERR_SYSTEM;
        }
        name_from_file[name_size] = '\0';
  
        int names_match = !strcmp(event->vars[i], name_from_file);
        free(name_from_file);
        if (names_match) break;
  
        // Collision (names do not match), this is not the data you are looking for.
      }
  
      uint64_t data_size_from_file;
      if (1 != sion_fread_key(&data_size_from_file, key, sizeof(data_size_from_file), 1, sid)) {
        sion_parclose_mpi(sid);
        return PDI_ERR_SYSTEM;
      }
  
      if (data_size != data_size_from_file) {
        fprintf(stderr, "[PDI/SIONlib] Size of data for variable '%s' in file '%s' does not match memory size (%" PRIu64 " (file) vs. %zu (memory)).\n", event->vars[i], file.c_str(), data_size_from_file, data_size);
        sion_parclose_mpi(sid);
        return PDI_ERR_SYSTEM;
      }
  
      if (1 != sion_fread_key(ref.get(), key, data_size, 1, sid)) {
        sion_parclose_mpi(sid);
        return PDI_ERR_SYSTEM;
      }
    } else {
      fprintf(stderr, "[PDI/SIONlib] Dataset unavailable '%s'.\n", event->vars[i]);
      return PDI_UNAVAILABLE;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_event(const char *event)
{
  PDI_status_t write_status = PDI_OK;
  int write_attempted = 0;
  for (size_t i = 0; i < n_output_events; ++i) {
    if (!strcmp(output_events[i].name, event)) {
      write_attempted = 1;
      long select = output_events[i].select.to_long();
      if (select) write_status = write_event(&output_events[i]);
    }
  }
  if(write_status){
    if( write_status != PDI_UNAVAILABLE) {
      fprintf(stderr, "[PDI/SIONlib] Error while writing data during event %s\n", event);
    } else {
      fprintf(stderr, "[PDI/SIONlib] Warning, some dataset are unavailable and not written during event %s\n", event);
    }
  }

  PDI_status_t read_status = PDI_OK;
  int read_attempted = 0;
  for (size_t i = 0; i < n_input_events; ++i) {
    if (!strcmp(input_events[i].name, event)) {
      read_attempted = 1;
      long select = input_events[i].select.to_long();
      if (select) read_status = read_event(&input_events[i]);
    }
  }
  if(read_status){
    if( read_status != PDI_UNAVAILABLE) {
      fprintf(stderr, "[PDI/SIONlib] Error while reading data during event %s\n", event);
    } else {
      fprintf(stderr, "[PDI/SIONlib] Warning, some dataset are unavailable and not read during event %s\n", event);
    }
  }

  if (write_attempted && read_attempted) {
    // attempted to write and read, if either succeeded, call this a success
    return (write_status) ? read_status : write_status;
  } else if (write_attempted) {
    // write attempt only
    return write_status;
  } else if (read_attempted) {
    // read attempt only
    return read_status;
  } else {
    // no attempts triggered
    return PDI_OK;
  }
}

static PDI_status_t write_var(Data_r_ref& ref, const SIONlib_var_t *var)
{
  // check that data type is dense
  PDI_status_t status;
  int is_dense;
  if ((status = PDI_datatype_is_dense(&ref.type(), &is_dense))) return status;
  if (!is_dense) {
    fprintf(stderr, "[PDI/SIONlib] Sparse data is not supported.\n");
    return PDI_ERR_IMPL;
  }

  // open file
  int n_files = var->n_files.to_long();

  string file = var->file.to_str();

  size_t data_size;
  if ((status = PDI_datatype_datasize(&ref.type(), &data_size))) return status;
  sion_int64 chunksize = data_size;

  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(file.c_str(), "w", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

  // write data to file
  size_t written = sion_fwrite(ref.get(), data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || written != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_var(const Data_ref & ref, const SIONlib_var_t *var)
{
  // check that data type is dense
  PDI_status_t status;
  int is_dense;
  if ((status = PDI_datatype_is_dense(&ref.type(), &is_dense))) return status;
  if (!is_dense) {
    fprintf(stderr, "[PDI/SIONlib] Sparse data is not supported.\n");
    return PDI_ERR_IMPL;
  }

  // open file
  string file = var->file.to_str();

  int n_files = 1;
  size_t data_size;
  if ((status = PDI_datatype_datasize(&ref.type(), &data_size))) {
    return status;
  }
  sion_int64 chunksize = 0;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(file.c_str(), "r", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);

  // read data from file
  size_t read = sion_fread(ref.get(), data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || read != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data(const string& name, Data_ref cref)
{
  PDI_status_t write_status = PDI_OK;
  if ( Data_r_ref ref = cref ) {
    for (size_t i = 0; i < n_output_vars; ++i) {
      if ( name == output_vars[i].name ) {
        long select = output_vars[i].select.to_long();
        if ( select && (!write_status) ) {
          write_status = write_var(ref, &output_vars[i]);
          break;
        }
      }
    }
  }

  PDI_status_t read_status = PDI_OK;
  if ( Data_w_ref ref = cref ) {
    for (size_t i = 0; i < n_input_vars; ++i) {
      if ( name == input_vars[i].name ) {
        long select = input_vars[i].select.to_long();
        if (select && (!read_status)){
          read_status = read_var(ref, &input_vars[i]);
          break;
        }
      }
    }
  }

  if(write_status) return write_status;
  return read_status;
}

PDI_PLUGIN(SIONlib)
