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

#include <paraconf.h>

#include <sion.h>

static MPI_Comm comm;

typedef struct
{
  char *name;
  PDI_value_t directory;
  PDI_value_t generation;
  PDI_value_t select;
  PDI_value_t n_files;
} SIONlib_var_t;

typedef struct
{
  char *name;
  char *transaction;
  PDI_value_t path;
  PDI_value_t select;
  PDI_value_t n_files;
  size_t n_vars;
  char **vars;
} SIONlib_file_t;

static size_t n_output_vars = 0;
static SIONlib_var_t *output_vars = NULL;

static size_t n_input_vars = 0;
static SIONlib_var_t *input_vars = NULL;

static size_t n_output_files = 0;
static SIONlib_file_t *output_files = NULL;

static size_t n_input_files = 0;
static SIONlib_file_t *input_files = NULL;

#ifndef STRDUP_WORKS
static char *strdup(const char *s)
{
  char *p = malloc(strlen(s)+1);
  if ( p ) strcpy(p, s);
  return p;
}
#endif

static PDI_status_t read_var_property_from_config(PC_tree_t conf, const char *var_name, const char *property_name, const char *default_value, PDI_value_t *property)
{
  char *strv = NULL;
  if (PC_string(PC_get(conf, property_name), &strv)) {
    if (default_value) {
      strv = strdup(default_value);
    } else {
      fprintf(stderr, "[PDI/SIONlib] Property '%s' not found for variable %s.\n", property_name, var_name);
      free(strv);
      return PDI_ERR_CONFIG;
    }
  }
  if (!strv) return PDI_ERR_SYSTEM;
  PDI_status_t status = PDI_value_parse(strv, property);
  free(strv);
  return status;
}

static PDI_status_t read_vars_from_config(PC_tree_t conf, SIONlib_var_t *vars[], size_t *n_vars, const char *default_directory, const char *default_select)
{
  int len;
  if ( PC_len(conf, &len) ) { // if no subtree found
          *n_vars = 0;
          *vars = NULL;
          return PDI_OK;
  }
  *n_vars = len;
  *vars = calloc(sizeof(SIONlib_var_t), *n_vars);
  if (!vars) return PDI_ERR_SYSTEM;

  for (size_t i = 0; i < *n_vars; ++i) {
    PC_string(PC_get(conf, "{%zd}", i), &(*vars)[i].name);
    PC_tree_t var_conf = PC_get(conf, "<%zd>", i);

    PDI_status_t status;
    // set directory
    if ((status = read_var_property_from_config(var_conf, (*vars)[i].name, ".directory", default_directory, &(*vars)[i].directory))) return status;

    // set generation
    if ((status = read_var_property_from_config(var_conf, (*vars)[i].name, ".generation", "-1", &(*vars)[i].generation))) return status;

    // set select
    if ((status = read_var_property_from_config(var_conf, (*vars)[i].name, ".select", default_select, &(*vars)[i].select))) return status;

    // set n_files
    if ((status = read_var_property_from_config(var_conf, (*vars)[i].name, ".n_files", "1", &(*vars)[i].n_files))) return status;
  }

  return PDI_OK;
}

static PDI_status_t read_file_property_from_config(PC_tree_t conf, const char *file_name, const char *property_name, const char *default_value, PDI_value_t *property)
{
  char *strv = NULL;
  if (PC_string(PC_get(conf, property_name), &strv)) {
    if (default_value) {
      strv = strdup(default_value);
    } else {
      fprintf(stderr, "[PDI/SIONlib] Property '%s' not found for file %s.\n", property_name, file_name);
      free(strv);
      return PDI_ERR_CONFIG;
    }
  }
  if (!strv) return PDI_ERR_SYSTEM;
  PDI_status_t status = PDI_value_parse(strv, property);
  free(strv);
  return status;
}

static PDI_status_t read_file_vars_from_config(PC_tree_t conf, const char *file_name, size_t *n_vars, char ***vars)
{
  int len;
  if (PC_len(conf, &len)) {
    fprintf(stderr, "[PDI/SIONlib] No variables specified for file '%s'.\n", file_name);
    return PDI_ERR_CONFIG;
  }
  *n_vars = (size_t) len;
  *vars = calloc(sizeof(char *), *n_vars);
  if (!*vars) return PDI_ERR_SYSTEM;

  for (size_t i = 0; i < *n_vars; ++i) {
    PC_string(PC_get(conf, "[%zd]", i), *vars + i);
  }
  return PDI_OK;
}

static PDI_status_t read_files_from_config(PC_tree_t conf, SIONlib_file_t *files[], size_t *n_files)
{
  int len;
  if (PC_len(conf, &len)) { // if no subtree found
    *n_files = 0;
    *files = NULL;
    return PDI_OK;
  }
  *n_files = len;
  *files = calloc(sizeof(SIONlib_file_t), *n_files);
  if (!files) return PDI_ERR_SYSTEM;

  for (size_t i = 0; i < *n_files; ++i) {
    PC_string(PC_get(conf, "{%zd}", i), &(*files)[i].name);
    PC_tree_t file_conf = PC_get(conf, "<%zd>", i);

    // set transaction
    if (PC_string(PC_get(file_conf, ".transaction"), &(*files)[i].transaction)) {
      fprintf(stderr, "[PDI/SIONlib] Property '.transaction' not found for file %s.\n", (*files)[i].name);
      return PDI_ERR_CONFIG;
    }

    PDI_status_t status;
    // set path
    if ((status = read_file_property_from_config(file_conf, (*files)[i].name, ".path", NULL, &(*files)[i].path))) return status;

    // set select
    if ((status = read_file_property_from_config(file_conf, (*files)[i].name, ".select", "1", &(*files)[i].select))) return status;

    // set n_files
    if ((status = read_file_property_from_config(file_conf, (*files)[i].name, ".n_files", "1", &(*files)[i].n_files))) return status;

    // set variables
    PC_tree_t vars_conf = PC_get(file_conf, ".vars");
    if ((status = read_file_vars_from_config(vars_conf, (*files)[i].name, &(*files)[i].n_vars, &(*files)[i].vars))) return status;
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

  char *default_out_directory = NULL; // default output directory if none specified
  PC_string(PC_get(conf, ".variables.defaults.outputs.directory"), &default_out_directory);
  char *default_out_select = NULL; // default output select if none specified
  PC_string(PC_get(conf, ".variables.defaults.outputs.select"), &default_out_select);
  PC_tree_t output_vars_cfg = PC_get(conf, ".variables.outputs");
  PDI_status_t status = read_vars_from_config(output_vars_cfg, &output_vars, &n_output_vars, default_out_directory, default_out_select);
  free(default_out_directory);
  free(default_out_select);
  if (status) goto err0;

  char* default_in_directory = NULL; // default input directory if none specified
  PC_string(PC_get(conf, ".variables.defaults.inputs.directory"), &default_in_directory);
  char* default_in_select = NULL; // default input select if none specified
  PC_string(PC_get(conf, ".variables.defaults.inputs.select"), &default_in_select);
  PC_tree_t input_vars_cfg = PC_get(conf, ".variables.inputs");
  status = read_vars_from_config(input_vars_cfg, &input_vars, &n_input_vars, default_in_directory, default_in_select);
  free(default_in_directory);
  free(default_in_select);
  if (status) goto err0;

  PC_tree_t output_files_cfg = PC_get(conf, ".files.outputs");
  status = read_files_from_config(output_files_cfg, &output_files, &n_output_files);
  if (status) goto err0;

  PC_tree_t input_files_cfg = PC_get(conf, ".files.inputs");
  status = read_files_from_config(input_files_cfg, &input_files, &n_input_files);

err0:
  PC_errhandler(errh);
  return status;
}

static void destroy_vars(size_t n_vars, SIONlib_var_t *vars) {
  for (size_t i = 0; i < n_vars; ++i) {
    PDI_value_destroy(&vars[i].directory);
    PDI_value_destroy(&vars[i].generation);
    PDI_value_destroy(&vars[i].select);
    free(vars[i].name);
  }
  free(vars);
}

static void destroy_files(size_t n_files, SIONlib_file_t *files)
{
  for (size_t i = 0; i < n_files; ++i) {
    free(files[i].name);
    free(files[i].transaction);
    PDI_value_destroy(&files[i].path);
    PDI_value_destroy(&files[i].select);
    PDI_value_destroy(&files[i].n_files);
    for (size_t j = 0; j < files[i].n_vars; ++j) {
      free(files[i].vars[j]);
    }
    files[i].n_vars = 0;
    free(files[i].vars);
  }
  free(files);
}

PDI_status_t PDI_SIONlib_finalize()
{
  destroy_vars(n_output_vars, output_vars);
  destroy_vars(n_input_vars, input_vars);

  destroy_files(n_output_files, output_files);
  destroy_files(n_input_files, input_files);

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

static PDI_status_t write_to_file(const SIONlib_file_t *file)
{
  PDI_status_t status;
  // check that data is available and data type is dense
  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);
    if (!data || data->nb_content < 1 || !(data->content[data->nb_content - 1].access & PDI_OUT)) return PDI_UNAVAILABLE;

    int is_dense;
    if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
    if (!is_dense) {
      fprintf(stderr, "[PDI/SIONlib] Sparse data type of variable '%s' is not supported.\n", file->vars[i]);
      return PDI_ERR_IMPL;
    }
  }

  long n_files_;
  if ((status = PDI_value_int(&file->n_files, &n_files_))) return status;
  int n_files = n_files_;

  sion_int64 chunksize = 0;
  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);
    if (!data) return PDI_UNAVAILABLE;
    size_t data_size;
    if ((status = PDI_datatype_datasize(&data->type, &data_size))) return status;
    chunksize += data_size;
  }

  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);

  char *path = NULL;
  if ((status = PDI_value_str(&file->path, &path))) {
    free(path);
    return status;
  }

  int sid = sion_paropen_mpi(path, "w,keyval=inline", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);

    size_t data_size;
    if ((status = PDI_datatype_datasize(&data->type, &data_size))) {
      sion_parclose_mpi(sid);
      return status;
    }

    size_t name_size = strlen(data->name);
    uint64_t key = hash((uint8_t*)data->name, name_size);

    uint64_t name_size_to_file = name_size;
    if (1 != sion_fwrite_key(&name_size_to_file, key, sizeof(name_size_to_file), 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    if (1 != sion_fwrite_key(data->name, key, name_size, 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    uint64_t data_size_to_file = data_size;
    if (1 != sion_fwrite_key(&data_size_to_file, key, sizeof(data_size_to_file), 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    if (1 != sion_fwrite_key(data->content[data->nb_content - 1].data, key, data_size, 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_from_file(const SIONlib_file_t *file)
{
  // check that data type is dense
  PDI_status_t status;
  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);
    if (!data || data->nb_content < 1 || !(data->content[data->nb_content - 1].access & PDI_IN)) return PDI_UNAVAILABLE;

    int is_dense;
    if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
    if (!is_dense) {
      fprintf(stderr, "[PDI/SIONlib] Sparse data type of variable '%s' is not supported.\n", file->vars[i]);
      return PDI_ERR_IMPL;
    }
  }

  int n_files = 1;
  sion_int64 chunksize = 0;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);

  char *path = NULL;
  if ((status = PDI_value_str(&file->path, &path))) {
    free(path);
    return status;
  }

  int sid = sion_paropen_mpi(path, "r,keyval=unknown", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);

    size_t data_size;
    if ((status = PDI_datatype_datasize(&data->type, &data_size))) {
      sion_parclose_mpi(sid);
      return status;
    }

    size_t name_size = strlen(data->name);
    uint64_t key = hash((uint8_t*)data->name, name_size);

    for (int j = 0; ; ++j) {
      if (SION_SUCCESS != sion_seek_key(sid, key, 4 * j, 0)) {
        fprintf(stderr, "[PDI/SIONlib] Could not find variable '%s' for reading in file '%s'.\n", data->name, file->name);
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

      char *name_from_file = malloc(name_size + 1);
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

      int names_match = !strcmp(data->name, name_from_file);
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
      fprintf(stderr, "[PDI/SIONlib] Size of data for variable '%s' in file '%s' does not match memory size (%"PRIu64" (file) vs. %zu (memory)).\n", data->name, file->name, data_size_from_file, data_size);
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }

    if (1 != sion_fread_key(data->content[data->nb_content - 1].data, key, data_size, 1, sid)) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_event(const char *event)
{
  PDI_status_t write_status = PDI_OK;
  int write_attempted = 0;
  for (size_t i = 0; i < n_output_files; ++i) {
    if (!strcmp(output_files[i].transaction, event)) {
      write_attempted = 1;
      long select;
      if ((write_status = PDI_value_int(&output_files[i].select, &select))) break;
      if (select) write_status = write_to_file(&output_files[i]);
    }
  }

  PDI_status_t read_status = PDI_OK;
  int read_attempted = 0;
  for (size_t i = 0; i < n_input_files; ++i) {
    if (!strcmp(input_files[i].transaction, event)) {
      read_attempted = 1;
      long select;
      if ((read_status = PDI_value_int(&input_files[i].select, &select))) break;
      if (select) read_status = read_from_file(&input_files[i]);
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

static char *construct_path(const char *directory, const char* name, long generation) {
  size_t path_len = strlen(directory) + 1 + strlen(name) + 1;
  if (generation != -1) path_len += 1 + 9;
  char *path = malloc(path_len);
  if (!path) return NULL;

  int written;
  if (generation != -1) {
    written = snprintf(path, path_len, "%s/%s_%09ld", directory, name, generation);
  } else {
    written = snprintf(path, path_len, "%s/%s", directory, name);
  }
  if (written < 0 || (size_t)written >= path_len) {
    free(path);
    return NULL;
  }

  return path;
}


static PDI_status_t write_var_to_file(const PDI_data_t *data, const SIONlib_var_t *var)
{
  // check that data type is dense
  PDI_status_t status;
  int is_dense;
  if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
  if (!is_dense) {
    fprintf(stderr, "[PDI/SIONlib] Sparse data is not supported.\n");
    return PDI_ERR_IMPL;
  }

  // open file
  long generation;
  if ((status = PDI_value_int(&var->generation, &generation))) return status;

  long n_files_;
  if ((status = PDI_value_int(&var->n_files, &n_files_))) return status;
  int n_files = n_files_;

  char *directory;
  if ((status = PDI_value_str(&var->directory, &directory))) {
    free(directory);
    return status;
  }

  char* path = construct_path(directory, data->name, generation);
  free(directory);
  if (!path) return PDI_ERR_SYSTEM;

  size_t data_size;
  if (status = PDI_datatype_datasize(&data->type, &data_size)) {
    free(path);
    return status;
  }
  sion_int64 chunksize = data_size;

  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(path, "w", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  // write data to file
  size_t written = sion_fwrite(data->content[data->nb_content - 1].data, data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || written != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_var_from_file(const PDI_data_t *data, const SIONlib_var_t *var)
{
  // check that data type is dense
  PDI_status_t status;
  int is_dense;
  if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
  if (!is_dense) {
    fprintf(stderr, "[PDI/SIONlib] Sparse data is not supported.\n");
    return PDI_ERR_IMPL;
  }

  // open file
  long generation;
  if ((status = PDI_value_int(&var->generation, &generation))) return status;

  char *directory;
  if ((status = PDI_value_str(&var->directory, &directory))) {
    free(directory);
    return status;
  }

  char* path = construct_path(directory, data->name, generation);
  free(directory);
  if (!path) return PDI_ERR_SYSTEM;

  int n_files = 1;
  size_t data_size;
  if (status = PDI_datatype_datasize(&data->type, &data_size)) {
    free(path);
    return status;
  }
  sion_int64 chunksize = 0;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(path, "r", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  // read data from file
  size_t read = sion_fread(data->content[data->nb_content - 1].data, data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || read != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data_start(PDI_data_t *data)
{
  int access = data->content[data->nb_content - 1].access;

  PDI_status_t write_status = PDI_OK;
  if (access & PDI_OUT) {
    for (size_t i = 0; i < n_output_vars; ++i) {
      if (!strcmp(output_vars[i].name, data->name)) {
        long select;
        if ((write_status = PDI_value_int(&output_vars[i].select, &select))) break;
        if (select) write_status = write_var_to_file(data, &output_vars[i]);
        break;
      }
    }
  }

  PDI_status_t read_status = PDI_OK;
  if (access & PDI_IN) {
    for (size_t i = 0; i < n_input_vars; ++i) {
      if (!strcmp(input_vars[i].name, data->name)) {
        long select;
        if ((read_status = PDI_value_int(&input_vars[i].select, &select))) break;
        if (select) read_status = read_var_from_file(data, &input_vars[i]);
        break;
      }
    }
  }

  if (access & PDI_IN && access & PDI_OUT) {
    return (write_status) ? read_status : write_status;
  } else if (access & PDI_OUT) {
    return write_status;
  } else {
    return read_status;
  }
}

PDI_status_t PDI_SIONlib_data_end(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_PLUGIN(SIONlib)
