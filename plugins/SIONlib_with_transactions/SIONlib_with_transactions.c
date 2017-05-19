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

#include <stddef.h>
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
#include <sion_mpi.h>

static MPI_Comm comm;

typedef struct
{
  char *name;
  char *transaction;
  PDI_value_t path;
  PDI_value_t select;
  PDI_value_t n_files;
  size_t n_vars;
  char **vars;
} SIONlib_with_transactions_file_t;

static size_t n_outputs = 0;
static SIONlib_with_transactions_file_t *outputs = NULL;

static size_t n_inputs = 0;
static SIONlib_with_transactions_file_t *inputs = NULL;

#ifndef STRDUP_WORKS
static char *strdup(const char *s)
{
  char *p = malloc(strlen(s)+1);
  if ( p ) strcpy(p, s);
  return p;
}
#endif

static PDI_status_t read_file_property_from_config(PC_tree_t conf, const char *file_name, const char *property_name, const char *default_value, PDI_value_t *property)
{
  char *strv = NULL;
  if (PC_string(PC_get(conf, property_name), &strv)) {
    if (default_value) {
      strv = strdup(default_value);
    } else {
      fprintf(stderr, "[PDI/SIONlib_with_transactions] Property '%s' not found for file %s.\n", property_name, file_name);
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
    fprintf(stderr, "[PDI/SIONlib_with_transactions] No variables specified for file '%s'.\n", file_name);
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

static PDI_status_t read_files_from_config(PC_tree_t conf, SIONlib_with_transactions_file_t *files[], size_t *n_files)
{
  int len;
  if (PC_len(conf, &len)) { // if no subtree found
    *n_files = 0;
    *files = NULL;
    return PDI_OK;
  }
  *n_files = len;
  *files = calloc(sizeof(SIONlib_with_transactions_file_t), *n_files);
  if (!files) return PDI_ERR_SYSTEM;

  for (size_t i = 0; i < *n_files; ++i) {
    PC_string(PC_get(conf, "{%zd}", i), &(*files)[i].name);
    PC_tree_t file_conf = PC_get(conf, "<%zd>", i);

    // set transaction
    if (PC_string(PC_get(file_conf, ".transaction"), &(*files)[i].transaction)) {
      fprintf(stderr, "[PDI/SIONlib_with_transactions] Property '.transaction' not found for file %s.\n", (*files)[i].name);
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

PDI_status_t PDI_SIONlib_with_transactions_init(PC_tree_t conf, MPI_Comm* world)
{
  if (MPI_Comm_dup(*world, &comm)) {
    fprintf(stderr, "[PDI/SIONlib_with_transactions] Cannot duplicate MPI communicator.\n");
    return PDI_ERR_SYSTEM;
  }

  if (PC_status(conf)) {
    fprintf(stderr, "[PDI/SIONlib_with_transactions] Configuration is invalid.\n");
    return PDI_ERR_CONFIG;
  }

  PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);

  PC_tree_t outputs_cfg = PC_get(conf, ".outputs");
  PDI_status_t status = read_files_from_config(outputs_cfg, &outputs, &n_outputs);
  if (status) goto err0;

  PC_tree_t inputs_cfg = PC_get(conf, ".inputs");
  status = read_files_from_config(inputs_cfg, &inputs, &n_inputs);

err0:
  PC_errhandler(errh);
  return status;
}

static void destroy_files(size_t n_files, SIONlib_with_transactions_file_t *files)
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

PDI_status_t PDI_SIONlib_with_transactions_finalize()
{
  destroy_files(n_outputs, outputs);
  destroy_files(n_inputs, inputs);
  return PDI_OK;
}

static PDI_status_t write_to_file(const SIONlib_with_transactions_file_t *file)
{
  PDI_status_t status;
  // check that data is available and data type is dense
  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);
    if (!data || data->nb_content < 1 || !(data->content[data->nb_content - 1].access & PDI_OUT)) return PDI_UNAVAILABLE;

    int is_dense;
    if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
    if (!is_dense) {
      fprintf(stderr, "[PDI/SIONlib_with_transactions] Sparse data type of variable '%s' is not supported.\n", file->vars[i]);
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

  int sid = sion_paropen_mpi(path, "w", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);

    size_t data_size;
    if ((status = PDI_datatype_datasize(&data->type, &data_size))) {
      sion_parclose_mpi(sid);
      return status;
    }

    size_t written = sion_fwrite(data->content[data->nb_content - 1].data, data_size, 1, sid);
    if (written != 1) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_from_file(const SIONlib_with_transactions_file_t *file)
{
  // check that data type is dense
  PDI_status_t status;
  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);
    if (!data || data->nb_content < 1 || !(data->content[data->nb_content - 1].access & PDI_IN)) return PDI_UNAVAILABLE;

    int is_dense;
    if ((status = PDI_datatype_is_dense(&data->type, &is_dense))) return status;
    if (!is_dense) {
      fprintf(stderr, "[PDI/SIONlib_with_transactions] Sparse data type of variable '%s' is not supported.\n", file->vars[i]);
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

  int sid = sion_paropen_mpi(path, "r", &n_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  for (size_t i = 0; i < file->n_vars; ++i) {
    PDI_data_t *data = PDI_find_data(file->vars[i]);

    size_t data_size;
    if ((status = PDI_datatype_datasize(&data->type, &data_size))) {
      sion_parclose_mpi(sid);
      return status;
    }

    size_t read = sion_fread(data->content[data->nb_content - 1].data, data_size, 1, sid);
    if (read != 1) {
      sion_parclose_mpi(sid);
      return PDI_ERR_SYSTEM;
    }
  }

  if (SION_SUCCESS != sion_parclose_mpi(sid)) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_with_transactions_event(const char *event)
{
  PDI_status_t write_status = PDI_OK;
  int write_attempted = 0;
  for (size_t i = 0; i < n_outputs; ++i) {
    if (!strcmp(outputs[i].transaction, event)) {
      write_attempted = 1;
      long select;
      if ((write_status = PDI_value_int(&outputs[i].select, &select))) break;
      if (select) write_status = write_to_file(&outputs[i]);
    }
  }

  PDI_status_t read_status = PDI_OK;
  int read_attempted = 0;
  for (size_t i = 0; i < n_inputs; ++i) {
    if (!strcmp(inputs[i].transaction, event)) {
      read_attempted = 1;
      long select;
      if ((read_status = PDI_value_int(&inputs[i].select, &select))) break;
      if (select) read_status = read_from_file(&inputs[i]);
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

PDI_status_t PDI_SIONlib_with_transactions_data_start(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_with_transactions_data_end(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_PLUGIN(SIONlib_with_transactions)
