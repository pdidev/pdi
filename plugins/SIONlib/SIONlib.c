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
#include <pdi/plugin.h>
#include <pdi/state.h>

#include <paraconf.h>

#include <sion.h>
#include <sion_mpi.h>

static MPI_Comm comm;

typedef struct
{
  char *name;
  PDI_value_t directory;
  PDI_value_t generation;
  PDI_value_t select;
} SIONlib_var_t;

static size_t n_outputs = 0;
static SIONlib_var_t *outputs = NULL;

static size_t n_inputs = 0;
static SIONlib_var_t *inputs = NULL;

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
  PC_string(PC_get(conf, ".defaults.outputs.directory"), &default_out_directory);
  char *default_out_select = NULL; // default output select if none specified
  PC_string(PC_get(conf, ".defaults.outputs.select"), &default_out_select);
  PC_tree_t outputs_cfg = PC_get(conf, ".outputs");
  PDI_status_t status = read_vars_from_config(outputs_cfg, &outputs, &n_outputs, default_out_directory, default_out_select);
  free(default_out_directory);
  free(default_out_select);

  if (status) goto err0;

  char* default_in_directory = NULL; // default input directory if none specified
  PC_string(PC_get(conf, ".defaults.inputs.directory"), &default_in_directory);
  char* default_in_select = NULL; // default input select if none specified
  PC_string(PC_get(conf, ".defaults.inputs.select"), &default_in_select);
  PC_tree_t inputs_cfg = PC_get(conf, ".inputs");
  status = read_vars_from_config(inputs_cfg, &inputs, &n_inputs, default_in_directory, default_in_select);
  free(default_in_directory);
  free(default_in_select);

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

PDI_status_t PDI_SIONlib_finalize()
{
  destroy_vars(n_outputs, outputs);
  destroy_vars(n_inputs, inputs);
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_event(const char *event)
{
  (void)event;
  return PDI_OK;
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


static PDI_status_t write_to_file(const PDI_data_t *data, const char *directory, long generation)
{
  // open file
  char* path = construct_path(directory, data->name, generation);
  if (!path) return PDI_ERR_SYSTEM;

  int num_files = 1;
  size_t data_size;
  PDI_status_t status = PDI_data_size(&data->type, &data_size);
  if (status) {
    free(path);
    return status;
  }
  sion_int64 chunksize = (size_t)data_size;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(path, "w", &num_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  // write data to file
  // TODO: assert type is dense/contiguous
  size_t written = sion_fwrite(data->content->data, data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || written != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

static PDI_status_t read_from_file(const PDI_data_t *data, const char *directory, long generation)
{
  // open file
  char* path = construct_path(directory, data->name, generation);
  if (!path) return PDI_ERR_SYSTEM;

  int num_files = 1;
  size_t data_size;
  PDI_status_t status = PDI_data_size(&data->type, &data_size);
  if (status) {
    free(path);
    return status;
  }
  sion_int64 chunksize = (size_t)data_size;
  sion_int32 blksize = -1;
  int rank; MPI_Comm_rank(comm, &rank);
  int sid = sion_paropen_mpi(path, "r", &num_files, comm, &comm, &chunksize, &blksize, &rank, NULL, NULL);
  free(path);

  // read data from file
  // TODO: assert type is dense/contiguous
  size_t read = sion_fread(data->content->data, data_size, 1, sid);

  // close file
  if (SION_SUCCESS != sion_parclose_mpi(sid) || read != 1) return PDI_ERR_SYSTEM;

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data_start(PDI_data_t *data)
{
  if (data->content[data->nb_content - 1].access & PDI_OUT) {
    for (size_t i = 0; i < n_outputs; ++i) {
      if (!strcmp(outputs[i].name, data->name)) {
        PDI_status_t status = PDI_OK;
        long generation;
        if ((status = PDI_value_int(&outputs[i].generation, &generation))) return status;
        long select;
        if ((status = PDI_value_int(&outputs[i].select, &select))) return status;
        char *directory;
        if ((status = PDI_value_str(&outputs[i].directory, &directory))) {
          free(directory);
          return status;
        }

        if (select) {
          if ((status = write_to_file(data, directory, generation))) {
            free(directory);
            return status;
          }
        }

        free(directory);
        break;
      }
    }
  }

  if (data->content[data->nb_content - 1].access & PDI_IN) {
    for (size_t i = 0; i < n_inputs; ++i) {
      if (!strcmp(inputs[i].name, data->name)) {
        PDI_status_t status = PDI_OK;
        long generation;
        if ((status = PDI_value_int(&inputs[i].generation, &generation))) return status;
        long select;
        if ((status = PDI_value_int(&inputs[i].select, &select))) return status;
        char *directory;
        if ((status = PDI_value_str(&inputs[i].directory, &directory))) {
          free(directory);
          return status;
        }

        if (select) {
          if ((status = read_from_file(data, directory, generation))) {
            free(directory);
            return status;
          }
        }

        free(directory);
        break;
      }
    }
  }

  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data_end(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_PLUGIN(SIONlib)
