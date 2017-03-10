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

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>

#include <sion.h>
#include <sion_mpi.h>

PDI_status_t PDI_SIONlib_init(PC_tree_t conf, MPI_Comm* world)
{
  (void)conf;
  (void)world;
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_finalize()
{
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_event(const char *event)
{
  (void)event;
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data_start(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_status_t PDI_SIONlib_data_end(PDI_data_t *data)
{
  (void)data;
  return PDI_OK;
}

PDI_PLUGIN(SIONlib)
