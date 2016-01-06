/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#define _GNU_SOURCE
#include <stdio.h>
#include <dlfcn.h>
#include <stdarg.h>

#include <paraconf.h>

#include "pdi/plugin.h"

#include "plugin_loader.h"

#define PRINTF_BUFFER_SIZE 256

static char *vmsprintf(const char *fmt, va_list ap)
{
	int index_size = PRINTF_BUFFER_SIZE;
	char *index = malloc(index_size);
	while ( vsnprintf(index, index_size, fmt, ap) > index_size ) {
		index_size *= 2;
		index = realloc(index, PRINTF_BUFFER_SIZE);
	}
	return index;
}

static char *msprintf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char *res = vmsprintf(fmt, ap);
	va_end(ap);
	return res;
}

typedef PDI_status_t (*init_f)(PC_tree_t conf, MPI_Comm *world, PDI_plugin_t* plugin);

PDI_status_t plugin_loader_load(char *plugin_name, PC_tree_t node, MPI_Comm *world, PDI_plugin_t* plugin)
{
	PDI_status_t err = PDI_OK;
	
	char *plugin_symbol = msprintf("PDI_plugin_%s_ctor", plugin_name);
	// ugly data to function ptr cast to be standard compatible (though undefined behavior)
	void *plugin_ctor_uncast = dlsym(NULL, plugin_symbol);
	init_f plugin_ctor = *((init_f *)&plugin_ctor_uncast);
	if ( !plugin_ctor ) {
		err = PDI_ERR_PLUGIN;
		goto load_err0;
	}
	
	err = plugin_ctor(node, world, plugin); if (err) goto load_err0;
	
load_err0:
	free(plugin_symbol);
	return err;
}
