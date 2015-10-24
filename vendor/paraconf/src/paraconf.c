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

#include <string.h>
#include <stdlib.h>

#include "paraconf.h"

#define PC_BUFFER_SIZE 256

PC_status PC_get(yaml_node_t* document, const char* index, yaml_node_t* value)
{
	char tmp[PC_BUFFER_SIZE];
	
	value = document;
	
	while ( *index ) {
		switch ( *index ) {
		case '[': {
			int ii; long ll;
			if ( value->type != YAML_SEQUENCE_NODE ) return PC_INVALID_NODE_TYPE;
			++index; // consume the starting '['
			for ( ii=0; *index != ']' && ii<PC_BUFFER_SIZE; ++ii ) {
				tmp[ii] = index[ii];
			}
			tmp[ii] = 0;
			if ( index[ii] != ']' ) return PC_INVALID_PARAMETER;
			index += ii+1;
		}; break;
		case '.': {
		}; break;
		
		}
	}
	return PC_OK;
}

PC_status PC_get_double(yaml_node_t* document, const char* index, double* value)
{
	return PC_OK;
}

PC_status PC_get_int(yaml_node_t* document, const char* index, int* value)
{
	return PC_OK;
}

PC_status PC_get_string(yaml_node_t* document, const char* index, char* value)
{
	return PC_OK;
}

PC_status PC_broadcast(yaml_node_t* document, int count, int root, MPI_Comm comm)
{
	char data[255];
	MPI_Bcast(data, 255, MPI_CHAR, root, comm);
	return PC_OK;
}

