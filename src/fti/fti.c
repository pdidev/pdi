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

#include <mpi.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <fti.h>
#include <string.h>

MPI_Comm my_world;

int get_id(PDI_variable_t *data)
{
	int id;
	PC_int(PC_get(data->config, ".id"), &id);
	return id;
}

long get_size(PDI_variable_t *data)
{
	long total = 1;
	int i;

	switch(data->type.kind) 
	{
		case PDI_K_SCALAR: return total;
		case PDI_K_ARRAY:
			for(i = 0;i<data->type.c.array->ndims;i++)
			{
				total = total * data->type.c.array->sizes[i].c.constval;
			}
			//printf("taille pour %s : %ld\n",data->name, total);
			return total;
		break;
		case PDI_K_STRUCT:
		//TODO
		break;
		default:
		break;
	}
	return 0;
}

FTIT_type get_type(PDI_variable_t *data)
{

	switch(data->type.kind) 
	{
		case PDI_K_SCALAR:
			switch(data->type.c.scalar)
			{
				case(PDI_T_INT8):
				return FTI_CHAR;
				case(PDI_T_INT16):
				return FTI_SHRT;
				case(PDI_T_INT32):
				return FTI_INTG;
				case(PDI_T_INT64):
				return FTI_LONG;
				case(PDI_T_FLOAT):
				return FTI_SFLT;
				case(PDI_T_DOUBLE):
				return FTI_DBLE;
				case(PDI_T_LONG_DOUBLE):
				return FTI_LDBE;
			}
		break;
		case PDI_K_ARRAY:
			switch(data->type.c.array->type.c.scalar)
			{
				case(PDI_T_INT8):
				return FTI_CHAR;
				case(PDI_T_INT16):
				return FTI_SHRT;
				case(PDI_T_INT32):
				return FTI_INTG;
				case(PDI_T_INT64):
				return FTI_LONG;
				case(PDI_T_FLOAT):
				return FTI_SFLT;
				case(PDI_T_DOUBLE):
				return FTI_DBLE;
				case(PDI_T_LONG_DOUBLE):
				return FTI_LDBE;
			}
		break;
		case PDI_K_STRUCT:
		//TODO
		break;
		default:
		break;
	}
	return FTI_CHAR;
}


PDI_status_t PDI_fti_init(PC_tree_t conf, MPI_Comm *world)
{
	my_world = *world;
	char * fti_file;

	PC_string(PC_get(conf, ".config"), &fti_file);

	FTI_Init(fti_file,my_world);

	free(fti_file);


	
	return PDI_OK;
}

PDI_status_t PDI_fti_finalize()
{
	FTI_Finalize();
	return PDI_OK;
}

PDI_status_t PDI_fti_event(const char *event)
{
	
	if(strcmp(event,"Snapshot")==0)
	{
		FTI_Snapshot();
	}
	
	return PDI_OK;
}

PDI_status_t PDI_fti_data_start(PDI_variable_t *data)
{

	FTI_Protect(get_id(data),data->content.data,get_size(data),get_type(data));
	
	return PDI_OK;
}

PDI_status_t PDI_fti_data_end(PDI_variable_t *data)
{

	FTI_Protect(get_id(data),NULL,0,get_type(data));
	
	return PDI_OK;
}

PDI_PLUGIN(fti)
