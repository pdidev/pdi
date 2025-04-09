#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <mpi.h>
//#include "Damaris.h"
#include <yaml.h>
#include <paraconf.h>

#include "pdi.h"

#define MAX_CYCLES 3


int WIDTH;
int HEIGHT;
int domains; // change the number of domains from .xml file (both from the <domains count=".."> tag and domains parameter)
int size;
int rank;
int local_width;
int local_height;
int GX0;    // ****************************************************
int GX1;    // * The ghost zones for each process will be something like:
int GY0;    // * ghost="GX0:GX1,GY0:GY1". Do not forget to update the related parameters as well.
int GY1;    // ****************************************************
int total_width;
int total_height;
int ghost_value = 42;


void normalWrite(int iteration , int* array) {

    int (*parray)[total_height] =  (int (*)[total_height]) array;
    int x,y;

    int offset_width = rank*local_width;
    int offset_height = 0;

    int64_t position_space[2];

    position_space[0] = offset_width;
    position_space[1] = offset_height;

    for(x = 0; x < total_width; x++)
        for(y = 0; y < total_height; y++){

            if ((x < GX0) || (y < GY0))
                parray[x][y] = ghost_value;
            else if ((x >= total_width - GX1) || (y >= total_height - GY1))
                parray[x][y] = ghost_value;
            else
                parray[x][y] = rank + iteration;
        }

    //damaris_set_position("space",position_space);
    PDI_multi_expose("damaris_set_position"
    ,"pos_var_name", "space", PDI_OUT
    ,"position_space", position_space, PDI_OUT
    , NULL);

    //damaris_write("space",parray);
    PDI_multi_expose("damaris_write"
    ,"w_var_name", "space", PDI_OUT
    ,"parray", parray, PDI_OUT
    , NULL);
    
    //damaris_end_iteration();
    PDI_event("damaris_end_iteration");
}

void blockWrite(int iteration , int* array){

    int (*parray)[total_height] =  (int (*)[total_height]) array;
    int dom;

    for(dom=0; dom<domains ; dom++) {
        int offset_width = rank * local_width;
        int offset_height = dom * local_height;

        int64_t position_space[2];
        int x,y;

        position_space[0] = offset_width;
        position_space[1] = offset_height;

        for (x = 0; x < total_width; x++) {
            for (y = 0; y < total_height; y++) {
                if ((x < GX0) || (y < GY0))
                    parray[x][y] = ghost_value;
                else if ((x >= total_width - GX1) || (y >= total_height - GY1))
                    parray[x][y] = ghost_value;
                else
                    parray[x][y] = rank*10 +dom + iteration;
            }
        }

        //damaris_set_block_position("space", dom, position_space);
    	PDI_multi_expose("damaris_set_block_position"
    	,"bpos_var_name", "space", PDI_OUT
    	,"dom", &dom, PDI_OUT
    	,"position_space", position_space, PDI_OUT
    	, NULL);
    	
        //damaris_write_block("space", dom, array);
    	PDI_multi_expose("damaris_write_block"
    	,"wb_var_name", "space", PDI_OUT
    	,"dom", &dom, PDI_OUT
    	,"array", array, PDI_OUT
    	, NULL);
    }

    //damaris_end_iteration();
    PDI_event("damaris_end_iteration");
}

int main(int argc, char** argv)
{
	if ( argc != 2 ) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(0);
	}
	
	MPI_Init(&argc, &argv);	
	PC_tree_t conf = PC_parse_path(argv[1]);
	
	PDI_init(PC_get(conf, ".pdi"));	
	
	//PDI_event("initialize"); // or init
	PDI_event("init"); 	 
	//damaris_initialize(argv[1],MPI_COMM_WORLD);
		//MPI_Comm comm = MPI_COMM_WORLD;
		//MPI_Comm_rank(MPI_COMM_WORLD , &rank);
		//MPI_Comm_size(MPI_COMM_WORLD , &size);
	
	
	int is_client = -1;
	
	PDI_multi_expose("damaris_start"
    	,"is_client", &is_client, PDI_INOUT
    	, NULL);
    	
    	printf("-------------------------------------------------------------------------------------D: :) ;) is_client = %d\n", is_client);
	
	//if((err == DAMARIS_OK || err == DAMARIS_NO_SERVER) && is_client) {
	if(is_client) {
		
	
		//Evaluation of metadate, etc...
		
		int dsize[2];
		dsize[0] = 32;
		dsize[1] = 16;
		PDI_expose("dsize", dsize, PDI_OUT);
			
		MPI_Comm client_comm;
		//damaris_client_comm_get(&comm);
	    	PDI_multi_expose("damaris_client_comm_get"
	    	,"client_comm", &client_comm, PDI_INOUT
	    	, NULL);
		

		/*
		damaris_parameter_get("WIDTH" , &WIDTH , sizeof(int));
		damaris_parameter_get("HEIGHT" , &HEIGHT , sizeof(int));
		damaris_parameter_get("domains", &domains , sizeof(int));
		damaris_parameter_get("GX0" , &GX0 , sizeof(int));
		damaris_parameter_get("GY0" , &GY0 , sizeof(int));
		damaris_parameter_get("GX1" , &GX1 , sizeof(int));
		damaris_parameter_get("GY1" , &GY1 , sizeof(int));
		*/		
		int int_size = sizeof(int);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "HEIGHT", PDI_OUT
	    	,"prm_buffer", &HEIGHT, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "WIDTH", PDI_OUT
	    	,"prm_buffer", &WIDTH, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "domains", PDI_OUT
	    	,"prm_buffer", &domains, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "GX0", PDI_OUT
	    	,"prm_buffer", &GX0, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "GX1", PDI_OUT
	    	,"prm_buffer", &GX1, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "GY0", PDI_OUT
	    	,"prm_buffer", &GY0, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
	    	PDI_multi_expose("damaris_parameter_get"
	    	,"prm_name", "GY1", PDI_OUT
	    	,"prm_buffer", &GY1, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);
		

		MPI_Comm_rank(client_comm , &rank);
		MPI_Comm_size(client_comm , &size);
		
		assert(size < WIDTH) ;
		
		//damaris_parameter_set("size" , &size, sizeof(int)) ;
		//PDI_expose("size", &size, PDI_OUT); /*If the parameter size depends on size, this can work*/
	    	PDI_multi_expose("damaris_parameter_set"
	    	,"prm_name", "size", PDI_OUT
	    	,"prm_buffer", &size, PDI_INOUT
	    	,"prm_size", &int_size, PDI_OUT
	    	, NULL);

		local_width      = WIDTH/size;
		local_height     = HEIGHT/domains;

		total_width = local_width + GX0 + GX1; // GX:GX
		total_height = local_height + GY0 + GY1; // GY:GY

		int (*space)[total_height] = malloc((total_height)*(total_width)*sizeof(int));
		int i,j;

		for(i=0; i < MAX_CYCLES; i++) {
			double t1 = MPI_Wtime();

			if (domains == 1)
				normalWrite(i , (int*)space);
			else
				blockWrite(i , (int*)space);
			
			MPI_Barrier(client_comm);

			double t2 = MPI_Wtime();

			if(rank == 0) {
				printf("2dmesh: Iteration %d done in %f seconds\n",i,(t2-t1));
			}
		}

		
		/*if (rank == 0)
		{
		    printf("\nTotal array for rank 0 is: \n");
		    for (i = 0; i < total_width; i++) {
		        for (j = 0; j < total_height; j++)
		            printf("%3d ", space[i][j]);

		        printf("\n");
		    }
		}*/

		//damaris_stop();
		PDI_event("damaris_stop");

        	free(space);
	}

	//damaris_finalize();
	PDI_event("finalize");

	PDI_finalize();
	PC_tree_destroy(&conf);
	
	MPI_Finalize();
	return 0;
}

