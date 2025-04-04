#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>

#include <mpi.h>
//#include "Damaris.h"
#include <yaml.h>
#include <paraconf.h>

#include "pdi.h"


/** C code: 3dmesh_py
 Damaris example of obtaining data from a simulation via Python

 mpirun --oversubscribe -np 5 ./3dmesh_py 3dmesh_py.xml -i 3 -v 2 -r
 
*/
void print_usage(char* exename) {
   
      fprintf(stderr,"Usage: %s <3dmesh_py.xml> [-v] [-r] [-s X]\n",exename);
      fprintf(stderr,"-v  <X>    X = 0 default, do not print arrays\n");
      fprintf(stderr,"           X = 1 Verbose mode, prints arrays\n");
      fprintf(stderr,"           X = 2 Verbose mode, prints summation of arrays\n");
      fprintf(stderr,"-r         Array values set as rank of process\n");
      fprintf(stderr,"-s  <Y>    Y is integer time to sleep in sconds between iterations\n");
      fprintf(stderr,"-i  <I>    I is the number of iterations of simulation to run\n");
}

int WIDTH;
int HEIGHT;
int DEPTH;
int MAX_CYCLES ;


int main(int argc, char** argv)
{
   if(argc < 2)
   {
      print_usage(argv[0]) ;
      exit(0);
   }

   MPI_Init(&argc, &argv);	
   PC_tree_t conf = PC_parse_path(argv[1]);
   PDI_init(PC_get(conf, ".pdi"));	

   //damaris_initialize(argv[1],MPI_COMM_WORLD);
   PDI_event("initialize"); // or init
   //PDI_event("init"); 	
   
  int verbose = 0;
  int rank_only = 0;
  int current_arg = 2 ;  
  int time = 1 ;
  MAX_CYCLES = 5;  // default number of iterations to run
  while (current_arg < argc ) 
  {
    if (strcmp(argv[current_arg],"-v") == 0) {
        current_arg++;
        verbose = atoi(argv[current_arg]);
    }
    else if (strcmp(argv[current_arg],"-r") == 0)
      rank_only = 1 ;
    else if (strcmp(argv[current_arg],"-s") == 0) {
        current_arg++;
        time = atoi(argv[current_arg]);
    } else if (strcmp(argv[current_arg],"-i") == 0) {
        current_arg++;
        MAX_CYCLES = atoi(argv[current_arg]);
    } else if (strcmp(argv[current_arg],"-h") == 0) {
        print_usage(argv[0]) ;
        exit(0);
    }
    
      current_arg++;
  }

  

   int size_client, rank_client, whd_layout;

      
   int is_client = -1;
   //int err = damaris_start(&is_client);

   PDI_multi_expose("damaris_start"
   ,"is_client", &is_client, PDI_INOUT
   , NULL);

   printf("-------------------------------------------------------------------------------------D: :) ;) is_client = %d\n", is_client);


   // if((err == DAMARIS_OK || err == DAMARIS_NO_SERVER) && is_client) {
   //if(err == DAMARIS_OK && is_client) {
   if(is_client) {

        MPI_Comm comm;
	//damaris_client_comm_get(&comm);
    	PDI_multi_expose("damaris_client_comm_get"
    	,"client_comm", &comm, PDI_INOUT
    	, NULL);

        //damaris_parameter_get("WIDTH" , &WIDTH , sizeof(int));
        //damaris_parameter_get("HEIGHT", &HEIGHT, sizeof(int));
        //damaris_parameter_get("DEPTH" , &DEPTH , sizeof(int));
        //damaris_parameter_get("whd_layout" , &whd_layout , sizeof(int));
        
	int int_size = sizeof(int);
    	PDI_multi_expose("damaris_parameter_get"
    	,"prm_name", "WIDTH", PDI_OUT
    	,"prm_buffer", &WIDTH, PDI_INOUT
    	,"prm_size", &int_size, PDI_OUT
    	, NULL);
    	PDI_multi_expose("damaris_parameter_get"
    	,"prm_name", "HEIGHT", PDI_OUT
    	,"prm_buffer", &HEIGHT, PDI_INOUT
    	,"prm_size", &int_size, PDI_OUT
    	, NULL);
    	PDI_multi_expose("damaris_parameter_get"
    	,"prm_name", "DEPTH", PDI_OUT
    	,"prm_buffer", &DEPTH, PDI_INOUT
    	,"prm_size", &int_size, PDI_OUT
    	, NULL);
    	PDI_multi_expose("damaris_parameter_get"
    	,"prm_name", "whd_layout", PDI_OUT
    	,"prm_buffer", &whd_layout, PDI_INOUT
    	,"prm_size", &int_size, PDI_OUT
    	, NULL);

        MPI_Comm_rank(comm , &rank_client);
        MPI_Comm_size(comm , &size_client);

        if (verbose > 0 )
            fprintf(stdout,"C++ Input paramaters found: v=%d r=%d (0 is not found)\n",verbose, rank_only);

        // Dynamically update the size used in the Damaris layout configuration
        //damaris_parameter_set("size" , &size_client , sizeof(int));
    	PDI_multi_expose("damaris_parameter_set"
    	,"prm_name", "size", PDI_OUT
    	,"prm_buffer", &size_client, PDI_INOUT
    	,"prm_size", &int_size, PDI_OUT
    	, NULL);

        int64_t position_cube[3];
        int local_width, local_height, local_depth  ;
        int rank_start = 0 ;

     
        if (size_client > DEPTH) {
         printf("ERROR: MPI process count (size=%2d) is greater than the blocked index (DEPTH=%2d)\t", size_client, DEPTH  );
         exit(-1);
        }
        // used with: <layout name="cells_whd_wf" type="int" dimensions="WIDTH/size,HEIGHT,DEPTH" global="WIDTH,HEIGHT,DEPTH" />
        local_width      = WIDTH ; // WIDTH/size;
        local_height     = HEIGHT;
        local_depth      = DEPTH/size_client;

        position_cube[0] = rank_client*local_depth;
        position_cube[1] = 0;
        position_cube[2] = 0;

        rank_start = rank_client * local_width * local_height ;

        // allocate the local data array
        int cube[local_depth][local_height][local_width];
        float cube_f[local_depth][local_height][local_width];

        // set the appropriate position for the current rank
        //damaris_set_position("cube_i",position_cube);
        //damaris_set_position("cube_f",position_cube);
        PDI_multi_expose("damaris_set_position"
        ,"pos_var_name", "cube_i", PDI_OUT
        ,"position_cube", position_cube, PDI_OUT
        , NULL);
        PDI_multi_expose("damaris_set_position"
        ,"pos_var_name", "cube_f", PDI_OUT
        ,"position_cube", position_cube, PDI_OUT
        , NULL);

        // do not do this here as we will not get an updated value (if time-varying="true" )
        // damaris_write("last_iter" , &MAX_CYCLES);
	
	sleep(time);

        int i, d, h, w ;
        for( i=0; i < MAX_CYCLES; i++) {
            double t1 = MPI_Wtime();         
            int sequence ;
            sequence =  i ;  // this is the start of the sequence for this iteration
            if (verbose == 0) { // default - do not print values to screen

                for ( d = 0; d < local_depth; d++){
                    for ( h = 0; h < local_height; h++){
                         for ( w = 0; w < local_width; w++) {

                            cube[d][h][w] = (int) sequence + rank_start;
                            cube_f[d][h][w] = (float) sequence + rank_start +0.5f;
                            if (rank_only==0) sequence++;
                         }  
                    }
                }            
            } else  if (verbose == 1) { // print values to screen
            int current_rank = 0 ;
            int sending_rank = 0 ;
             // serialize the print statements
             while ( current_rank < size_client)
             {
                printf("\n"); 
                if (rank_client == current_rank) { // start of serialized section

                  for ( d = 0; d < local_depth; d++){
                    for ( h = 0; h < local_height; h++){
                     for ( w = 0; w < local_width; w++) {
                        cube[d][h][w] = (int) sequence + rank_start;
                        cube_f[d][h][w] = (float) sequence + rank_start +0.5f;
                        printf("%2d\t", cube[d][h][w] );
                        if (rank_only==0) sequence++;
                     }
                     printf("\n");
                    }
                    printf("\n");
                  }
                  fflush(stdin); 
                  sending_rank = current_rank ;
                  current_rank++ ;
                } // end of serialized section
                MPI_Bcast(&current_rank,1, MPI_INT,sending_rank, comm);
                sending_rank = current_rank ;
            } // end of in-order loop over ranks
            } else  if (verbose == 2) { // print sumation values to screen
            
                long int sumdata = 0 ;
                for ( d = 0; d < local_depth; d++){
                    for ( h = 0; h < local_height; h++){
                        for ( w = 0; w < local_width; w++) {
                           cube[d][h][w] = (int) sequence + rank_start ;
                           cube_f[d][h][w] = (float) sequence + rank_start +0.5f;
                           sumdata += cube[d][h][w] ;
                           if (rank_only==0) sequence++;
                        }
                    }
                } 
                  
                // Each MPI process sends its rank to reduction, root MPI process collects the result
                long int reduction_result = 0;
                MPI_Reduce(&sumdata, &reduction_result, 1, MPI_LONG, MPI_SUM, 0, comm);
                
                
                if (rank_client == 0) 
                    printf("C++    iteration  %d , Sum () = %ld\n", i, reduction_result );
              
                double array_sum = (double) reduction_result ;
                //damaris_write("array_sum" , &array_sum);  // Used to confirm the summation value found in Dask
	        PDI_multi_expose("damaris_write"
	        ,"w_var_name", "array_sum", PDI_OUT
	        ,"array_sum", &array_sum, PDI_OUT
	        , NULL);
            }
            
            //damaris_write("cube_i" , cube);
            //damaris_write("cube_f" , cube_f);
            PDI_multi_expose("damaris_write"
            ,"w_var_name", "cube_i", PDI_OUT
            ,"cube", cube, PDI_OUT
            , NULL);
            PDI_multi_expose("damaris_write"
            ,"w_var_name", "cube_f", PDI_OUT
            ,"cube_f", cube_f, PDI_OUT
            , NULL);
            
            //damaris_write("last_iter" , &MAX_CYCLES); // The Damaris XML variable has time-varing=true so we must give an updated value at each iteration
            PDI_multi_expose("damaris_write"
            ,"w_var_name", "last_iter", PDI_OUT
            ,"MAX_CYCLES", &MAX_CYCLES, PDI_OUT
            , NULL);
            sleep(time);

	    //damaris_end_iteration();
	    PDI_event("damaris_end_iteration");
            MPI_Barrier(comm);

            // sleep(time);

            double t2 = MPI_Wtime();

            if(rank_client == 0) {
                if (verbose > 0 )
                    printf("C++    iteration %d done in %f seconds\n",i,(t2-t1)); 
            }
            fflush(stdin);
        }  // end of for loop over MAX_CYCLES

	//damaris_stop();
	PDI_event("damaris_stop");
   }

   //damaris_finalize();
   PDI_event("finalize");
   PDI_finalize();	
   MPI_Finalize();
   
   return 0;
}
