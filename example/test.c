#include <iostream>
#include <unistd.h>

#include "pdi.h"

void sim_main_loop (MPI_Comm comm)
{
    int i ;
    for ( i =0; i < 100; i++) {
        // do something using comm as global communicator
        damaris_end_iteration( ) ;
    }
}
 

int main(int argc, char *argv[])
{
    MPI_Init(&argc , &argv);
    damaris_initialize(argv[1] , MPI_COMM_WORLD);


    int is_client;
    int err = damaris_start(&is_client);
    printf("%d",err);
    printf("%d",is_client);
    if (&is_client) {
        printf("I am client !");
    } else {
        printf("I am not a client, i am server !");
    }
    if ((err == DAMARIS_OK || err == DAMARIS_NO_SERVER) && is_client) {
        MPI_Comm comm;

        damaris_client_comm_get(&comm);
        sim_main_loop(comm);

        damaris_stop();
    }

    damaris_finalize();
    MPI_Finalize();

    return 0;
}
