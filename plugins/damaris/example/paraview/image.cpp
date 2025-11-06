#include <iostream>
#include <unistd.h>

//#include "mpi.h"
#include <mpi.h>
//#include "Damaris.h"
#include <yaml.h>
#include <paraconf.h>

#include "pdi.h"

#include <string>
using std::string;

int Steps = 1000;


struct simdata {
  double* zonal_cube;
  double* nodal_cube;
    int step;
    int rank;
    int size;

  int zonal_x;
  int zonal_y;
  int zonal_z;

  int nodal_x;
  int nodal_y;
  int nodal_z;
};


template <typename T>
void setZonalValue(simdata& sim , T value, int i , int j , int k=0) {
  if ((i == sim.zonal_x) || (j == sim.zonal_y) || (k == sim.zonal_z))
    return;

  sim.zonal_cube[i + j * sim.zonal_x + k * sim.zonal_y * sim.zonal_x] = value; // row major
}

template <typename T>
void setNodalValue(simdata& sim , T value, int i , int j , int k=0) {
  sim.nodal_cube[i + j * sim.nodal_x + k * sim.nodal_y * sim.nodal_x] = value; // row major
}

double GetFillValue(simdata& sim, int i , int j , int k)
{
    if ((sim.step % 100) == i)
        return -100;

    int a = (int)k/10;
    int b = (int)i/10;

    return ((a+b)*10+(sim.rank*100));
}

void InitSimData(simdata &sim ,  MPI_Comm comm)
{
    int X,Y,Z;
    MPI_Comm_size(comm , &sim.size);
    MPI_Comm_rank(comm , &sim.rank);
    
    int int_size = sizeof(int);
    // Pass the specific size parameter back to Damaris
    // damaris_parameter_set("size",&sim.size,sizeof(int));
    char * prm_name = "size"; 
    PDI_multi_expose("damaris_parameter_set"
    ,"prm_name", prm_name, PDI_OUT
    ,"prm_buffer", &sim.size, PDI_INOUT
    ,"prm_size", &int_size, PDI_OUT
    , NULL);

    //damaris_parameter_get("WIDTH",&X,sizeof(int));
    //damaris_parameter_get("HEIGHT",&Y,sizeof(int));
    //damaris_parameter_get("DEPTH",&Z,sizeof(int));
    prm_name = "WIDTH"; 
    PDI_multi_expose("damaris_parameter_get"
    ,"prm_name", prm_name, PDI_OUT
    ,"prm_buffer", &X, PDI_INOUT
    ,"prm_size", &int_size, PDI_OUT
    , NULL);
    prm_name = "HEIGHT"; 
    PDI_multi_expose("damaris_parameter_get"
    ,"prm_name", prm_name, PDI_OUT
    ,"prm_buffer", &Y, PDI_INOUT
    ,"prm_size", &int_size, PDI_OUT
    , NULL);
    prm_name = "DEPTH"; 
    PDI_multi_expose("damaris_parameter_get"
    ,"prm_name", prm_name, PDI_OUT
    ,"prm_buffer", &Z, PDI_INOUT
    ,"prm_size", &int_size, PDI_OUT
    , NULL);

    // Split the cube over the Z direction
    int local_z = Z/sim.size;

      sim.zonal_x = X;
      sim.zonal_y = Y;
      sim.zonal_z = local_z;
      sim.zonal_cube = new double[sim.zonal_x*sim.zonal_y*sim.zonal_z];

      sim.nodal_x = sim.zonal_x + 1;
      sim.nodal_y = sim.zonal_y + 1;
      sim.nodal_z = sim.zonal_z + 1;
      sim.nodal_cube = new double[sim.nodal_x*sim.nodal_y*sim.nodal_z];
}

void FreeSimData(simdata& sim)
{
   delete [] sim.zonal_cube;
   delete [] sim.nodal_cube;

   sim.zonal_cube = nullptr;
   sim.nodal_cube = nullptr;
}

void WriteCoordinates(simdata sim)
{
  float* XCoord = new float[sim.nodal_x];
  float* YCoord = new float[sim.nodal_y];
  float* ZCoord = new float[sim.nodal_z];

  for(int i=0; i<sim.nodal_x ; i++)
    XCoord[i] = i*2;

  for(int j=0; j<sim.nodal_y ; j++)
    YCoord[j] = j*3;

  for(int k=0; k<sim.nodal_z ; k++)
    ZCoord[k] = k+sim.rank*sim.zonal_z;


  //damaris_write("coord/x" , XCoord);
  //damaris_write("coord/y" , YCoord);
  //damaris_write("coord/z" , ZCoord);
  char * prm_name = "coord/x"; 
  PDI_multi_expose("damaris_write"
  ,"w_var_name", prm_name, PDI_OUT
  ,"XCoord", XCoord, PDI_OUT
  , NULL);
  prm_name = "coord/y"; 
  PDI_multi_expose("damaris_write"
  ,"w_var_name", prm_name, PDI_OUT
  ,"YCoord", YCoord, PDI_OUT
  , NULL);
  prm_name = "coord/z"; 
  PDI_multi_expose("damaris_write"
  ,"w_var_name", prm_name, PDI_OUT
  ,"ZCoord", ZCoord, PDI_OUT
  , NULL);
    

  delete [] XCoord;
  delete [] YCoord;
  delete [] ZCoord;
}

void SimMainLoop(simdata& sim)
{
  for(int i=0; i<=sim.zonal_x; i++)
    for(int j=0; j<=sim.zonal_y; j++)
      for(int k=0; k<=sim.zonal_z; k++)
      {
        setZonalValue(sim , GetFillValue(sim , i , j , k) ,  i , j , k  );
        setNodalValue(sim , i*j*GetFillValue(sim , i,j,k) ,  i , j , k  );
      }

   // write results to Damaris
   if (sim.step % 10 == 0)
   {
      if (sim.rank == 0)
      {
         std::cout << "Image example: Iteration " << sim.step << " out of " << Steps << std::endl;
      }
      int64_t pos[3];

      pos[0] = 0;
      pos[1] = 0;
      pos[2] = sim.rank*sim.zonal_z;
	
      char * prm_name = "zonal_pressure"; 
      //damaris_set_position("zonal_pressure" ,  pos);
      PDI_multi_expose("damaris_set_position"
      ,"pos_var_name", prm_name, PDI_OUT
      ,"pos", pos, PDI_OUT
      , NULL);
      //damaris_write("zonal_pressure" , sim.zonal_cube);
      PDI_multi_expose("damaris_write"
      ,"w_var_name", prm_name, PDI_OUT
      ,"sim.zonal_cube", sim.zonal_cube, PDI_OUT
      , NULL);

      prm_name = "nodal_pressure"; 
      //damaris_set_position("nodal_pressure" ,  pos);
      PDI_multi_expose("damaris_set_position"
      ,"pos_var_name", prm_name, PDI_OUT
      ,"pos", pos, PDI_OUT
      , NULL);
      //damaris_write("nodal_pressure" , sim.nodal_cube);
      PDI_multi_expose("damaris_write"
      ,"w_var_name", prm_name, PDI_OUT
      ,"sim.zonal_cube", sim.nodal_cube, PDI_OUT
      , NULL);

      //damaris_end_iteration();
      PDI_event("damaris_end_iteration");

      sleep(5);
   }

}

int main(int argc, char *argv[])
{
    if ( argc != 2 ) {
	fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
	exit(0);
    }

    MPI_Init(&argc, &argv);	
    PC_tree_t conf = PC_parse_path(argv[1]);

    PDI_init(PC_get(conf, ".pdi"));	
   //damaris_initialize(argv[1],MPI_COMM_WORLD);
   PDI_event("initialize"); // or init
   //PDI_event("init"); 


   int is_client = -1;
   //int err = damaris_start(&is_client);

   PDI_multi_expose("damaris_start"
   ,"is_client", &is_client, PDI_INOUT
   , NULL);

   // if((err == DAMARIS_OK || err == DAMARIS_NO_SERVER) && is_client) {
   if(is_client) {
        simdata sim;
        MPI_Comm comm;

	//damaris_client_comm_get(&comm);
    	PDI_multi_expose("damaris_client_comm_get"
    	,"client_comm", &comm, PDI_INOUT
    	, NULL);


        InitSimData(sim , comm);
        WriteCoordinates(sim);

        for(int s=0; s < Steps ; s++) {
            sim.step = s;
            SimMainLoop(sim);
        }

        FreeSimData(sim);
	//damaris_stop();
	PDI_event("damaris_stop");
    }

    //damaris_finalize();
    PDI_event("finalize");
    PDI_finalize();
    MPI_Finalize();

    return 0;
}
