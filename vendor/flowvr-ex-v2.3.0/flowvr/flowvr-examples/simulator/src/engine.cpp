#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h> // for usleep
#include <utime.h>
#include <iostream>
#include <fstream>

#include <vector>

#include "flowvr/module.h"

#include "simulator/Vec3D.h"
#include "simulator/types.h"


//Grille de particle
typedef struct _particle {
	Vec3D pos;
	Vec3D vit;
	float speed;
} Particle;

double *posParticles = 0;

std::vector<Particle> particles;

double Ox;
double Oy;
double Oz;

double Dx;
double Dy;
double Dz;

float speedRatio;

float localR;
float localG;
float localB;

// FlowVR objects & variables :
flowvr::ModuleAPI* pFlowVRModule = 0;
flowvr::OutputPort* pPortSimulatorOut = 0;
flowvr::BufferPool* pOutPool = 0;
flowvr::StampInfo *pStampComputeTime = 0;
flowvr::StampInfo StampID("ID",flowvr::TypeArray::create(2,flowvr::TypeInt::create()));	//Identfiant du module qui fait le calcul et nombre de particles

int computeNodesCount;
int computeNodeRank;

unsigned int itAdvance = 0;
unsigned int frequency = 50000;

// Initialize FlowVR module :
int SetupFlowVR()
{

  // Declare user defined ports :
  pPortSimulatorOut = new flowvr::OutputPort("simulatorOut");

  // Declare a new stamp for storing computation time
  //pStampComputeTime = new flowvr::StampInfo("computationTimeIt", flowvr::TypeInt::create());
  //pPortSimulatorOut->stamps->add(pStampComputeTime);

  //Declare a new stamp for storing local informations
  //StampID = new flowvr::StampInfo("EngineID",flowvr::TypeArray::create(2,flowvr::TypeInt::create()));
  pPortSimulatorOut->stamps->add(&StampID);

  std::vector <flowvr::Port*> ports;
  ports.push_back(pPortSimulatorOut);

  // Registers and initializes the module to the FlowVR daemon :
  if (!(pFlowVRModule = flowvr::initModule(ports)))
  {
    return -1;
  }

  // Create a pool of buffers :
  pOutPool = new flowvr::BufferPool();

  return 0;
}



// Clean up FlowVR module :
void CleanFlowVR()
{
  // Release buffer pool :
  if (pOutPool)
  {
    delete pOutPool;
    pOutPool = 0;
  }

  // Release FlowVR module handler :
  if (pFlowVRModule)
  {
    pFlowVRModule->close();

    delete pFlowVRModule;
    pFlowVRModule = 0;
  }
}

// Send prime numbers to visualization node :
void SendPositions(int lastIterationComputeTime)
{
  flowvr::MessageWrite msgWrite;
  
  // Request for a new buffer from the pool to send new computed prime numbers.
  //msgWrite.data = pFlowVRModule->alloc( particles.size() * 3 * sizeof(double));
  msgWrite.data = pOutPool->alloc(pFlowVRModule->getAllocator(), particles.size() * 3 * sizeof(double));
  //m.data = flowvr->alloc(text.length());

  // Fill message data :
  memcpy((void*)msgWrite.data.writeAccess(), (void*)posParticles, particles.size() * 3 * sizeof(double));
  

  // Write computation time into the 'computationTimeIt' stamp :
  //msgWrite.stamps.write(*pStampComputeTime, lastIterationComputeTime);
  msgWrite.stamps.write(StampID[0],computeNodeRank);
  msgWrite.stamps.write(StampID[1],(int)(particles.size()));

  // Transmit message to daemon :
  pFlowVRModule->put(pPortSimulatorOut, msgWrite);
}


void initGrid(){
	Ox = 5.0;
	Oy = 5.0;
	Oz = 0.0;

	Dx = 790.0;
	Dy = 590.0;
	Dz = 500.0;
}

void initParticles(unsigned int nbPart){
	particles.clear();
	if(posParticles) free(posParticles);

	posParticles = (double*)malloc(nbPart * 3 * sizeof(double));

	speedRatio = 0.5f;
	
	timeval initialTime;
	gettimeofday(&initialTime, NULL);
	int usec = (int)(initialTime.tv_sec*1000000 + initialTime.tv_usec);
	
	srand(usec + computeNodeRank * 10000);
	
	localR = ((double)rand() / (double)RAND_MAX);
	localG = ((double)rand() / (double)RAND_MAX);
	localB = ((double)rand() / (double)RAND_MAX);


	std::cout<<"Initialisation de la couleur du proc "<<computeNodeRank<<" ("<<localR<<","<<localG<<","<<localB<<")"<<std::endl;
	
	for(unsigned int i = 0; i < nbPart; i++){
		Particle p;
		p.pos.x = Ox + ((double)rand() / (double)RAND_MAX) * Dx;
		p.pos.y = Oy + ((double)rand() / (double)RAND_MAX) * Dy;
		p.pos.z = Oz + ((double)rand() / (double)RAND_MAX) * Dz;
		
		//std::cout<<"Generation d'une particle en ("<<p.pos.x<<","<<p.pos.y<<","<<p.pos.z<<")"<<std::endl;

		p.vit.x = ((double)rand() / (double)RAND_MAX);
		p.vit.y = ((double)rand() / (double)RAND_MAX);
		p.vit.z = ((double)rand() / (double)RAND_MAX);
		p.vit.normalize();
		//std::cout<<"vecteur vitesse de la particle : norme : "<<p.vit.length()<<std::endl;
		
		p.speed = ((double)rand() / (double)RAND_MAX);

		particles.push_back(p);
		posParticles[i * 3] = p.pos.x;
		posParticles[i * 3 + 1] = p.pos.y;
		posParticles[i * 3 + 2] = p.pos.z;
	}
	
	


}

void updateParticles(int deltaT){

	double deltaTF = ((double)(deltaT) / 1000.0) * speedRatio;

	for(unsigned int i = 0; i < particles.size(); i++){
		particles.at(i).pos.x += particles.at(i).vit.x * particles.at(i).speed * deltaTF;
		particles.at(i).pos.y += particles.at(i).vit.y * particles.at(i).speed * deltaTF;
		particles.at(i).pos.z += particles.at(i).vit.z * particles.at(i).speed * deltaTF;
		
		if(particles.at(i).pos.x < Ox){
		  particles.at(i).pos.x = Ox + (abs(Ox - particles.at(i).pos.x));
		  particles.at(i).vit.x = -particles.at(i).vit.x;
		}
		if(particles.at(i).pos.x > Ox + Dx){
		  particles.at(i).pos.x = Ox + Dx - (abs(Ox + Dx - particles.at(i).pos.x));
		  particles.at(i).vit.x = -particles.at(i).vit.x;
		}
		
		if(particles.at(i).pos.y < Oy){
		  particles.at(i).pos.y = Oy + (abs(Oy - particles.at(i).pos.y));
		  particles.at(i).vit.y = -particles.at(i).vit.y;
		}
		if(particles.at(i).pos.y > Oy + Dy){
		  particles.at(i).pos.y = Oy + Dy -(abs(Oy + Dy - particles.at(i).pos.y));
		  particles.at(i).vit.y = -particles.at(i).vit.y;
		}

		if(particles.at(i).pos.z < Oz){
                  particles.at(i).pos.z = Oz + (abs(Oz - particles.at(i).pos.z));
                  particles.at(i).vit.z = -particles.at(i).vit.z;
                }
                if(particles.at(i).pos.z > Oz + Dz){
                  particles.at(i).pos.z = Oz + Dz -(abs(Oz + Dz - particles.at(i).pos.z));
                  particles.at(i).vit.z = -particles.at(i).vit.z;
                }
		particles.at(i).vit.normalize();

		//Mise a jour du tableau de positons
		posParticles[i * 3] = particles.at(i).pos.x;
		posParticles[i * 3 + 1] = particles.at(i).pos.y;
		posParticles[i * 3 + 2] = particles.at(i).pos.z;
	}
}





/*
  ----------------------------------------------------------------------
   main --- main routine
  ----------------------------------------------------------------------
*/

int main(int argc, char ** argv)
{

	flowvr::Parallel::init(true);

	computeNodesCount = flowvr::Parallel::getNbProc();
	computeNodeRank = flowvr::Parallel::getRank();

	// Init FlowVR environment :
 	if (SetupFlowVR() != 0)
    		return -1;
	
	//Initialisation de la simulation
	initGrid();
	initParticles(1000 / computeNodesCount);

	int timePreviousCompute;	//Time in microsecond
	int timeCurrentCompute;
	int deltaT;

	timeval initialTime;
	gettimeofday(&initialTime, NULL);
	timePreviousCompute = (int)(initialTime.tv_sec*1000000 + initialTime.tv_usec);

	// Main loop :
  	int bLoop = 1;
  	while (bLoop)
  	{
    	// Compute a new set of prime numbers,
    	// mesuring the time elapsed during the computation :

	    timeval time;

	    gettimeofday(&time, NULL);
	    timeCurrentCompute = (int)(time.tv_sec*1000000 + time.tv_usec);

	    deltaT = timeCurrentCompute - timePreviousCompute;
	    if(deltaT < frequency)
	      usleep(frequency - deltaT);
	    timePreviousCompute = timeCurrentCompute;
	    
	    updateParticles(deltaT);


	    // Send these numbers to visualization node.
	    // Note that within the network, some modules must send messages before waiting in order to avoid deadlocks.
	    // Here, compute nodes send a set of prime numbers before the visualization node starts its rendering task.
	    SendPositions(deltaT);	   


	    // The module wait for notification from visualization node to know if it is ready to receive a new set of prime numbers.
	    // Note that there is no need to get the message as only the signal is useful in this case.
	    // Remember that the wait function blocks until every connected port receive a message, a behavior often taken in
	    // consideration for synchronization purpose.
	    bLoop = pFlowVRModule->wait();
	}

  // Clean up :
  CleanFlowVR();

  return 0;
}

	


