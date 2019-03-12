/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./compute.cpp                                             *
*                                                                 *
* Contacts:                                                       *
*     2004  Cyril Nortet <cyril.nortet@lifo.univ-orleans.fr>      *
*                                                                 *
******************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>

#include "flowvr/module.h"


// Variables used for prime numbers computation :
unsigned int *tTempPrimeNumbers = 0;
unsigned int tempPrimeNumbersCount;
unsigned int tempPrimeNumbersMaxCount;

// Allow the module to identify itself among the metamodule :
unsigned int computeNodesCount;
unsigned int computeNodeRank;

// FlowVR objects & variables :
flowvr::ModuleAPI* pFlowVRModule = 0;
flowvr::OutputPort* pPortPrimesOut = 0;
flowvr::BufferPool* pOutPool = 0;
flowvr::StampInfo *pStampComputeTime = 0;


/*
  ----------------------------------------------------------------------
   FlowVR related functions
  ----------------------------------------------------------------------
*/

// Initialize FlowVR module :
int SetupFlowVR()
{

  // Declare user defined ports :
  pPortPrimesOut = new flowvr::OutputPort("primesOut");

  // Declare a new stamp for storing computation time
  // We use a stamp so this data can be easily handled by filters (when merging several messages, 'computationTimeIt' are accumulated)
  pStampComputeTime = new flowvr::StampInfo("computationTimeIt", flowvr::TypeInt::create());
  pPortPrimesOut->stamps->add(pStampComputeTime);

  std::vector <flowvr::Port*> ports;
  ports.push_back(pPortPrimesOut);

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
void SendPrimeNumbers(int lastIterationComputeTime)
{
  flowvr::MessageWrite msgWrite;

  // Request for a new buffer from the pool to send new computed prime numbers.
  msgWrite.data = pOutPool->alloc(pFlowVRModule->getAllocator(), tempPrimeNumbersMaxCount*sizeof(unsigned int));

  // Fill message data :
  memcpy((void*)msgWrite.data.writeAccess(), (void*)tTempPrimeNumbers, tempPrimeNumbersMaxCount*sizeof(unsigned int));

  // Write computation time into the 'computationTimeIt' stamp :
  msgWrite.stamps.write(*pStampComputeTime, lastIterationComputeTime);

  // Transmit message to daemon :
  pFlowVRModule->put(pPortPrimesOut, msgWrite);
}




/*
  ----------------------------------------------------------------------
   Functions used to compute prime numbers
  ----------------------------------------------------------------------
*/

// Test if 'number' is prime :
inline int IsPrime(unsigned int number)
{
  unsigned int divisor;
  int bIsPrime;

  // Special cases :
  if (number <= 2)
    return (number == 2);  // 2 is prime, not 0 neither 1

  // Check divisibility :
  bIsPrime = 1;
  divisor = 3;
  while ( (bIsPrime) && (divisor < (number-1)) )
  {
    bIsPrime = number % divisor;
    divisor += 2; // An even number can't be prime, so no need to check even divisors too
  }

  return bIsPrime;
}



// Find a set of new prime numbers :
void ComputePrimeNumbers()
{
  static int bStartingNumberInitialized = 0; // First call to the function need initialization
  static unsigned int number = 0; // Continuously incremented integer tested for primality

  tempPrimeNumbersCount = 0;

  if (!bStartingNumberInitialized)
  {
    number = 3 + 2*computeNodeRank; // The global calculation is "interlaced", each module starting at different step
    bStartingNumberInitialized = 1;

    // Special case : 2 is a prime number
    if (computeNodeRank == 0)
      tTempPrimeNumbers[tempPrimeNumbersCount++] = 2;
  }

  while (tempPrimeNumbersCount < tempPrimeNumbersMaxCount)
  {
    if (IsPrime(number))
      tTempPrimeNumbers[tempPrimeNumbersCount++] = number;

    number += 2 * computeNodesCount; // A prime number cannot be even, and each compute node treat only a subset of all integers
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

  // Set up data structures :
  tempPrimeNumbersMaxCount = 80 / computeNodesCount; // Compute nodes share the task
  if (tempPrimeNumbersMaxCount < 1)
    tempPrimeNumbersMaxCount = 1;
  tTempPrimeNumbers = new unsigned int[tempPrimeNumbersMaxCount];
  tempPrimeNumbersCount = 0;

  // Main loop :
  int bLoop = 1;
  while (bLoop)
  {
    // Compute a new set of prime numbers,
    // mesuring the time elapsed during the computation :

    timeval time;
    int timeBeforeCompute, timeAfterCompute;  // Time in microseconds

    gettimeofday(&time, NULL);
    timeBeforeCompute = (int)(time.tv_sec*1000000 + time.tv_usec);

    ComputePrimeNumbers();

    gettimeofday(&time, NULL);
    timeAfterCompute = (int)(time.tv_sec*1000000 + time.tv_usec);


    // Send these numbers to visualization node.
    // Note that within the network, some modules must send messages before waiting in order to avoid deadlocks.
    // Here, compute nodes send a set of prime numbers before the visualization node starts its rendering task.
    SendPrimeNumbers(timeAfterCompute-timeBeforeCompute);


    // The module wait for notification from visualization node to know if it is ready to receive a new set of prime numbers.
    // Note that there is no need to get the message as only the signal is useful in this case.
    // Remember that the wait function blocks until every connected port receive a message, a behavior often taken in
    // consideration for synchronization purpose.
    bLoop = pFlowVRModule->wait();
  }

  // Clean up :
  CleanFlowVR();
  delete [] tTempPrimeNumbers;

  return 0;
}
