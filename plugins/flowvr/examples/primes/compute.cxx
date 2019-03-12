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
*  15/10/2018 Reworked for PDI:       Karol Sierocinski           *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./compute.cxx                                             *
*                                                                 *
* Contacts:                                                       *
*     2004  Cyril Nortet <cyril.nortet@lifo.univ-orleans.fr>      *
*     15/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>         *
*                                                                 *
******************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>

#include <iostream>

#include <pdi.h>

// Variables used for prime numbers computation :
int *tTempPrimeNumbers = 0;
int tempPrimeNumbersCount;
int tempPrimeNumbersMaxCount;

// Allow the module to identify itself among the metamodule :
int computeNodesCount;
int computeNodeRank;

/*
  ----------------------------------------------------------------------
   FlowVR related functions
  ----------------------------------------------------------------------
*/

// Send prime numbers to visualization node :
void SendPrimeNumbers(int lastIterationComputeTime)
{
	PDI_expose("tempPrimeNumbersMaxCount", &tempPrimeNumbersMaxCount, PDI_OUT);

	void* tTempPrimeNumbers_shr;
	PDI_access("tTempPrimeNumbers_shr", &tTempPrimeNumbers_shr, PDI_OUT);
	memcpy(tTempPrimeNumbers_shr, (void*)tTempPrimeNumbers, tempPrimeNumbersMaxCount*sizeof(int));
	PDI_release("tTempPrimeNumbers_shr");

	PDI_expose("lastIterationComputeTime", &lastIterationComputeTime, PDI_OUT);
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

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path("compute.yml");
	PDI_init(conf);

	PDI_expose("parallel_rank", &computeNodeRank, PDI_IN);
	PDI_expose("parallel_size", &computeNodesCount, PDI_IN);

	// Set up data structures :
	tempPrimeNumbersMaxCount = 80 / computeNodesCount; // Compute nodes share the task
	if (tempPrimeNumbersMaxCount < 1)
		tempPrimeNumbersMaxCount = 1;
	tTempPrimeNumbers = new int[tempPrimeNumbersMaxCount];
	tempPrimeNumbersCount = 0;

	// Main loop :
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	while (wait)
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
		PDI_expose("wait", &wait, PDI_IN);
	}

	// Clean up :
	PDI_finalize();
	delete [] tTempPrimeNumbers;

	return 0;
}
