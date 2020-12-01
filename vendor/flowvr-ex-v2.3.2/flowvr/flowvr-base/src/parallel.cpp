/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/parallel.cpp                                          *
*                                                                 *
* Contacts:                                                       *
*  10/22/2004 Clement Menier <Clement.Menier@inrialpes.fr>        *
*                                                                 *
******************************************************************/

#include "flowvr/parallel.h"
#include <stdlib.h>
#include <iostream>

namespace flowvr
{

// Rank of the process.
int Parallel::rank = 0;
// Number of processes.
int Parallel::nbProc = 1;
// Is parallel interface initialized.
bool Parallel::initialized = false;
// Is the process in parallel mode.
bool Parallel::parallel = false;

// Initializes the Parallel Interface with the environment variables FLOWVR_RANK and FLOWVR_NBPROC.
void Parallel::init()
{
  if (!initialized) {
    char *rankEnv = getenv("FLOWVR_RANK");
    char *nbProcEnv = getenv("FLOWVR_NBPROC");
    if (rankEnv == NULL || rankEnv[0] == '\0' || nbProcEnv == NULL || nbProcEnv[0] == '\0') {
      parallel = false;
      rank = 0;
      nbProc = 1;
    } else {
      rank = atoi(rankEnv);
      nbProc = atoi(nbProcEnv);
      parallel = true;
    }
    initialized = true;
  }
}

// Initializes the Parallel Interface and forces the parallel mode.
void Parallel::init(bool forcing)
{
  if (forcing == false) {
    parallel = false;
    rank = 0;
    nbProc = 1;
  } else {
    init();
    parallel = true;
  }
  initialized = true;
}

// Initializes the Parallel Interface and activate the parallel mode "manually".
void Parallel::init(int _rank, int _nbProc)
{
  initialized = true;
  parallel = true;
  rank = _rank;
  nbProc = _nbProc;
}

// Close the Parallel Interface
void Parallel::close()
{
  initialized = false;
  parallel = false;
  rank = 0;
  nbProc = 1;
}

// Test if the Parallel Interface is initialized.
bool Parallel::isInitialized()
{
  return initialized;
}

// Test if the application is in parallel mode.
bool Parallel::isParallel()
{
  return parallel;
}

}
