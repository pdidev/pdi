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
* File: include/flowvr/parallel.h                                 *
*                                                                 *
* Contacts:                                                       *
*  22/10/2004 Clement Menier <Clement.Menier@inrialpes.fr>        *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PARALLEL_H
#define FLOWVR_PARALLEL_H

namespace flowvr 
{

/// Parallel Interface
class Parallel
{
 public:
  
  /// Initialize the parallel interface by looking at FLOWVR_RANK et FLOWVR_NBPROC variables.
  static void init();

  /// Intiialize the parallel interface by forcing the parallel mode.
  static void init(bool forcing);

  /// Initialize the parallel interface by giving the rank and the number of processes.
  static void init(int rank, int nbProc);

  /// Close the parallel interface.
  static void close();

  /// Test if the parallel interface is initialized.
  static bool isInitialized();

  /// Test if the program is in parallel mode.
  static bool isParallel();

  /// Retrieve the rank of the process.
  static int getRank() { return rank; }

  /// Retrieve the number of processes.
  static int getNbProc() { return nbProc; }

 private:
  /// Rank of the process.
  static int rank;
  /// Number of processes.
  static int nbProc;
  /// Boolean indicating if the interface is initialized.
  static bool initialized;
  /// Boolean indicating if the parallel mode is on or not.
  static bool parallel;
};


} // Namespace flowvr

#endif
