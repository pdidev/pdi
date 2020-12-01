/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2020                by                       *
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
* file: include/flowvr/utils/fkill.h                              *
*                                                                 *
* Contacts:                                                       *
*  02/08/2020 Bruno Raffin   <bruno.raffin@inria.fr               *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_FKILL_H
#define FLOWVR_UTILS_FKILL_H

namespace flowvr
{ 
   namespace utils
   {
       /// Log process PID (module or daemon)  in directory defined by env variable FLOWVR_PID_LOG_DIR (flowvr-kill read from there)
     void writePID(const std::string &name = std::string(""));
   }
}

#endif
