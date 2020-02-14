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
* File: include/flowvr/portreader.h                               *
*                                                                 *
* Contacts:                                                       *
*  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PORTREADER_H
#define FLOWVR_PORTREADER_H

#include "flowvr/message.h"

#include <string>

namespace flowvr
{

class PortReader
{
public:
  std::string filename;
  int fd;
  int fnbp;
  pid_t pid; ///< external uncompress process

  PortReader(const std::string& file);
  ~PortReader();

  bool init(flowvr::StampList* stamps=NULL);
  void close();

  bool read(flowvr::MessageWrite& msg, flowvr::Allocator* alloc);

protected:
  bool file_read(void* buf, size_t count);
  bool pipe_open(const char* progname);
};

} // namespace flowvr

#endif
