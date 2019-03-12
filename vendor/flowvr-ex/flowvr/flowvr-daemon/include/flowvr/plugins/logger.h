/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
* File: ./include/flowvr/plugins/logger.h                         *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_LOGGER_H
#define FLOWVR_PLUGINS_LOGGER_H

#include "flowvr/plugd/object.h"


namespace flowvr
{

namespace plugins
{

/// Hold the stat data to send to the controller.
class Logger : public flowvr::plugd::Object
{
public:

  /// Constructor.
  Logger(const std::string &objID)
    : Object(objID)
    {}

  /// Return the buffer containing the description of the memory holding the data
  virtual BufferWrite getLogInfo()=0;

};

} // namespace plugins

} // namespace flowvr

#endif
