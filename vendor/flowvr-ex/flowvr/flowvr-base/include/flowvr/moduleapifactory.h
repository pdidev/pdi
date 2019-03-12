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
* File: include/flowvr/moduleapifactory.h                         *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MODULEAPIFACTORY_H
#define FLOWVR_MODULEAPIFACTORY_H

#include <string>

namespace flowvr
{

class ModuleAPI;
class StampsWrite;

/**
 * @brief factory to create a module API with a matching memory model for the daemon.
 *
 * The factory is typically used by ModuleAPI to register a new module with
 * a local daemon. Normal users should not need to access this interface.
 */
class ModuleAPIFactory
{
 public:

  /**
   *  @brief registers a normal module with the daemon.
   *
   *  This method uses three environment variables to function
   *  - FLOWVR_DAEMON: determines daemon to use (a number indicates
   *    to use a shared memory segment with the number as shm ID),
   *    an URL prefixed with 'file:' determines to use the file based
   *    module API.
   *  - FLOWVR_PARENT: the parent process that launched the module.
   *    Is passed as is to the constructor for the ModuleAPI used.
   *  - FLOWVR_MODNAME: see modulename parameter
   *
   *  @param instancename Name of this instance (leave blank for single instance module).
   *         This is a postfix to the given module name and is used to differentiate
   *         between two modules runnning on the same host. Can be the empty string.
   *         If not empty, the instancename is used as a postfix to the modulename.
   *         Typically, this string is of sort "0" or "1" to indicate the rank.
   *         When given, the name is attached using a slash.
   *
   *
   *  @param modulename the module name (postfix of module ID)
   *         When this string is empty, its value is resolved by the environment
   *         variable FLOWVR_MODNAME
   */
  static ModuleAPI* registerModule(const std::string &instancename=std::string(""),
		                           const std::string &modulename = std::string("") );

  /**
   *  @brief choose the appropriate ModuleAPI implementation for a new controller.
   *
   * Currently, controllers can only be used in conjunction with a shared memory
   * daemon. This is a system internal method, so normal users should not use it.
   */
  static ModuleAPI* registerController(const std::string &instancename=std::string(""),
		                               const std::string &hostnameIP= std::string("gethostname"));

 private:
  /// Private constructor: no instantiation possible
  ModuleAPIFactory();

}; // class ModuleAPIFactory

} // namespace flowvr

#endif
