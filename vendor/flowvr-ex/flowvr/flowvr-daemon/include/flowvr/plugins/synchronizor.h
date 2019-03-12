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
* File: include/flowvr/plugins/synchronizor.h                     *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_SYNCHRONIZOR
#define FLOWVR_PLUGINS_SYNCHRONIZOR

#include "flowvr/plugins/baseobject.h"
#include "flowvr/plugd/genclass.h"

namespace flowvr
{

namespace plugins
{

/// Synchronizors base class.
///
/// A synchronizor does not necessary extend it but it is recommended.
class Synchronizor : public BaseObject
{
 public:

  /// Constructor
  Synchronizor(const std::string &objID);

  plugd::Result init(xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher);

  virtual plugd::Class* getClass() const;

 protected:
  virtual ~Synchronizor();

  int advance; ///< Number of order to send before waiting for the end of an iteration
  int nbOrder; ///< Number of order sent

  virtual void doStart(plugd::Dispatcher* dispatcher);

};

extern flowvr::plugd::AbsClass<Synchronizor> SynchronizorClass;

} // namespace plugins

} // namespace flowvr


#endif
