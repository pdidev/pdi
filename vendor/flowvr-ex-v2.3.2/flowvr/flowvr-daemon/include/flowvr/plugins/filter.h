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
* File: include/flowvr/plugins/filter.h                           *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_FILTER
#define FLOWVR_PLUGINS_FILTER

#include "flowvr/plugins/baseobject.h"
#include "flowvr/plugd/genclass.h"

namespace flowvr
{

namespace plugins
{

/// \brief Filters base class.
///
/// A filter does not necessary extend it but it is recommended.
class Filter : public BaseObject
{
 public:

  /// Constructor
  Filter(const std::string &objID);

  virtual plugd::Class* getClass() const;

 protected:
  virtual ~Filter();

};

extern flowvr::plugd::AbsClass<Filter> FilterClass;

} // namespace plugins

} // namespace flowvr


#endif
