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
* File: include/flowvr/plugd/class.h                              *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_CLASS_H
#define FLOWVR_PLUGD_CLASS_H

#include "flowvr/common.h"

namespace flowvr
{

namespace plugd
{

class Object;

/// Class of (daemon) objects.
///
/// Each class is designated by a (unique) name.
///
/// This name must be the fully specified c++ name (using the namespace
/// hierarchy), but with the same syntax as java class names
/// (ex: flowvr.plugd.Object).
///
/// When a class is created it must be registered using the registerClass
/// method.
///
/// To obtain the reference to a Class given its name call the static find
/// method.
///
/// A Class must implement:
/// - the name method which retreive this class's name.
/// - the xmlDescription method which construct the XML description of the
///   class.
/// - the construct method which create a instance of this class given an
///   object ID and a xml document containing construction parameters.
class Class
{
 public:

  /// Virtual destructor.
  virtual ~Class() {}

  /// Check if a classname is valid. A class name should only contain
  /// letters, digits, dots and underscores
  static bool isValidName(std::string name);

  /// Register this class (must be called after construction of a new class).
  /// Returns false if a Class already registered with the same name.
  virtual bool registerClass();

  /// Fully qualified name of the Class. Should correspond to the c++
  /// namespace-based class name with '::' replaced by '.'
  virtual std::string name()=0;

  /// Test if this class derive from another class.
  virtual bool derive(std::string baseclassname)=0;

  /// Retreive the XML description of the Class.
  virtual std::string xmlDescription()=0;

  /// Construct a new object from this class with the given object ID.
  virtual Object* construct(std::string objID)=0;

  /// Find a Class with the given name. If not found try to load a plugin
  /// corresponding to this name.
  static Class* find(std::string classname);

  static void purgeRegistry();

};

} // namespace plugd

} // namespace flowvr

#endif
