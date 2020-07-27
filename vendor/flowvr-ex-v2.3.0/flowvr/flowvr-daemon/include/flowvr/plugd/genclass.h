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
* File: include/flowvr/plugd/genclass.h                           *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_GENCLASS_H
#define FLOWVR_PLUGD_GENCLASS_H

#include "flowvr/plugd/class.h"

namespace flowvr
{

namespace plugd
{

/// Template utility class to easily register plugins base abstract classes
template<class Obj>
class AbsClass : public Class
{
public:
  std::string Name;
  std::string Description;
  Class* BaseClass;
  std::string BaseClassName;

  AbsClass(std::string _name,
	   std::string _description,
	   std::string _baseclassname="flowvr.plugd.Object")
    : Name(_name), Description(_description), BaseClass(NULL), BaseClassName(_baseclassname)
  {
    registerClass();
  }

  AbsClass(std::string _name,
	   std::string _description,
	   Class* _baseclass)
    : Name(_name), Description(_description), BaseClass(_baseclass)
  {
    registerClass();
  }

  virtual ~AbsClass()
  {
  }
  
  /// Fully qualified name of the Class. Should correspond to the c++
  /// namespace-based class name with '::' replaced by '.'
  virtual std::string name()
  {
    return Name;
  }

  /// Retreive the XML description of the Class.
  virtual std::string xmlDescription()
  {
    return Description;
  }

  /// Construct a new object from this class with the given object ID.
  virtual flowvr::plugd::Object* construct(std::string objID)
  {
    return NULL; //new Obj(objID);
  }

  /// Test if this class derive from another class.
  virtual bool derive(std::string baseclassname)
  {
    if (baseclassname==Name) return true;
    if (BaseClass!=NULL) return BaseClass->derive(baseclassname);
    else return (baseclassname == BaseClassName);
  }

};

/// Template utility class to easily register plugins classes
template<class Obj>
class GenClass : public AbsClass<Obj>
{
public:

  GenClass(std::string _name,
	   std::string _description,
	   std::string _baseclassname="flowvr.plugd.Object")
    : AbsClass<Obj>(_name,_description,_baseclassname)
  {
  }

  GenClass(std::string _name,
	   std::string _description,
	   Class* _baseclass)
    : AbsClass<Obj>(_name,_description,_baseclass)
  {
  }

  /// Construct a new object from this class with the given object ID.
  virtual flowvr::plugd::Object* construct(std::string objID)
  {
    return new Obj(objID);
  }

  Obj* constructT(std::string objID)
  {
    return (Obj*)construct(objID);
  }

};


} // namespace plugd

} // namespace flowvr

#endif
