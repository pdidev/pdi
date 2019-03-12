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
* File: include/flowvr/plugd/object.h                             *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_OBJECT_H
#define FLOWVR_PLUGD_OBJECT_H

#include "flowvr/plugd/class.h"
#include "flowvr/xml.h"
#include "flowvr/trace.h"

#include <vector>

namespace flowvr
{

namespace plugd
{

class ActionHandler;
class Dispatcher;

/// Class describing the (XML) result of a method (Object::init, Object::doAction).
class Result
{
 public:
  enum StatusType
  {
    OK = 0,
    ERROR,
  };

  Result(StatusType s = OK, xml::DOMNode* data=NULL) : status(s), xmldata(data) {}
  Result(StatusType s, const char* text)
    : status(s)
  {
    xmldata = new xml::DOMText(text);
  }
  Result(StatusType s, const std::string& text)
    : status(s)
  {
    xmldata = new xml::DOMText(text);
  }
  Result(xml::DOMElement* root)
    : status(OK)
  {
    const char* s = root->Attribute("status");
    if (s!=NULL && strcmp(s,"OK"))
      status = ERROR;
    xmldata = root->FirstChildElement();
    if (xmldata != NULL) xmldata = xmldata->Clone();
  }

  StatusType getStatus() const { return status; }
  void setStatus(StatusType s) { status=s; }

  xml::DOMNode* getXML() const { return xmldata; }
  void setXML(xml::DOMNode* doc) { xmldata = doc; }

  bool error() { return status!=OK; }

  void clear()
  {
    if (xmldata!=NULL) delete xmldata;
    status = OK;
    xmldata = NULL;
  }

  std::string toString();

 protected:
  StatusType status;
  xml::DOMNode* xmldata;
};

/// Object instances.
///
/// Each instance is designated by a unique objectID.
/// It can execute actions using two different modes:
/// - immediate mode: the doAction method takes a xml description of the
///   action and immediately execute it, returning an xml description of
///   the result
/// - batch mode: This mode is used when an action is used over and over,
///   as it is the case when it is used for each received message of a
///   given source. This is done by first using the createAction method
///   which takes the xml description of the action and create an
///   ActionHandler (or return NULL if the action is not supported). Then
///   the do method if the ActionHandler is used each time the action must
///   be executed.

class Object
{

protected:
 /// Virtual Destructor.
 virtual ~Object();


public:

  /// Constructor.
  Object(const std::string &objID);

  /// Initialization. Returns a XML document containing the result.
  virtual Result init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);

  /// Destruction. Returns a XML document containing the result.
  virtual Result close(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);

 public:
  /// Get the class of this object
  virtual Class* getClass() const = 0;

  /// Execute an action in immediate mode.
  virtual Result doAction(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);

  /// Create an ActionHandler for batch mode action execution.
  virtual ActionHandler* createAction(xml::DOMElement* xmlRoot) = 0;

  /// Retreive this object's ID
  const std::string& objectID() const { return ObjectID; }

  std::string parentID() const
  {
    return std::string(ObjectID,0,ObjectID.find_last_of("/"));
  }

  std::string name() const
  {
    return std::string(ObjectID,ObjectID.find_last_of("/")+1);
  }

  /// Create a one-line description of the current state
  virtual std::string status() const;

  /// Find an object given its ID
  static Object* find(const std::string &objID);
  
  /// Find an object given its ID, possibly relative to this object.
  /// If the given ID starts with a '/' then it is an absolute ID
  /// else the given ID is searched relative to this object
  /// and if not found recursively to its parent.
  Object* findRelative(const std::string &objID);

  /// Unregister an object from the registry
  void unregister();

  /// Build the list of object based  on the id mask
  static std::vector<std::string> getListObject(std::string id);


  virtual BufferWrite alloc(size_t size);
  virtual Allocator*  getAllocator() const;
 protected:

  const std::string ObjectID; ///< Object ID
  std::vector<Trace*> outputTraces; ///< Traces implemented by this object

 };

} // namespace plugd

} // namespace flowvr

#endif
