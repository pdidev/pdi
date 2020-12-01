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
* File: src/plugd/object.cpp                                      *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/daemon.h>
#include <flowvr/plugd/class.h>
#include <flowvr/plugd/object.h>
#include <flowvr/xml.h>
#include <flowvr/ipc/mtlock.h>
#include <flowvr/ipc/locker.h>

#include <flowvr/mem/sharedmemorymanager.h>

#include <iostream>
#include <sstream>
#include <map>
#include <dlfcn.h>

namespace flowvr
{

namespace plugd
{

using namespace flowvr::xml;

std::string Result::toString()
{
  std::ostringstream sout;
  sout << "<result status=\"";
  switch (status)
  {
  case OK: sout << "OK"; break;
  case ERROR: sout << "ERROR"; break;
  }
  if (xmldata!=NULL)
  {
    sout << "\">";
    sout << *xmldata;
    sout << "<result/>";
  }
  else
    sout << "\"/>";
  std::string str = sout.str();
  return str;
}

/// Constructor.
Object::Object(const std::string &objID)
  : ObjectID(objID)
{
}

/// Virtual Destructor.
Object::~Object()
{
}

// Object Registry: name->Object associative container
typedef std::map<std::string,Object*> ObjectRegistry;
// put it in the anonymous namespace to hinder external
// linking visibility
namespace
{
	// that's effectively a singleton, which can be accessed by
	// objects living in different threads,
	// so accessing it needs locking
	ObjectRegistry objectRegistry;

	// when using the lock: make sure, there is no nested
	// call trying to acquire it: this will deadlock on
	// most pthread implementations.
	flowvr::ipc::MTLock registryLock("objectRegistryLock");
}


void Object::unregister()
{
  flowvr::ipc::ScopedMTLock l(registryLock);
  ObjectRegistry::iterator it = objectRegistry.find(objectID());
  if (it != objectRegistry.end() && (it->second == this))
  {
    objectRegistry.erase(it);
  }
}

std::vector<std::string> Object::getListObject(std::string id)
{
  flowvr::ipc::ScopedMTLock l(registryLock);
  ObjectRegistry::iterator it = objectRegistry.begin();
  std::vector<std::string> listobject;
  //test all the objects registered with the id attribute given in the tag "action"
  while (it!=objectRegistry.end())
    {
      Object* object = it->second;
      std::string rep = object->objectID();
      //if the objectID contains the ID of the command
      if (!strncmp(id.c_str(),rep.c_str(),id.size()))
      {
    	  listobject.push_back(rep);
      }
      it++;
    }

  return listobject;

}

/// Destruction.
Result Object::close(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
{
  flowvr::ipc::ScopedMTLock l(registryLock);
  Result res;
  ObjectRegistry::iterator it = objectRegistry.find(objectID());
  if (it == objectRegistry.end())
  {
    res = Result(Result::OK,"flowvr::plugd: Destruction of Object "+objectID()+" not registered.");
  }
  else if (it->second != this)
  {
    res = Result(Result::OK,"flowvr::plugd: Duplicate Object ID "+objectID()+" found during destruction.");
  }
  else
  { // unregister the object
    res = Result(Result::OK,"flowvr::plugd: Unregistering Object "+objectID());
    objectRegistry.erase(it);
  }
  //delete this;
  return res;
}

/// Initialization. Returns a XML document containing the result.
Result Object::init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
{
  flowvr::ipc::ScopedMTLock l(registryLock);
  if (flowvr::daemon::verboseLevel>=1)
    std::cout<<"flowvr::plugd: initializing Object "<<objectID()<<std::endl;
  if (!(objectRegistry.insert(make_pair(objectID(),this)).second))
  {
    std::string message = "flowvr::plugd: An Object named "+objectID()
      +" already exists.";
    std::cerr<<message<<std::endl;
    DOMDocument* xmldoc = new DOMDocument();
    xmldoc->LinkEndChild(new DOMElement("message"))->LinkEndChild(new DOMText(message));
    return Result(Result::ERROR,xmldoc);
    //return "<result status=\"error\"><message>"+message+"</message></result>";
  }
  return Result(Result::OK);
}

/// Find an object given its ID
Object* Object::find(const std::string &objID)
{
  flowvr::ipc::ScopedMTLock l(registryLock);

  ObjectRegistry::const_iterator it = objectRegistry.find(objID);
  if (it != objectRegistry.end())
    return it->second; // object found

  if (flowvr::daemon::verboseLevel>=1)
    std::cout << "flowvr::plugd: Object "<<objID<<" not found"<<std::endl;
  return NULL;
}

/// Find an object given its ID, possibly relative to this objet.
/// If the given ID starts with a '/' then it is an absolute ID
/// else the given ID is searched relative to this object
/// and if not found recursively to its parent.
Object* Object::findRelative(const std::string &objID)
{
  if (objID.empty()) return NULL;
  if (objID[0]=='/') return find(objID); // absolute ID
  std::string prefix = objectID();
  Object* result;
  while ((result = find(prefix+"/"+objID)) == NULL && !prefix.empty())
  {
    prefix = prefix.substr(0,prefix.find_last_of("/"));
  }
  return result;
}

std::string Object::status() const
{
  std::string s;
  const std::string& c = getClass()->name();
  std::string::size_type p = c.rfind('.');
  if (p == std::string::npos)
	  s = c;
  else
	  s = std::string(c,p+1);

  s+=' ';
  std::string o = objectID();
  for (int i=0;i<3;i++)
  {
    p = o.find('/',1);
    if (p == std::string::npos) break;
    o = std::string(o,p+1);
    s = ' ' + s;
  }
  s+=o;
  return s;
}

BufferWrite Object::alloc(size_t size)
{
	return getAllocator()->alloc(size);
}

Allocator* Object::getAllocator() const
{
	return Allocator::getAllocator();
}

Result Object::doAction( xml::DOMElement *, Dispatcher *dispatcher )
{
	return Result( Result::ERROR, "No action supported for immediate mode");
}

} // namespace plugd

} // namespace flowvr
