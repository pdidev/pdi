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
* File: src/plugd/class.cpp                                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/config.h"
#include "flowvr/daemon.h"
#include "flowvr/plugd/class.h"
#include "flowvr/plugd/object.h"
#include "flowvr/utils/filepath.h"

#include <iostream>
#include <map>
#include <dlfcn.h>

namespace flowvr
{

namespace plugd
{

/// Check if a classname is valid. A class name should only contain
/// letters, digits, dots and underscores
bool Class::isValidName(std::string name)
{
  if (name.size()==0)
    return false; // a name can't be empty
  for (unsigned int i=0;i<name.size();i++)
  {
    char c = name[i];
    if (!(c>='a' && c<='z') && !(c>='A' && c<='Z') && !(c>='0' && c<='9')
	&& c!='_' && c!='.')
      return false; // bad char
  }
  return true;
}

// Class Registry: name->Class associative container

typedef std::map<std::string,Class*> ClassRegistry;

//ClassRegistry classRegistry;

static ClassRegistry* getClassRegistry()
{
  static ClassRegistry classRegistry;
  return &classRegistry;
}

void Class::purgeRegistry()
{
	for( ClassRegistry::iterator it = getClassRegistry()->begin(); it != getClassRegistry()->end(); ++it)
		delete (*it).second;

	getClassRegistry()->clear();

}
/// Register this class (must be called after construction of a new class).
/// Returns false if a Class already registered with the same name.
bool Class::registerClass()
{
  const std::string classname = name();
  if (flowvr::daemon::verboseLevel>=1)
	  std::cout << "flowvr::plugd: registering Class " << classname << std::endl;

  if (!isValidName(classname))
  {
    std::cerr << "Invalid class name " << classname << std::endl;
    return false;
  }
  if (!(getClassRegistry()->insert(make_pair(classname,this)).second))
  {
    std::cerr<< "flowvr::plugd: A Class named " << classname
	         << " already exists." << std::endl;
    return false;
  }
  return true;
}

/// Find a Class with the given name. If not found try to load a plugin
/// corresponding to this name.
Class* Class::find(std::string classname)
{
  if (!isValidName(classname))
  {
    std::cerr<<"flowvr::plugd: Invalid class name "<<classname<<std::endl;
    return NULL;
  }

  // look in the static class registry
  ClassRegistry::const_iterator it = getClassRegistry()->find(classname);
  // found one?
  if (it != getClassRegistry()->end())
    return it->second; // class found

  // if not found we try to dynamically load a plugin
  std::string plugname = classname;
#define FLOWVR_STR(f) #f
#define FLOWVR_STR2(f) FLOWVR_STR(f)

#ifdef FLOWVR_SUFFIX
  plugname.append(FLOWVR_STR2(FLOWVR_SUFFIX));
#endif
 
  plugname.append(FLOWVR_STR2(FLOWVR_SO_SUFFIX));
  std::string resultingPath;
  flowvr::utils::FilePath libpath("lib/flowvr/plugins", "FLOWVR_PLUGIN_PATH", resultingPath);
  std::cout << "FilePath resolved: [ " << resultingPath << "] for FLOWVR_DATA_PATH" << std::endl;
  if (!libpath.findFile(plugname))
  {
    std::cerr << "flowvr::plugd: Error loading "
              << plugname << ": "
              << dlerror() << std::endl;
    return NULL;
  }
#undef FLOWVR_STR
#undef FLOWVR_STR2

  if (flowvr::daemon::verboseLevel>=1)
    std::cout << "flowvr::plugd: Class "
              << classname
	          << " not found, trying to load plugin "
	          << plugname << std::endl;

  void *handle = dlopen(plugname.c_str(), RTLD_LAZY);

  if (!handle)
  {
    std::cerr << "flowvr::plugd: Error loading "
              << plugname  << ": "
              << dlerror() << std::endl;
    return NULL;
  }

  // search a registration (the global initializers may have added
  // a class during the call to dlopen()
  it = getClassRegistry()->find(classname);
  if (it != getClassRegistry()->end())
  {
    if (flowvr::daemon::verboseLevel>=1)
      std::cout << "flowvr::plugd: Plugin "
                << plugname << " loaded successfully"
		        << std::endl;
    return it->second; // class found
  }
  std::cerr << "flowvr::plugd: Plugin "
            << plugname  << " failed to load class "
	        << classname << std::endl;

  // gracefully close the so
  dlclose(handle);
  return NULL;
}

} // namespace plugd

} // namespace flowvr
