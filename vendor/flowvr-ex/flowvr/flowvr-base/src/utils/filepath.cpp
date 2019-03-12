/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
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
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./src/ext/filepath.cpp                                    *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <flowvr/utils/filepath.h>

#include <iostream>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

namespace flowvr
{

namespace utils
{

FilePath::FilePath(const char* envVar)
{
  if (envVar != NULL && envVar[0]!='\0')
  {
    const char* envpath = getenv(envVar);
    if (envpath != NULL && envpath[0]!='\0')
      vpath.push_back(envpath);
  }
}

FilePath::FilePath(const char * pathsuffix,
		           const char * envVar,
		           std::string &store,
		           const char * prefixVariable, const char * prefixPath)
{
  std::string envpath;
  if (pathsuffix && pathsuffix[0] != '\0')
  {
    envpath = (getenv(prefixVariable ? : "FLOWVR_PREFIX") ? : "");
    if (!envpath.empty())
    {
      if ((envpath[envpath.size()-1] != '/') && (pathsuffix[0] != '/'))
        envpath += "/";

      vpath.push_back(envpath + pathsuffix);

      store = (envpath+pathsuffix);
    }
    else
    {
      store = std::string("Not default prefix defined.");
    }
  }
  if (envVar != NULL && envVar[0] != '\0')
  {
    envpath = (getenv(envVar) ? : "");
    if (!envpath.empty()){
      vpath.push_back(envpath);
      store = envpath;
    }
  }
}

void FilePath::addPath(const std::string& path)
{
  vpath.push_back(path);
}

std::vector<std::string> FilePath::getAllPaths() const
{
	return vpath; // return by copy on purpose
}

bool FilePath::findFileInRep(const std::string& filename, const std::string& path, std::string& rep)
{
  if (filename.empty()) return false; // no filename
  struct stat s;
  size_t p0 = 0;
  size_t p1;
  while ( p0 < path.size() )
  {
    p1 = path.find(':',p0);
    if (p1 == std::string::npos) p1 = path.size();
    if (p1>p0+1)
    {
      std::string newfname = path.substr(p0,p1-p0);
      if (*newfname.rbegin() != '/') newfname += '/';
      newfname += filename;
#if defined(DEBUG)
      std::cout << "Looking for " << newfname <<std::endl;
#endif
      if (!stat(newfname.c_str(),&s))
      {
	// File found
#if defined(DEBUG)
	std::cout << "File "<<filename<<" found in "<<path.substr(p0,p1-p0)<<std::endl;
#endif
	rep = path.substr(p0, p1-p0);
	return true;
      }
    }
    p0 = p1+1;
  }
  std::cerr << "\033[31m" << "File "<<filename<<" NOT found." << std::endl
		    << "path given: " << path << "\033[0m" << std::endl;
  return false;
}

bool FilePath::findFileIn(std::string& filename, const std::string& path)
{
  if (filename.empty())
	  return false; // no filename

  struct stat s;
	size_t p0 = 0;
	size_t p1;
	while (p0 < path.size())
	{
		p1 = path.find(':', p0);
		if (p1 == std::string::npos)
			p1 = path.size();
		if (p1 > p0 + 1)
		{
			std::string newfname = path.substr(p0, p1 - p0);
			if (*newfname.rbegin() != '/')
				newfname += '/';
			newfname += filename;
#if defined(DEBUG)
		      std::cout << "Looking for " << newfname <<std::endl;
#endif
			if (!stat(newfname.c_str(), &s))
			{
				// File found
#if defined(DEBUG)
				std::cout << "File " << filename << " found in "
						<< path.substr(p0, p1 - p0) << std::endl;
#endif
				filename = newfname;
				return true;
			}
		}
		p0 = p1 + 1;
	}
	std::cerr << "\033[31m" << "File " << filename << " NOT found. " << std::endl
			  << "path given: " << path << "\033[0m" << std::endl;
	return false;
}





bool FilePath::findFileRep(const std::string& filename, std::string& repertory)
{
  if (filename.empty())
	  return false; // no filename

  struct stat s;
  if (!stat(filename.c_str(),&s))
  {
	  repertory = "./"; return true;
  } // file found

  if (filename[0]=='/')
	  return false; // absolute file path
  if (filename.substr(0,2)=="./" || filename.substr(0,3)=="../")
	  return false; // absolute or local file path
  for (int i=vpath.size()-1;i>=0;i--)
    if (findFileInRep(filename, vpath[i], repertory))
    	return true;

  std::cerr << "\033[0;44m\033[1m" << "File "<<filename<<" NOT found in " << repertory << "\033[0m" << std::endl;
  return false;

}

bool FilePath::exists( const std::string &path2file ) const
{
	struct stat s;
	if (!stat(path2file.c_str(),&s))
	  return true; // file found

	return false;

}

bool FilePath::findFile(std::string& filename)
{
  if (filename.empty())
	  return false; // no filename

  struct stat s;
  if (!stat(filename.c_str(),&s))
	  return true; // file found

  if (filename[0]=='/')
	  return false; // absolute file path

  if (filename.substr(0,2)=="./" || filename.substr(0,3)=="../")
	  return false; // absolute or local file path

  for (int i=vpath.size()-1;i>=0;i--)
    if (findFileIn(filename, vpath[i]))
    	return true;

#if defined(DEBUG)
  std::cout << "File "<<filename<<" NOT FOUND"<<std::endl;
#endif
  return false;
}

void FilePath::print()
{
  for (int i=vpath.size()-1;i>=0;i--)
  {
    std::string path = vpath[i];
    size_t p0 = 0;
    size_t p1;
    while ( p0 < path.size() )
    {
      p1 = path.find(':',p0);
      if (p1 == std::string::npos) p1 = path.size();
      if (p1>p0+1)
      {
	std::cout << path.substr(p0,p1-p0) << std::endl;
      }
      p0 = p1+1;
    }
  }
}

} // namespace utils

} // namespace flowvr
