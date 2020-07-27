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
* File: src/portwriter.cpp                                        *
*                                                                 *
* Contacts:                                                       *
*  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/portwriter.h"

#include <iostream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

namespace flowvr
{

PortWriter::PortWriter(const std::string& file, bool _raw)
: filename(file), fd(-1), pid(0), raw(_raw)
{
}

PortWriter::~PortWriter()
{
  close();
}

bool PortWriter::file_write(const void* buf, size_t count)
{
  if (fd==-1) return false;
  if (::write(fd, buf, count)==-1)
  {
    std::cerr << "Error writing to "<<filename<<std::endl;
    close();
    return false;
  }
  return true;
}

bool PortWriter::pipe_open(const char* progname)
{
  int fds[2];
  pid_t   pid;
  if (pipe(fds))
  {
    std::cerr << "pipe failed."<<std::endl;
    return false;
  }
  // this line prevent future forked processes to keep this pipe open forever.
  fcntl (fds[1], F_SETFD, FD_CLOEXEC);
  pid = fork ();
  if (pid < 0)
  {
    std::cerr << "fork failed."<<std::endl;
    return false;
  }
  else if (pid == 0)
  { // child process
    ::close(fds[1]);
    // Redirect standard input to pipe
    dup2(fds[0],0);
    // Redirect standard output to compressed file
    dup2(fd,1);
    dup2(open("/dev/null",O_WRONLY),2);
    int retexec = execlp(progname, progname, NULL); //shell,shell,"-c",progname,NULL);
    std::cerr << "ERROR: execlp("<<progname<<") returned "<<retexec<<std::endl;
    exit(1);
  }
  else
  { // parent process
    char buf[256];
    int n;
    ::close(fds[0]);

    ::close(fd); // we don't read from the compressed file
    fd = fds[1]; // but from the pipe
    this->pid = pid; // save the child process id
    //waitpid(pid,NULL,0);
  }
  return true;
}

bool PortWriter::init(flowvr::StampList* stamps)
{
  fd = open(filename.c_str(),O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
  //fd = creat(filename.c_str(), S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
  if (fd==-1)
  {
    int err = errno;
    std::cerr << "Creating file "<<filename<<" failed (error "<<err<<": "<<strerror(err)<<")."<<std::endl;
    return false;
  }

  // compression support
  size_t tmp = 0;
  tmp = filename.rfind(std::string("."), filename.size()-1);

  if(tmp != std::string::npos){
	  std::string ext(filename, tmp);

	  if (ext == ".gz")
	  {
		std::cout << "Compressing file "<<filename<<" with gzip."<<std::endl;
		if (!pipe_open("gzip")) return false;
	  }
	  else if (ext == ".bz2")
	  {
		std::cout << "Compressing file "<<filename<<" with bzip2."<<std::endl;
		if (!pipe_open("bzip2")) return false;
	  }
  }
  if (!raw)
  {
    file_write("FlowVRdump",10);
    int fnbp=1; file_write(&fnbp,sizeof(int));
    if (stamps==NULL)
    {
      int ssize = 0;
      file_write(&ssize,sizeof(int));
    }
    else
    {
      flowvr::xml::DOMNode* xmlstamps = stamps->generateXML();
      std::string str = flowvr::xml::DOMWriter::toString(xmlstamps);
      //std::cout<<str<<std::endl;
      delete xmlstamps;
      int ssize = str.size();
      file_write(&ssize,sizeof(int));
      file_write(str.c_str(),ssize);
    }
  }
  return true;
}

void PortWriter::close()
{
  if (fd!=-1)
  {
    ::close(fd);
    fd=-1;
  }
  if (pid > 0)
  {
    std::cout << "waiting for compress process to finish..."<<std::endl;
    waitpid(pid,NULL,0);
    std::cout << "compress process stopped."<<std::endl;
    pid=0;
  }
}

bool PortWriter::write(flowvr::Message msg)
{
  if (fd==-1) return false;
  if (!raw)
  {
    int ssize, dsize;
    ssize=msg.stamps.getSize(); file_write(&ssize,sizeof(int));
    dsize=msg.data.getSize(); file_write(&dsize,sizeof(int));
    file_write(msg.stamps.readAccess(),msg.stamps.getSize());
  }
  file_write(msg.data.readAccess(),msg.data.getSize());
  return true;
}

} // namespace flowvr
