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
* File: src/portreader.cpp                                        *
*                                                                 *
* Contacts:                                                       *
*  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/portreader.h"

#include <iostream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>

namespace flowvr
{

PortReader::PortReader(const std::string& file)
: filename(file), fd(-1), fnbp(0), pid(0)
{
}

PortReader::~PortReader()
{
  close();
}

bool PortReader::file_read(void* buf, size_t count)
{
  ssize_t n;
  if (fd==-1) return false;
  while (count>0 && (n=::read(fd,buf,count))>0)
  {
    count-=n;
    buf=((char*)buf)+n;
  }
  if (count==0) return true;
  std::cerr<<"Unexpected end of file "<<filename<<std::endl;
  close();
  return false;
}
#define file_read_err(buf,n) if (!file_read(buf,n)) return false; else


bool PortReader::pipe_open(const char* progname)
{
  int fds[2];
  pid_t   pid;
  if (pipe(fds))
  {
    std::cerr << "pipe failed."<<std::endl;
    return false;
  }
  pid = fork ();
  if (pid < 0)
  {
    std::cerr << "fork failed."<<std::endl;
    return false;
  }
  else if (pid == 0)
  { // child process
    ::close(fds[0]);
    // Redirect standard input to compressed file
    dup2(fd,0);
    // Redirect standard output to pipe
    dup2(fds[1],1);
    int retexec = execlp(progname, progname, NULL); //shell,shell,"-c",progname,NULL);
    std::cerr << "ERROR: execlp("<<progname<<") returned "<<retexec<<std::endl;
    exit(1);
  }
  else
  { // parent process
    char buf[256];
    int n;
    ::close(fds[1]);

    ::close(fd); // we don't read from the compressed file
    fd = fds[0]; // but from the pipe
    this->pid = pid; // save the child process id
    //waitpid(pid,NULL,0);
  }
  return true;
}

bool PortReader::init(flowvr::StampList* stamps)
{
  fd = open(filename.c_str(),O_RDONLY);
  if (fd==-1)
  {
    int err = errno;
    std::cerr << "Opening file "<<filename<<" failed (error "<<err<<": "<<strerror(err)<<")."<<std::endl;
    return false;
  }

  // compression support

  std::string ext(filename, filename.rfind("."));
  if (ext == ".gz")
  {
    std::cout << "Uncompressing file "<<filename<<" with gunzip."<<std::endl;
    if (!pipe_open("gunzip")) return false;
  }
  else if (ext == ".bz2")
  {
    std::cout << "Uncompressing file "<<filename<<" with bunzip2."<<std::endl;
    if (!pipe_open("bunzip2")) return false;
  }

  
  char buf[10];
  file_read_err(buf,10);
  if (strncmp(buf,"FlowVRdump",10))
  {
    std::cerr << "File "<<filename<<" is not a FlowVR dump file."<<std::endl;
    close();
    return false;
  }
  file_read_err(&fnbp,sizeof(int));
  if (fnbp<1)
  {
    std::cerr << "File "<<filename<<" contains an invalid number of ports ("<<fnbp<<")."<<std::endl;
    close();
    return false;
  }
  {
    int ssize=0;
    file_read_err(&ssize,sizeof(int));
    if (ssize>0)
    {
      char xmlstamps[ssize+1];
      file_read_err(xmlstamps,ssize);
      xmlstamps[ssize]='\0';
      if (stamps!=NULL)
      {
	//std::cout<<"File "<<filename<<" stamps:"<<xmlstamps<<std::endl;
	flowvr::xml::DOMParser parse;
	parse.parseString(xmlstamps);
	stamps->updateFromXML(parse.getDocument()->RootElement());
      }
    }
  }
  // Skip other ports in file
  for (int p=1;p<fnbp;p++)
  {
    int ssize=0;
    file_read_err(&ssize,sizeof(int));
    if (ssize>0)
      lseek(fd,ssize,SEEK_CUR);
  }
  return true;
}

void PortReader::close()
{
  if (fd!=-1)
  {
    ::close(fd);
    fd=-1;
  }
  if (pid > 0)
  {
    waitpid(pid,NULL,0);
    pid=0;
  }
}

bool PortReader::read(flowvr::MessageWrite& msg, flowvr::Allocator* alloc)
{
  if (fd==-1) return false;
  {
    int ssize=0, dsize=0;
    file_read_err(&ssize,sizeof(int));
    file_read_err(&dsize,sizeof(int));
    msg.stamps = alloc->alloc(ssize);
    msg.data = alloc->alloc(dsize);
    file_read_err(msg.stamps.writeAccess(),msg.stamps.getSize());
    file_read_err(msg.data.writeAccess(),msg.data.getSize());
  }
  for (int p=1;p<fnbp;p++)
  {
    int ssize=0, dsize=0;
    file_read_err(&ssize,sizeof(int));
    file_read_err(&dsize,sizeof(int));
    if (ssize+dsize>0)
      lseek(fd,dsize+ssize,SEEK_CUR);
  }
  return true;
}

} // namespace flowvr
