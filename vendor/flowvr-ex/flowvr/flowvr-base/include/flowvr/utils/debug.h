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
* File: include/flowvr/utils/debug.h                              *
*                                                                 *
* Contacts:                                                       *
*  02/05/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_DEBUG_H
#define FLOWVR_UTILS_DEBUG_H
 
#include <signal.h>
#ifdef FLOWVR_HAVE_EXECINFO_H
#include <execinfo.h>
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>

namespace flowvr
{

namespace utils
{

const char* program = NULL;

int debug()
{
#ifdef FLOWVR_HAVE_EXECINFO_H
  // start debugger (code inspired by PETSc).
  int child = (int)fork(); 
  if (child < 0)
  {
    printf("Error in fork() attaching debugger\n");
    return 0;
  }
  else if (!child)
  { // I am the child, will run the debugger
    const char *args[10];
    char pid[10];
    sprintf(pid,"%d",(int)getppid()); 

    if (getenv("DISPLAY")==NULL)
    { // console mode
      args[0] = "gdb";
      args[1] = program;
      args[2] = pid;
      args[3] = 0;
      printf("Attaching gdb to %s of pid %s\n",program,pid);
      if (execvp(args[0],(char**)args) < 0)
      {
        perror("Unable to start debugger");
        exit(1);
      }
    }
    else
    {
      args[0] = "xterm";  args[1] = "-e"; 
      args[2] = "gdb";    args[3] = program; 
      args[4] = pid;      args[5] = 0;
      printf("Attaching gdb in xterm to %s on pid %s\n",program,pid);
      if (execvp("xterm",(char**)args)  < 0)
      {
        perror("Unable to start debugger in xterm");
        exit(1);
      }
    }
    return 0; // should be unreachable...
  }
  else
  { // I am the parent, continue with user code
    sleep(10); // wait for debugger
    return 1;
  }
#else
  printf("debug autostart not supported on this system\n");
  return 0;
#endif
}

void sig_debug(int sig)
{
  printf("\n########## SIGNAL %d ##########\n",sig);

#ifdef FLOWVR_HAVE_EXECINFO_H
  void *array[128];
  int size;
  //char** symbols;
  size = backtrace(array, sizeof(array) / sizeof(array[0]));
  if (size > 0)
  {
    backtrace_symbols_fd(array, size, STDERR_FILENO);
  }
#endif

  signal(sig,SIG_DFL);
  debug();
  return; //raise(sig);
}

void autodebug(const char* programname)
{
  program = programname;
  signal(SIGSEGV, sig_debug);
  signal(SIGILL, sig_debug);
  signal(SIGFPE, sig_debug);
  signal(SIGPIPE, sig_debug);
  signal(SIGINT, sig_debug);
  signal(SIGTERM, sig_debug);
}

} // namespace utils

} // namespace flowvr

#endif

