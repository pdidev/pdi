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
* File: include/flowvr/utils/backtrace.h                          *
*                                                                 *
* Contacts:                                                       *
*  02/05/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_BACKTRACE_H
#define FLOWVR_UTILS_BACKTRACE_H
 
#include <signal.h>
#ifdef FLOWVR_HAVE_EXECINFO_H
#include <execinfo.h>
#include <unistd.h>
#endif
#include <stdio.h>

namespace flowvr
{

	namespace utils
	{

	inline void sig_backtrace(int sig)
	{
		printf("\n########## SIGNAL %d ##########\n", sig);

	#ifdef FLOWVR_HAVE_EXECINFO_H
		void *array[128];
		int size;
		//char** symbols;

		size = backtrace(array, sizeof(array) / sizeof(array[0]));

		if (size > 0)
		{
			backtrace_symbols_fd(array, size, STDERR_FILENO);
			//symbols = backtrace_symbols(array, size);
			//if (symbols) {
			//  for (int i = size - 1; i >= 0; --i) {
			//                puts(symbols[i]); putc('\n');
			//            }
			//        }
			//    }
		}
	#endif

		signal(sig, SIG_DFL);
		raise(sig);
	}

	inline void autobacktrace()
	{
		signal(SIGSEGV, sig_backtrace);
		signal(SIGILL, sig_backtrace);
		signal(SIGFPE, sig_backtrace);
		signal(SIGPIPE, sig_backtrace);
		signal(SIGINT, sig_backtrace);
		signal(SIGTERM, sig_backtrace);
	}


	void stacktrace();
}

}

#endif

