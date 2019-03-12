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
* File: include/flowvr/utils/hexdump.h                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_TIMING_H
#define FLOWVR_UTILS_TIMING_H


namespace flowvr
{

	namespace utils
	{
		typedef double microtime;   // resolution nano, unit s
		                            // integer part: s, fraction: ms
									// secs since epoch

		/**
		 * gets a micro-timestamp with nano-resolution using CLOCK_REALTIME
		 * on linux systems. Note that this clock value is valid on the local
		 * machine only. The fall-back time-stamp detected will be taken from
		 * what getNTPTimeStamp() will provide (gettimeofday).
		 * microtimes are in seconds (integer part) and ms (fractional part).
		 * @see getNTPTimeStamp()
		 */
		microtime getMicroStamp();

		/**
		 * returns a microtime value from the gettimeofday backend, which in
		 * an ideal case is reflecting the NTP value for the current system.
		 * Note that this property is determined on the system settings of
		 * your machine, and not within the scope of flowvr.
		 * @return a timestamp in granularity as described by microstamp
		 * @see getMicroStamp()
		 */
		microtime getNtpTimeStamp();


		/**
		 * wait (idle) for the given number of msecs. Uses select() on an empty
		 * file handle set with a given timeout. If msecs given is less than 1000,
		 * the method uses usleep, for more precise, but maybe busy-polling waiting.
		 * @param msecs the number of milliseconds to sleep
		 */
		void microsleep( int msecs );
	}
}

#endif

