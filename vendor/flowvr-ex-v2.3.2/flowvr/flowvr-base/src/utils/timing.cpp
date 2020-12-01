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
* File: src/stamp.cpp                                             *
*                                                                 *
* Contacts:                                                       *
*  05/25/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/

#include <flowvr/utils/timing.h>

#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <sys/select.h>



namespace flowvr
{
	namespace utils
	{
		microtime getNtpTimeStamp()
		{
			struct timeval tv;
			if(gettimeofday(&tv, NULL) == -1)
				return 0.0;
//			tv.tv_secs = long(tv.tv_sec);
//			tv.tv_usecs = long(tv.tv_usec);
			return  ( (double)tv.tv_sec + ((double)tv.tv_usec / 1000000.0) );
		}

		microtime getMicroStamp()
		{
#if defined(__APPLE__)
                    return getNtpTimeStamp(); 
#else
		    struct timespec tv;
		    clock_gettime(CLOCK_REALTIME, &tv );

		    return  ( (double)tv.tv_sec + ((double)tv.tv_nsec / 1000000000.0) );
#endif
		}


		void microsleep( int mlsec )
		{
			if(mlsec < 1000)
				usleep(mlsec*1000);
			else
			{
				mlsec = mlsec * 1000;
				struct timeval tv;
				tv.tv_sec  = (mlsec != 0) ? (mlsec / 1000000) : 0;
				tv.tv_usec = (mlsec != 0) ? (mlsec % 1000000) : 0;
					// simply poll, but in microwait!
				select(0, NULL, NULL, NULL, ((tv.tv_sec!=0 || tv.tv_usec!=0) ? &tv : NULL));
			}
		}
	}
}

