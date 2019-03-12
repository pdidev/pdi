/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                                                                 *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
*  Contact :                                                      *
*                                                                 *
******************************************************************/

#ifndef MTBARRIER_H_
#define MTBARRIER_H_

#include <pthread.h>
#include <exception>

namespace flowvr
{
	namespace ipc
	{
		class MTBarrier
		{
		public:
			MTBarrier(int nCount)
			{
				pthread_barrierattr_init(&m_attr);
				pthread_barrierattr_setpshared( &m_attr, PTHREAD_PROCESS_PRIVATE );

				if( pthread_barrier_init( &m_barrier, &m_attr, nCount ) )
					throw std::exception();
			}

			~MTBarrier()
			{
				pthread_barrier_destroy( &m_barrier );
				pthread_barrierattr_destroy( &m_attr );
			}

			int wait()
			{
				return pthread_barrier_wait( &m_barrier );
			}

		private:
			pthread_barrier_t m_barrier;
			pthread_barrierattr_t m_attr;
		};
	}
}


#endif // MTBARRIER_H_
