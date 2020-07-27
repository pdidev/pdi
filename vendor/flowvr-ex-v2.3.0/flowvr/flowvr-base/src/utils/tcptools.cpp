/******* PELuCOPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: tcptools.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include <flowvr/utils/tcptools.h>

#include <sstream>

#include <errno.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <signal.h>

#include <netinet/tcp.h>
#include <net/if.h>
#include <netdb.h>

#ifdef __APPLE__
#include <ifaddrs.h>
#endif

#include <arpa/inet.h>
#include <string.h> // strerror()


namespace flowvr
{
	namespace utils
	{

          // Use getifaddrs rather than ioctl to retreive the IP address
          // ioctl led to erros on mac.
          // getifaddrs is supposed to be portable. It works well on mac os x,  but
          // on linux it leads to some errors at runtime. 
#ifdef __APPLE__
		TcpTools::AddressVector::size_type TcpTools::getIPAddresses( AddressVector &storage )
		{

                    struct ifaddrs *if_addrs = NULL;
                    struct ifaddrs *if_addr = NULL;
                    void *tmp = NULL;
                    char buf[INET6_ADDRSTRLEN];
                    if (0 == getifaddrs(&if_addrs)) {    
                        for (if_addr = if_addrs; if_addr != NULL; if_addr = if_addr->ifa_next) {
                            
                            // Address
                            if (if_addr->ifa_addr->sa_family == AF_INET) {

                                if (0 != (if_addr->ifa_flags & IFF_LOOPBACK))
                                    {
#ifdef DEBUG
                                                    std::cout<<"TcpTools::getIPAddresses found IP address: loopback -> discard"<<std::endl;
#endif
                                        continue; // we do not store loopback
                                    }
                                
#ifdef DEBUG
                                std::cout<<"TcpTools::getIPAddresses found one IP address: "
                                         <<  inet_ntop(if_addr->ifa_addr->sa_family,
                                                       &((struct sockaddr_in *)if_addr->ifa_addr)->sin_addr,
                                                       buf,
                                                       sizeof(buf))
                                         <<std::endl;
#endif
                                //                                struct sockaddr_in* addr = (struct sockaddr_in*) (if_addr);
                                storage.push_back(* (struct sockaddr_in*) (if_addr));
                            } else {
                                // IPV6 IPaddres (AF_INET6
                                //                                struct sockaddr_in6 addr6 = &((struct sockaddr_in6 *)if_addr->ifa_addr)->sin6_addr;
                            }
                        }

                    }
                    freeifaddrs(if_addrs);
                    if_addrs = NULL;
#ifdef DEBUG                    
                    std::cout<<"TcpTools::getIPAddresses counted "<< storage.size()<<"IP addresses"<< std::endl;
#endif                    
                    return storage.size();
		}

#else //__APPLE__
 	TcpTools::AddressVector::size_type TcpTools::getIPAddresses( AddressVector &storage )
 		{
 			int sock = socket(AF_INET, SOCK_DGRAM, 0);
 			if( sock == -1 )
 				return 0;

 			struct ifconf ifc;
 			struct ifreq ifr[128];
 			ifc.ifc_buf = (caddr_t) ifr;
 			ifc.ifc_len = sizeof(ifr);

 			if (ioctl(sock, SIOCGIFCONF, &ifc) >= 0 && ifc.ifc_len > 0)
 			{
 				int nbif = ifc.ifc_len / sizeof(struct ifreq);

 				for (int i = 0; i < nbif; ++i)
 				{
 					if (ifr[i].ifr_addr.sa_family != AF_INET)
 						continue;

 					// convert to a socket address
 					struct sockaddr_in* addr = (struct sockaddr_in*) (&ifr[i].ifr_addr);

 					// test for loopback
 					if (((addr->sin_addr.s_addr) & 0xff) == 127)
 						continue; // we do not store loopback

 					storage.push_back(*addr);
 				}
 			}

 			// close dgram socket we opened above
 			::close(sock);

 			// return number of stored addresses
 			return storage.size();
 		}
 #endif
          std::string TcpTools::getNodeHostName()
		{
			char hostname[FLOWVR_OS_HOST_NAME_MAX];
			hostname[sizeof(hostname) - 1] = 0;
			if( gethostname(hostname, sizeof(hostname)-1) ) //JJ
				return std::string();

			return std::string(hostname);
		}
		
		std::string TcpTools::getStringFromAdress( const struct in_addr &addr )
		{
			char dst[ INET_ADDRSTRLEN ];
			if ( NULL == inet_ntop( AF_INET, &addr, dst, INET_ADDRSTRLEN ) )
				return std::string( "" );
			return std::string( dst );
		}
		std::string TcpTools::getStringFromAdress( const struct in6_addr &addr )
		{
			char dst[ INET6_ADDRSTRLEN ];
			if ( NULL == inet_ntop( AF_INET6, &addr, dst, INET6_ADDRSTRLEN ) )
				return std::string( "" );
			return std::string( dst );
		}

		std::string TcpTools::getHostFromAddress( const struct sockaddr_in &addr, bool bResolveIp )
		{
		  std::string hostip = inet_ntoa(addr.sin_addr);
		  int hostport = ntohs(addr.sin_port);

		  std::stringstream in;
		  if(!bResolveIp)
		  {
			  in << hostip << ":" << hostport;
		  }
		  else
		  {
			  in << "(" << getHostNameFromAddress(addr) << ") / " << hostip << ":" << hostport;
		  }

		  return in.str();
		}

		void TcpTools::createListenOnAllAddresses( sockaddr_in &storage, int port )
		{
			storage.sin_family = AF_INET;
			storage.sin_port = htons(port);
			storage.sin_addr.s_addr = INADDR_ANY;
		}

		void TcpTools::printAddress( const sockaddr_in &ip )
		{
			std::string hostname = getHostFromAddress(ip);
			std::cout << hostname << std::endl;
		}

		//////////////////////////////////////////////////////////////////////////////////////
		// createTcpServerSocket
		//////////////////////////////////////////////////////////////////////////////////////
		int TcpTools::createTcpServerSocket( sockaddr &address, bool bAllowReuse, int nBacklog, bool bAsync, int *error )
		{
			int sock = socket( AF_INET, SOCK_STREAM, 0 );
			if( sock == -1 )
			{
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw CreateSocketException(__FILE__, errno );
			}

			int reuse = (bAllowReuse ? 1 : 0);
			if( setsockopt( sock, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(int) ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetOptionException(__FILE__, "SO_REUSEADDR failed to set.", errno );
			}

			if( bind( sock, &address, sizeof( sockaddr ) ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw BindSocketException(__FILE__, errno );
			}

			if( listen( sock, nBacklog ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetListenStateException(__FILE__, errno );
			}

			int nonBlocking = bAsync ? 1:0;
			if( ioctl( sock, FIONBIO, &nonBlocking ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetOptionException( __FILE__, "Failed to set blocking state", errno );
			}
			return sock;
		}

		bool TcpTools::closeServerSocket( int sock )
		{
			return closeSocket(sock);
		}

		bool TcpTools::closeSocket( int sock )
		{
			::close(sock);
			return true;
		}


		int TcpTools::acceptNextTcpClient( int sock, sockaddr_in &peer, int *error )
		{
			socklen_t src_len = sizeof(peer);
			int s = accept( sock, (sockaddr*)&peer, &src_len );
			if( s < 0 )
			{
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw AcceptException( __FILE__, errno );
			}

			return s;
		}


		int  TcpTools::connectTo( sockaddr_in &server, bool setTcpNoDelay, int *error )
		{
			int sock = socket( AF_INET, SOCK_STREAM, 0 );
			if( sock == 0 )
			{
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw CreateSocketException( __FILE__, errno );
			}

			if( connect( sock, (sockaddr*)&server, sizeof( sockaddr_in ) ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw ConnectException( __FILE__, errno, server );
			}

			int delay = ( setTcpNoDelay ? 1 : 0 );

			if( setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &delay, sizeof(int) ) < 0 )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetOptionException( __FILE__, "setting TCP_NODELAY failed.", errno );
			}

			return sock;
		}

		bool TcpTools::disconnect( int sock )
		{
			shutdown( sock, SHUT_RDWR );
			waitForSendFinish( sock, ~0 );
			return closeSocket(sock);
		}

		bool TcpTools::waitForSendFinish( int sock, unsigned int msecs )
		{
			fd_set rdSocks, wrSocks, exSocks;
			FD_ZERO( &wrSocks );
			FD_SET( sock, &wrSocks );

			struct timeval  tv;
			struct timeval *ev;

			if( msecs == ~0 )
				ev = NULL;
			else
			{
				tv.tv_sec = msecs/1000;
				tv.tv_usec = (msecs%1000)*1000;
				ev = &tv;
			}


			int r = select(sock+1, NULL, &wrSocks, NULL, ev );
			return (r == 1);
		}

		/////////////////////////////////////////////////////////////////////////////////
		/// SOCKET PARAMETER STUFF
		/////////////////////////////////////////////////////////////////////////////////
		int TcpTools::makeAsync( int sock, int *error )
		{
			int nonBlocking = 1;
			if( ioctl( sock, FIONBIO, &nonBlocking ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetOptionException( __FILE__, "Failed to set blocking state", errno );
			}
			return 0;
		}


		int TcpTools::makeSync( int sock, int *error )
		{
			int nonBlocking = 1;
			if( ioctl( sock, FIONBIO, &nonBlocking ) )
			{
				::close(sock);
				if( error )
				{
					*error = errno;
					return -1;
				}
				else
					throw SetOptionException( __FILE__, "Failed to set blocking state", errno );
			}
			return 0;
		}

		std::string TcpTools::getErrorText( int error )
		{
			char *err = strerror( error );
			if( err )
				return std::string(err);
			return std::string();
		}

		/////////////////////////////////////////////////////////////////////////////////
		/// ADDRESS UTILITY
		/////////////////////////////////////////////////////////////////////////////////

		std::string TcpTools::getHostNameFromAddress( const struct sockaddr_in &address )
		{
			hostent *host = gethostbyaddr( &address, sizeof( sockaddr_in), AF_INET );
			if( host == NULL )
				return std::string();

			return std::string( host->h_name ? host->h_name : "" );
		}

		bool TcpTools::getNodeAddress( const std::string &nodename, sockaddr_in &store )
		{
			if( inet_aton(nodename.c_str(), &(store.sin_addr) ) == 0 )
			{
				// try DNS
				hostent* host = gethostbyname(nodename.c_str());
				if (host == NULL)
					return false; // failed
				store.sin_addr.s_addr = *((in_addr_t*)*host->h_addr_list);
			}

			store.sin_family = AF_INET;
			return true; // OK
		}

		bool TcpTools::isLocal( sockaddr_in &address, const AddressVector &localIp )
		{
			if ( ( (address.sin_addr.s_addr) & 0xff ) == 127 )
				return true; // loopback IP address

			for ( AddressVector::size_type i=0; i < localIp.size() ; i++ )
				if (address.sin_addr.s_addr == localIp[i].sin_addr.s_addr)
				  return true;

			return false;
		}

		/////////////////////////////////////////////////////////////////////////////////
		// SENDING AND RECEIVING
		/////////////////////////////////////////////////////////////////////////////////
		bool TcpTools::sendBlock( int sock, const void *buffer, size_t size, Stats *stats )
		{
			int sent, flags;

			#ifndef FLOWVR_OS_HAVE_MSGNOSIGNAL
				flags = 0;

				static bool bOnce = false;
				if(!bOnce)
				{
					signal( SIGPIPE, SIG_IGN ); // ignore signal on a global level!
					                            // this is very coarse, but keeps the
					                            // daemon from being terminated.
					bOnce = true;
				}
			#else
				flags = MSG_NOSIGNAL;
			#endif

			flowvr::utils::microtime n0;
			if( stats and (*stats).m_bTakeTime )
				n0 = flowvr::utils::getNtpTimeStamp();

			while (size > 0 and ((sent = ::send(sock, buffer, size, flags)) > 0 or errno == EINTR))
			{
				if (sent > 0)
				{
					buffer = ((const char*) buffer) + sent;
					size -= sent;
					if( stats )
					{
						(*stats).m_dataSize += sent;
						++(*stats).m_nNumBatches;
					}
				}
			}
			if( errno == EINTR )
				throw SendException( __FILE__, errno );

			if( stats and (*stats).m_bTakeTime )
				(*stats).m_dTimeNeeded += flowvr::utils::getNtpTimeStamp() - n0;

			return (size == 0);
		}

		bool TcpTools::multisendBlocks( int sock, multibuf* buffers, int count, Stats *stats )
		{
			#ifndef FLOWVR_OS_HAVE_WRITEV
				// emulate writev behavior
				bool ret = true;
				for (int i=0;i<count;i++)
					ret &= sendBlock(sock, buffers[i].iov_base,buffers[i].iov_len, stats);
				return ret;
			#else
				size_t sent;

				flowvr::utils::microtime n0;
				if( stats and (*stats).m_bTakeTime )
					n0 = flowvr::utils::getNtpTimeStamp();

				size_t nBuffersSent = 0;
				while( nBuffersSent < count )
				{
					size_t sendNow = std::min<size_t>( IOV_MAX, count-nBuffersSent );
					nBuffersSent += sendNow; // let's be optimistic, we get'em all through

					// count-down on sendNow
					while (sendNow > 0 and ((sent = ::writev(sock, buffers, sendNow)) > 0 or errno == EINTR))
					{
						if (sent > 0)
						{
							if( stats )
							{
								(*stats).m_dataSize += sent;
								++(*stats).m_nNumBatches;
							}

							// ok, some bytes were written, find the correct offset in the data
							while (sendNow > 0 && sent >= buffers[0].iov_len)
							{
								sent -= buffers[0].iov_len; // we obviously sent this buffer
								--sendNow; // so, get him out of the stats
								++buffers; // side-effect: advance in the buffer array
							}

							// we advances as much as possible, but there may be a part of the current buffer that was sent
							// sendNow > 0 := there are still buffers to send
							// sent > 0 := only a part of that buffer was already sent
							if (sendNow > 0 && sent > 0)
							{
								buffers[0].iov_base = ((char*) buffers[0].iov_base) + sent; // advance the amount of bytes left of this buffer
								buffers[0].iov_len -= sent; // decrease the send-length by the number already sent
							}


						}
					}

					if(errno == EINTR)
						throw SendException( __FILE__, errno );
				}

				if( stats and (*stats).m_bTakeTime )
					(*stats).m_dTimeNeeded += (flowvr::utils::getNtpTimeStamp() - n0);

				return (count == nBuffersSent);
			#endif
		}

		bool TcpTools::receiveBlock( int sock, void *buffer, size_t size, Stats *stats )
		{
			int flags;
			size_t rec;

			#if FLOWVR_OS_HAVE_MSGNOSIGNAL
				flags = MSG_NOSIGNAL; // avoid a SIGPIPE on broken pipe
			#else
				flags = 0;


			static bool bOnceRecv = false;
			if(!bOnceRecv)
			{
				signal( SIGPIPE, SIG_IGN ); // ignore signal on a global level!
											// this is very coarse, but keeps the
											// daemon from being terminated.
				bOnceRecv = true;
			}

			#endif

			flowvr::utils::microtime n0;

			if( stats and (*stats).m_bTakeTime )
				n0 = flowvr::utils::getNtpTimeStamp();

			// size>0 := while we still have data to expect for this block
			while (size > 0 && ((rec = ::recv(sock, buffer, size, flags)) > 0 or errno == EINTR))
			{
				if (rec > 0)
				{
					// ok, we received some bytes
					buffer = ((char*) buffer) + rec; // advance in the recv buffer
					size -= rec; // take off what we have read

					if (stats)
					{
						(*stats).m_dataSize += rec;
						++(*stats).m_nNumBatches;
					}
				}
				else
					// we assume blocking send / receive, so rec <= 0 indicates an error
					throw ReceiveException(__FILE__, errno );
			}

			if( stats and (*stats).m_bTakeTime )
				(*stats).m_dTimeNeeded += (flowvr::utils::getNtpTimeStamp() - n0);

			// success means we could read everything (effectively, using the exception above,
			// this function should always return true
			return (size==0);
		}
	}
}

