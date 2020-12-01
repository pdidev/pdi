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

#ifndef TCPTOOLS_H_
#define TCPTOOLS_H_


#include <vector>
#include <string>
#include <netinet/in.h>
#include <flowvr/utils/multibuf.h>
#include <flowvr/utils/timing.h>
#include <exception>
#include <string.h>
#include <iostream>

namespace flowvr
{
	namespace utils
	{
		class TcpTools
		{

		public:
			/**
			 * @brief little helper structure / class for send / receive statistics.
			 * This structure can be passed to
			 * - sendBlock()
			 * - multisendBlocks()
			 * - receiveBlock()
			 *
			 * It will contain accumulated results that can be used for statistics / debugging (a bit)
			 * if you re-use the same Stats object all the time. If you want information about just one call,
			 * always pass a new instance to one of the functions above. Note that is is the same Stats
			 * type for send and receive calls, we do not expect you to mix things up (e.g., passing a Stats
			 * that is accounting for sends to a receive call.
			 */
			class Stats
			{
			public:
				Stats()
				: m_bTakeTime(false)
				, m_dTimeNeeded(0)
				, m_dataSize(0)
				, m_nNumBatches(0)
				{}

				bool      m_bTakeTime;   /**< set to false (default) to turn of time-taking */
				microtime m_dTimeNeeded; /**< accumulated time (will be +='ed) */
				size_t    m_dataSize;    /**< accumulated data size (will be +='ed) */
				size_t    m_nNumBatches; /**< number of send / recv requests before finishing */

				/**
				 * @brief utility operator to sum up stats
				 * @return *this
				 * @param other stats to add to this one
				 */
				Stats &operator+( const Stats &other )
				{
					m_dTimeNeeded += other.m_dTimeNeeded;
					m_dataSize    += other.m_dataSize;
					m_nNumBatches += other.m_nNumBatches;

					return *this;
				}
			};


			/**
			 * @brief typedef to simplify typing a bit
			 */
			typedef std::vector<sockaddr_in> AddressVector;

			///{@ ip addressing and lookup

			/**
			 * @brief get a vector of all IP addresses known to this host
			 * Note that the method filters the loopback IP 127.0.0.0 as we assume that
			 * to be present on any machine supporting IP.
			 * @param list the 'list' to store all known IP addresses of this host to.
			 * @return the number of entries of the list
			 */
			static AddressVector::size_type getIPAddresses( AddressVector &list );

			/**
			 * @brief DNS lookup for a given hostname.
			 * This method uses gethostbyname on the given nodename and fills the sin_family
			 * with AF_INET. Fills the address to sin_addr.s_addr and does <b>not touch any other field</b>.
			 * You can give a dotted IP number as argument.
			 * @return depending on the success of gethostbyname
			 *         - true if gethostbyname succeeded
			 *         - false else
			 * @param nodename the hostname to look up, can be a dottet IP address
			 * @param store the address storage space.
			 */
			static bool getNodeAddress( const std::string &nodename, sockaddr_in &store );

			/**
			 * @brief tests whether a given IP address is 'local', meaning bound to this host.
			 * It therefore needs the list of local addresses acquired by getIPAddresses() or other means.
			 * We pass it as argument so client code can cache the list of local adapters, as they most
			 * likely do not change (unless you have wireless or USB network adapters, which we do
			 * not assume for a cluster).
			 *  @return - true when the address points to this host
			 *          - false when the address points to a remote host
			 */
			static bool isLocal( sockaddr_in &address, const AddressVector &localAddresses );

			/**
			 * @brief return the hostname for this machine.
			 * calls gethostname and returns the result as a string object.
			 * @return the hostname for this machine, or the empty string.
			 */
			static std::string getNodeHostName();

			/**
			 * @brief getStringFromAdress returns a string containing the given adress
			 * @return a string containing the given adress
			 */
			static std::string getStringFromAdress( const struct in_addr & );

			/**
			 * @brief getStringFromAdress returns a string containing the given adress
			 * @return a string containing the given adress
			 */
			static std::string getStringFromAdress( const struct in6_addr & );

			/**
			 * @brief analyzes a given address and returns a formatted string <host>:<port>.
			 * @return a string giving the hostname and the port used in address
			 * @param address the address to format as a string, port should be set.
			 */
			static std::string getHostFromAddress( const struct sockaddr_in &address, bool bResolveIp = false );
			
			static std::string getHostNameFromAddress( const struct sockaddr_in &address );

			/**
			 * @brief little macro to set up an address to be used for a 'listen-on-all-adapters' TCP server socket.
			 * @param port to listen to
			 * @param storage the address to be set up correctly
			 */
			static void createListenOnAllAddresses( sockaddr_in &storage, int port );

			/**
			 * @brief macro to have consistent printing of the hostname to std::cout
			 * @param ip the address to print to console using getHostName()
			 * @see getHostName()
			 */
			static void printAddress( const sockaddr_in &ip );
			///@}

			///{@ tcp send and receive stuff

			/**
			 * @brief sends a block of data using a given socket.
			 * It is assumed that the socket is a blocking TCP socket. This method will iterate as long
			 * as everything is sent, avoiding the need for multiple calls from the user.
			 * Throws a TcpTools::SendException in case of an error while sending.
			 *
			 * @param socket the TCP socket to send data to
			 * @param buffer a continuous block of memory to send of size 'size'
			 * @param size the size of buffer in bytes
			 * @param stats (optional) a Stats object to account to
			 *
			 * @return true when everything was of buffer was passed to the network layer.
			 *         As this function throws an exception upon (detected) failure, this function
			 *         should always return true.
			 */
			static bool sendBlock( int socket, const void *buffer, size_t size, Stats *stats = NULL );

			/**
			 * @brief sending a number of predefined buffers using the multibuf structure.
			 * It is assumed that 'socket' is a blocking TCP socket. The function returns when all buffers
			 * were passed to the network layer. The function throws an TcpTools::SendException in case
			 * of errors while sending.
			 *
			 * @param socket a blocking TCP socket
			 * @param buffers a pointer to an array of multibuf instances, the arraysize is given in 'count'
			 * @param count the length of the array passed as buffer
			 * @param stats (optional) a Stats object to account to.
			 *
			 * @return true when all buffers were passed to the network layer.
			 *              As this function throws an exception in case of errors, this function should
			 *              always return true.
			 */
			static bool multisendBlocks( int socket, multibuf* buffers, int count, Stats *stats = NULL );

			/**
			 * @brief receiving a block of data on a blocking TCP socket.
			 * The function receives a given number of bytes and stores them in 'buffer'. It will return
			 * when all bytes given by 'size' were received and stored. In case of a read error, this function
			 * will throw a TcpTools::ReceiveException.
			 *
			 * @param socket a blocking TCP socket
			 * @param buffer a pointer to the buffer to store the data in
			 * @param size the number of bytes to receive
			 * @param stats (optional) a Stats object to account to.
			 *
			 * @return true when all bytes were received successfully, false else.
			 *         As this function throws an exception, this function should always
			 *         return true (a problem could be an invalid socket though)
			 */
			static bool receiveBlock( int socket, void *buffer, size_t size, Stats *stats = NULL );

			/**
			 * @brief suspend caller until more data can be send.
			 * Utility function to test whether there is enough buffer space to send more data.
			 * This typically indicates that the current send is finished (hence the name). Can be used
			 * to wait for an ACK during a connect() (call after connect() to be sure that you can
			 * write to the socket without raising an exception).
			 *
			 * @param socket the socket to wait for
			 * @param msec a timeout to wait at maximum for the wait (~0 for infinite wait)
			 *
			 * @return true iff the socket can be used for writing again
			 */
			static bool waitForSendFinish( int socket, unsigned int msesc = ~0 );
			///@}


			///{@ tcp server and client stuff

			/**
			 * @brief macro function to create a TCP socket ca be used to receive client connections.
			 * Binds the given address and sets up the listen structure.
			 * @param address to use (should be an ip of this machine)
			 * @param bAllowReuse - true: overwrite an existing record (good when having pending sockets the would otherwise block / lead to failure)
			 *                    - false: fail during bind if there already is a socket bound
			 * @param nBacklog the number of backlogs to use in listen (the number of concurrent connection tries that will be back-logged)
			 * @param bAsync -true marks the socket as non-blocking
			 *               - false marks the socket as blocking (recommended)
			 * @param error (optional) when given, this method will not throw exceptions in case of failure, but store the error number in *error
			 *               and return -1. When not given (default) all failures will result in a TcpTools::Exception (and derivatives)
			 *
			 *
			 * @return the socket indicator. Can be -1 in case error is non-NULL and there was a problem during the socket setup.
			 *
			 * @see acceptNextTcpClient(), connectTo(), closeSocket(), closeServerSocket()
			 */
			static int  createTcpServerSocket( sockaddr &address, bool bAllowReuse, int nBacklog, bool bAsync, int *error = NULL );

			/**
			 * @brief gets the next client from a given TCP server socket.
			 * Calls accept with the given socket and passes the peer address for information of the peer.
			 *
			 * @param sock the server socket (TCP / non-blocking / setup with createTcpServerSocket() )
			 * @param peer storage to store peer information to
			 * @param error (optional) when not null and an error occurs, the errno is stored to error
			 *
			 * @return the socket indicator of the peer, -1 in case of error when error is non NULL
			 *
			 * @see createTcpServerSocket(), coonectTo()
			 */
			static int  acceptNextTcpClient( int sock, sockaddr_in &peer, int *error = NULL );

			/**
			 * @brief connect to a given server using its address
			 * Client call to connect to a server socket giving the (fully filled) address.
			 * Optionally sets the TCP_NODELAY flag. This function throws a TcpTools::Exception
			 * in case of error, when error is NULL.
			 *
			 * @param server the address to connect to
			 * @param setTcpNoDelay true if you want that flag to be set on the socket, false else
			 * @param error (optional) if non NULL, and an error occurs, the errno is stored to this variable
			 *
			 * @return the socket indicator for the server, can be -1 in case error is non-NULL
			 */
			static int  connectTo( sockaddr_in &server, bool setTcpNoDelay, int *error = NULL );

			/**
			 * @brief macro function to disconnect a client socket from a server.
			 * Calls shutdown (read and write) and tests for send-finished. Afterwards,
			 * it calls close.
			 *
			 * @param socket the socket indicator to disconnect
			 * @return the result of closeSocket()
			 */
			static bool disconnect( int socket );

			/**
			 * @brief closes an open server socket.
			 * Is currently an alias to closeSocket(), but makes it a bit more clear that we want to close a server socket.
			 * @return the result of closeSocket()
			 * @param sock the socket indicator to close
			 */
			static bool closeServerSocket( int sock );

			/**
			 * @brief call simple close on a socket indicator.
			 * @param sock the socket indicator to close
			 * @return true
			 */
			static bool closeSocket( int sock );
			///@}

			///{@ socket param stuff

			/**
			 * @brief macro function to enable non-blocking IO for a socket
			 *
			 * In case there was an error, the socket will be closed.
			 *
			 * @param sock the socket to mark non-blocking
			 * @param error (optional) when given, no exception will be thrown in case of error
			 *              and *error will be set to errno
			 * @return -1 in case of error and error was non-NULL, 0 else
			 */
			static int makeAsync( int sock, int *error = NULL );

			/**
			 * @brief macro function to disable non-blocking IO for a socket
			 *
			 * In case there was an error, the socket will be closed.
			 *
			 * @param sock the socket to mark blocking
			 * @param error (optional) when given, no exception will be thrown in case of error
			 *              and *error will be set to errno
			 * @return -1 in case of error and error was non-NULL, 0 else
			 */
			static int makeSync( int sock, int *error = NULL );
			///@}

			/**
			 * @brief macro function to return a human-readable text for a given errno.
			 * This function calls strerror with errno, checks the result and returns that
			 * as string. In case strerror does not return a meaningful value, the empty string
			 * is returned.
			 *
			 * @param error the error id probably returned by any IO function here
			 * @return the human-readable string describing error
			 */
			static std::string getErrorText( int error );

			// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			// TcpException
			// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

			class TcpException : public std::exception
			{
			public:
				TcpException( const char *type,
						      const char *where,
						      const char *what,
						      int _errno = 0,
						      sockaddr_in *addr = NULL )
				: std::exception()
				, m_type(type)
				, m_where(where)
				, m_what(what)
				, m_errno(_errno)
				, m_peer( addr ? true : false )
				{
					if(addr)
						m_addr = *addr;
				}

				virtual const char *what() const throw ()
				{
					return m_what;
				}

				virtual const char *where() const throw ()
				{
					return m_where;
				}

				virtual const char *type() const throw ()
				{
					return m_type;
				}


				int getError() const throw()
				{
					return m_errno;
				}

				std::string getErrorText() const
				{
					char *c = strerror( m_errno );
					if( c )
						return std::string(c);
					else
						return std::string("EINVAL");
				}

				void print() const
				{
					std::cerr << "TCP error during init: "
							  << "\t" << type() << std::endl
							  << "\t" << getErrorText()
							  << " at " << std::endl << "\t"
							  << where() << std::endl;
					if(m_peer)
							  std::cerr << "peer address: " << getHostFromAddress(m_addr) << std::endl;
				}
				const char *m_where, *m_what, *m_type;
				int         m_errno;
				sockaddr_in m_addr;
				bool m_peer;
			}; // TcpException

			// ----------------------------------------------------------------

			class CreateSocketException : public TcpException
			{
			public:
				CreateSocketException( const char *where, int   err )
				: TcpException( "CreateSocketException", where, "Could not create TCP socket",  err )
				{
				}

			};

			// ----------------------------------------------------------------

			class ConnectException : public TcpException
			{
			public:
				ConnectException( const char *where, int err, sockaddr_in &addr )
				: TcpException("ConnectException", where, "Failed to connect", err, &addr )
				{

				}
			}; // ConnectException

			// ----------------------------------------------------------------

			class SetOptionException : public TcpException
			{
			public:
				SetOptionException( const char *where, const char *what, int err )
				: TcpException("SetOptionException", where, what, err ) {}
			};

			// ----------------------------------------------------------------

			class BindSocketException : public TcpException
			{
			public:
				BindSocketException( const char *where, int err )
				: TcpException( "BindSocketException", where, "failed to bind socket.", err ) {}
			};

			// ----------------------------------------------------------------

			class SetListenStateException : public TcpException
			{
			public:
				SetListenStateException( const char *where, int err )
				: TcpException( "SetListenStateException", where, "failed to set listen state on server socket", err ) {}
			};

			// ----------------------------------------------------------------

			class AcceptException : public TcpException
			{
			public:
				AcceptException( const char *where, int err )
				: TcpException( "AcceptException", where, "failed during accept.", err ) {}
			};

			// ---------------------------------------------------------------

			class ReceiveException : public TcpException
			{
			public:
				ReceiveException( const char *where, int err )
				: TcpException( "ReceiveException", where, "failed during receiving of data.", err ) {}
			};

			// ---------------------------------------------------------------

			class SendException : public TcpException
			{
			public:
				SendException( const char *where, int _err )
				: TcpException( "SendException", where, "failed to write data", _err ) {}
			};

			// ---------------------------------------------------------------

		}; // TcpTools
	}
}


#endif // TCPTOOLS_H_
