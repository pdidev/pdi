/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
* File: src/plugins/flowvr.plugins.NetMPI.cpp                     *
*                                                                 *
* Contacts:                                                       *
*  10/12/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/config.h>
#include <flowvr/daemondata.h>
#include <flowvr/daemon.h>
#include <flowvr/stamp.h>
#include <flowvr/message.h>
#include <flowvr/thread.h>
#include <flowvr/ipc/mtlock.h>
#include <flowvr/ipc/mtsignal.h>
#include <flowvr/plugd/class.h>
#include <flowvr/plugd/object.h>

#include <flowvr/plugd/genclass.h>
#include <flowvr/plugd/dispatcher.h>
#include <flowvr/plugd/actionhandler.h>

#include <flowvr/utils/multibuf.h>
#include <flowvr/utils/tcptools.h>

#include <mpi.h>

#include <map>
#include <sys/types.h>
#include <vector>
#include <limits.h>
#include <functional>
#include <algorithm>
#include <sstream>


#define TCPMAXMSG  IOV_MAX/3

#define notNULL(A) ((A) == NULL ? std::string("") : std::string(A))


namespace flowvr
{
	namespace plugins
	{

	using namespace flowvr::plugd;
	using namespace flowvr::utils;

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/**
	 * @brief helper structure for tcp protocol
	 * Each message over the tcp connection is preceded by a MsgHeader, describing
	 * what to expect coming. It is a bit optimized for the 'original' protocol
	 * (without segments).
	 * @private
	 */
	struct MsgHeader
	{
		  enum
		  {
			STANDARD = 0,
			CONTROL  = 1
		  };

		  MsgHeader()
		  : type(0), stamps_size(0), data_size(0), num_segments(0) {}

		  MsgHeader( int _type, size_t _stamps_size, size_t _data_size, size_t _numSegs)
		  : type( _type )
		  , stamps_size( _stamps_size )
		  , data_size( _data_size )
		  , num_segments( _numSegs ) {}

		  int    type;         /**< STANDARD or CONTROL */
		  size_t stamps_size;  /**< number of bytes to receive as stamps */
		  size_t data_size;    /**< -1 if stamps_only message, otherwise it is the number of bytes for the payload (data) */
		  size_t num_segments; /**< number of segments for the top-level buffer (not the total number of segments */
	};

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


	/**
	 * @brief helper structure for sending messages.
	 * It encapsulates system-routing information (control or standard message)
	 * as well as it keeps a reference up until the message was processed and turned into
	 * a SendBlock.
	 * @private
	 */
	struct TaggedMessage
	{
		TaggedMessage()
		: m_msg()
		, m_bControlMessage(false) {}

		TaggedMessage( const flowvr::Message &msg, bool bControl )
		: m_msg(msg)
		, m_bControlMessage( bControl ) {}

		flowvr::Message m_msg;  /**< reference to the original message (hold ref-pointer) */
		bool m_bControlMessage; /**< true if this message is a control/daemon message */
	};


	/**
	 * @brief the NetMPI plugin to the daemon.
	 * It encapsulates the send and receive protocol for messages sent from one host to
	 * one another on a different IP.
	 * In case a message is detected to be for the host this NetMPI object is running on
	 * it will bypass sending and directly put it back to the daemon for further dispatching.
	 * Things to note:
	 * - it opens two connections PER HOST to communicate stamps and messages separately
	 *   (different applications running would not cause a new connection unless there is a
	 *   new host detected as destination)
	 * - it opens a server socket to accept incoming connections from other daemons.
	 * - for each host / connection there is a dedicated thread that takes care of getting
	 *   the messages from the incoming / outgoing queue, dispatching / encoding and
	 *   sending the message
	 * - the plugin can be told to record stats on the connections and the number of messages
	 *   sent per host.
	 */
	class NetMPI : public flowvr::plugd::Object
	{
	public:
	  class mpiThread;
	  class SendThread;
	  class RecvThread;
		
	  NetMPI(const std::string &objID);
	  virtual ~NetMPI();

	  virtual Result init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);

	  /**
	   * @brief main dispatcher class for the tcp server socket to receive incoming
	   *        client requests. The server socket is monitored by a state thread,
	   *        so for legacy reasons, this method is still called 'run'.
	   *
	   */
	  virtual int run();


	  virtual Class*         getClass() const;
	  virtual ActionHandler* createAction(xml::DOMElement* xmlRoot);

	  virtual void processControlMessage(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher);
	  virtual Result doAction(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);


	  // ---------------------------------------------------------------------
	  // message receiver management

          bool remRevcThread( RecvThread * );


	  /**
	   * @brief toggle recording of statistics for this instance.
	   * @return the old state (true: stats now disabled, false else)
	   */
	  bool toggleStats();

	  /**
	   * @brief gets the current state of whether statistics are taken or not
	   * @return true if for each message sent and received there is an update in statistics
	   */
	  bool getTakeStats() const;

	  std::string createSendStatsString() const;
	  std::string createRecvStatsString() const;
	  std::string createConnectionsString() const;


	public:
	  
	  // ---------------------------------------------------------------------
	  // mpiThread
	  // ---------------------------------------------------------------------
	  class mpiThread : public flowvr::Thread
	  {
	  public:
		class Job;
		class TodoList;

		mpiThread(NetMPI *parent, Dispatcher* _dispatcher);
		
		/// return the rank of the daemon
		int getRank() const { return _rank; }
		/// return the rank of the daemon whose \p addr is an address
		int getRank( in_addr addr ) const;
		
		virtual void enqueue( const Job & );
		virtual void splice( std::list< Job > & );
		
	  protected:
		
		friend class NetMPI::SendThread;
		friend class NetMPI::RecvThread;
		
		std::list< Job >    _newJobs;
		ipc::MTSignal       _signalQueue;
		ipc::MTLock         _lockThread;
		
		void close();
		virtual int run();
		Buffer receiveBuffer(bool keepSegments);
		
	  private:
		
		bool               stop;
		
		NetMPI *           m_parent;
		Dispatcher*        m_dispatcher;
		TcpTools::Stats    m_stats;
		
		int                _rank;
		std::map< std::string, int > _ip2rank;
		
		void init_MPI();
	  };

	  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  // Helper classes, threads for sending and receiving
	  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	  // ---------------------------------------------------------------------
	  // SendThread
	  // ---------------------------------------------------------------------
	  class SendThread : public flowvr::Thread
	  {
	  public:
		SendThread(NetMPI* _parent, struct sockaddr_in _ip, int _sock);


		std::deque<TaggedMessage> msgQueue;
		ipc::MTSignal         signalQueue;
		ipc::MTLock           lockThread;

		struct sockaddr_in ip;
		int _peerRank; ///< rank of the MPI peer
		NetMPI             *parent;

		int  sock;
		bool stop;

		TcpTools::Stats m_stats;

		virtual void enqueue(const flowvr::Message& msg, bool control);
		int getSocket() const { return sock; }

		void close();
	  protected:
		virtual int run();
	  };

	  // ---------------------------------------------------------------------
	  // RecvThread
	  // ---------------------------------------------------------------------
	  class RecvThread : public flowvr::Thread
	  {
	  public:
		RecvThread(NetMPI *parent, Dispatcher* _dispatcher, int _sock, struct sockaddr_in _ip);
		void close();

		Dispatcher* dispatcher;
		bool stop;
		int  sock;
		struct sockaddr_in ip;
		int _peerRank;

		NetMPI *m_parent;

		TcpTools::Stats m_stats;
	  protected:
		virtual int run();
		Buffer receiveBuffer( std::vector<multibuf> &bufs, bool keepSegments );
	  };

	  // ---------------------------------------------------------------------
	  // StateThread
	  // ---------------------------------------------------------------------
	  class StateThread : public flowvr::Thread
	  {
	  public:
		  StateThread( NetMPI *parent );

	  protected:
		  virtual int run();
		  NetMPI *m_parent;
	  };

	  // ---------------------------------------------------------------------
	  // SendActionHandler
	  // ---------------------------------------------------------------------
	  class SendActionHandler : public ActionHandler
	  {
	  public:
		SendActionHandler(NetMPI* _parent, struct sockaddr_in _ip, int _sock);

		ipc::MTAtomicInt   nbref;
		NetMPI            *parent;
		struct sockaddr_in ip;

		// No need for 2 queues using MPI: Data won't stall stamp-only messages
		// Furthermore, we rely on the FIFO order when calling MPI_iRecv
		SendThread queue;

		/// Create an ActionHandler for batch mode action execution.
		virtual ActionHandler* createAction(xml::DOMElement* xmlRoot);
		virtual void enqueue(const flowvr::Message& msg, bool control);
		virtual void doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher);
		virtual void remove();
		void close();

		bool operator==( const SendActionHandler &other ) const;
		bool operator==( const sockaddr_in &other ) const;
	  };

	  // ---------------------------------------------------------------------
	  // ControlActionHandler
	  // ---------------------------------------------------------------------
	  class ControlActionHandler : public ActionHandler
	  {
	  public:
		ControlActionHandler(NetMPI* _parent);

		ipc::MTLock      lock;
		ipc::MTAtomicInt nbref;
		NetMPI     *parent;

		virtual ActionHandler* createAction(xml::DOMElement* xmlRoot);
		virtual void doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher);
		virtual void remove();
	  };

	private:
	  SendActionHandler* getSendActionHandler(sockaddr_in &dest);

	  ipc::MTLock                     lockSenders;
	  std::vector<SendActionHandler*> senders;

	  ipc::MTLock                     lockReceivers;
	  std::vector<RecvThread*>        receivers, toClaim;
	  
//	  ipc::MTLock                     lockMPI;
	  mpiThread                      *mpiPerformer;

	  ControlActionHandler           *controlActionHandler;

	  int m_DaemonPort,
	      m_acceptSocket;

	  Dispatcher             *threadDispatcher;
	  TcpTools::AddressVector localIp; ///< List of local network interfaces IP addresses
	  StateThread             stateThread;

	  bool bTakeStats;
	};

	template<class T>
	class _setStats
	{
	public:
		_setStats( bool bVal )
		: m_val(bVal) {}

		bool m_val;

		void operator()( T *el ) const
		{
			el->m_stats.m_bTakeTime = m_val;
		}
	};

	template<>
	void _setStats<NetMPI::SendActionHandler>::operator()( NetMPI::SendActionHandler *el ) const
	{
		el->queue.m_stats.m_bTakeTime = m_val;
	}

	template<class T>
	class _createConString
	{
	public:
		std::string m_text;

		void operator()( T *el )
		{
			m_text += TcpTools::getHostFromAddress(el->ip) + std::string("\n");
		}
	};

	template<>
	void _createConString<NetMPI::SendActionHandler>::operator()( NetMPI::SendActionHandler *el )
	{
		m_text += (TcpTools::getHostFromAddress( el->ip ) + std::string("\n")
		       + TcpTools::getHostFromAddress( el->queue.ip ) + std::string("\n") );
	}

	template<class T>
	class _createStatsString
	{
	public:
		std::string m_text;

		void operator()( T *el )
		{
			std::stringstream in;
			in << "\n" << TcpTools::getHostFromAddress(el->ip) << "\n"
					<< "Total time : " << el->m_stats.m_dTimeNeeded << "\n"
				   << "Data size  : " << el->m_stats.m_dataSize    << "\n"
				   << "Num batches: " << el->m_stats.m_nNumBatches << "\n";

			m_text += in.str();
		}
	};


	template<>
	void _createStatsString<NetMPI::SendActionHandler>::operator()( NetMPI::SendActionHandler *el )
	{
		std::stringstream in;
		in << "\n" << TcpTools::getHostFromAddress(el->ip) << "\n"
			   << "Full queue:\n"
			   << "Total time : " << el->queue.m_stats.m_dTimeNeeded << "\n"
			   << "Data size  : " << el->queue.m_stats.m_dataSize    << "\n"
			   << "Num batches: " << el->queue.m_stats.m_nNumBatches << "\n";
		m_text += in.str();
	}

//################################################################################
// IMPLEMENTATION BLOCK
//################################################################################

	/**
	 * @brief The NetMPI::mpiThread::Job class represent a data send/recv to
	 *  perform usign MPI.
	 *
	 * Instances are created using the \c newSend and \c newRecv static
	 * functions before being inserted in a \c TodoList. You don't have to 
	 */
	class NetMPI::mpiThread::Job
	{
	public:
		
		friend class TodoList;
		
		//////////////////////////////////////////////////////////////////////
		//  Public types
		//////////////////////////////////////////////////////////////////////
		
		enum what_e { Recv, Send };

		//////////////////////////////////////////////////////////////////////
		//  Public static functions
		//////////////////////////////////////////////////////////////////////
		
		/**
		 * @brief newSend creates a send Job
		 * @param msg the FlowVR messages sent, for reference counting sake
		 * @param bufs memory chunks (from the messages) storing data to sent
		 * @param peerRank rank of the receiver daemon
		 * @return a Job to insert in a TodoList
		 */
		static inline Job newSend( const std::vector<flowvr::Message> & msg, const std::vector<multibuf>& bufs, int peerRank );// { return Job( msgs, bufs, peerRank ); }
		
		/**
		 * @brief newRecv creates a receive Job
		 * @param msg the FlowVR message to be forwarded to the dispatcher
		 * @param bufs memory chunks (in the message) to store received data
		 * @param peerRank rank of the sender daemon
		 * @return a Job to insert in a TodoList
		 */
		static inline Job newRecv( const TaggedMessage& msg, const std::vector<multibuf>& bufs, int peerRank );// { return Job( msg, bufs, peerRank ); }
		
		//////////////////////////////////////////////////////////////////////
		//  Public methods
		//////////////////////////////////////////////////////////////////////
		
		/// return what kind of job is the instance
		enum what_e what() const { return _what; }
		/// return true is the job is a Send one
		bool isRecv() const { return Recv == what(); }
		/// return true is the job is a Receive one
		bool isSend() const { return Send == what(); }
		
		/// return \c true if the job is completed
		bool isDone() const { return 0 == _nLeft; }
		
		
	protected:
		
		//////////////////////////////////////////////////////////////////////
		//  Protected types
		//////////////////////////////////////////////////////////////////////
	
		//////////////////////////////////////////////////////////////////////
		//  Protected members
		//////////////////////////////////////////////////////////////////////
		
		//////////////////////////////////////////////////////////////////////
		//  Protected methods
		//////////////////////////////////////////////////////////////////////
		
		/// create a Send job
		Job( const std::vector<flowvr::Message> & msg, const std::vector<multibuf> &, int peerRank );
		/// create a Receive job
		Job( const TaggedMessage& msg, const std::vector<multibuf> &, int peerRank );
		
		/// begin the job, creating the MPI requests
		inline void beginIt( const std::vector<MPI_Request*> & jobR );
		/// end the job after MPI request are completed, doing any required 
		/// post-process (giving a received message to the dispatcher)
		inline void endIt( NetMPI::mpiThread *thread );
		
		/// update the job after \p nDone MPI request have been completed
		/// \attention no range-check is performed  
		void update( int nDone ) { _nLeft -= nDone; }
		
		
	private:
		
		//////////////////////////////////////////////////////////////////////
		//  Private types
		//////////////////////////////////////////////////////////////////////
		
		//////////////////////////////////////////////////////////////////////
		//  Private members
		//////////////////////////////////////////////////////////////////////
		
		std::vector<flowvr::Message> _msgs; ///< copy of the msgs (ref counting)
		std::vector<multibuf> _bufs; ///< the memory blocks to send/reveive
		int _rank; /// rank of the daemon we'll send/receive a message to/from
		int _nLeft; ///< the number of requests which are still to be completed
		int _msgType; ///< type of the message, (eg: MsgHeader::STANDARD )
		enum what_e _what; ///< what kind of job it is (send or receive)
		
		//////////////////////////////////////////////////////////////////////
		//  Private methods
		//////////////////////////////////////////////////////////////////////
		
		/// begin a send job, \see beginIt
		void beginSend( const std::vector<MPI_Request*> & jobR );
		/// begin a receive job, \see beginIt
		void beginRecv( const std::vector<MPI_Request*> & jobR );
		
		/// end a send job, \see beginIt
		void endSend( NetMPI::mpiThread * ) { /* no post-process */ }
		/// end a receive job, \see beginIt
		void endRecv( NetMPI::mpiThread *thread );
		
	};

	/**
	 * @brief The NetMPI::mpiThread::TodoList class handles every MPI calls
	 *  after initialization.
	 *
	 * The public interface is quite simple with only two methods.
	 */
	class NetMPI::mpiThread::TodoList
	{
	public:
		
		TodoList( NetMPI::mpiThread *performer )
			: _performer( performer )
			, _jobs()
			, _newJob( _jobs.end() )
			, _reqs()
			, _reqJobs()
			, _freeIndexes()
//			, _nSend( 0 )
			, _nRecv( 0 )
		{}
		
		~TodoList()
		{
			freeAll();
		}
		
//		int nSend() const { return _nSend; }
		int nRecv() const { return _nRecv; }

		size_t size() const { return _jobs.size(); }
		size_t empty() const { return _jobs.empty(); }
		
		/**
		 * @brief splice new jobs to perform from a given list.
		 *
		 * This method keeps an iterator to the first newly-inserted job,
		 * updating the \c _newJob iterator if necessary.
		 *
		 * \attention It should be possible to splice jobs in several calls but
		 * this has neither been found necessary nor tested.
		 *
		 * @param jobs list to splice to the end of the TodoList.
		 */
		void splice( std::list< Job > & jobs )
		{
			if ( ! jobs.empty() ) {
				// no previous new Jobs
				if ( _newJob == _jobs.end() ) {
					_newJob = jobs.begin();// splice doesn't invalidate any iterator 
				}
				_jobs.splice( _jobs.end(), jobs );
			}
		}
		
		/**
		 * @brief doIt try to complete the stored jobs.
		 *
		 * This methods have a different behavior if the list contains a receive
		 * Job.
		 *
		 * If there is a \em receive \c Job, it performs a passive wait until at
		 * least one job is completed. This ensure received messages are
		 * forwarded to the dispatcher as soon as possible. This requires
		 * however that no passive wait is done elsewhere.
		 *
		 * Otherwise, if there is no rending receive, it settle for testing jobs.
		 */
		void doIt()
		{
			beginNewJobs();
			
			// Using MVAPICH2, it seems an immediate receive takes time to
			// complete if the corresponding send request is still pending.
			// Thus, wait is only performed using MPI_Waitsome.
			// \see \c NetMPI::mpiThread::run

                        // Bruno: waitSome is a blocking instruction. Avoid it to repvent deadlocks.
                        // Use testSome instead
                        //			if ( true || 0 != nRecv() )
                        //				waitSome();
                        //			else
				testSome();
		}
		
		
	protected:
		
		/**
		 * @brief freeAll cancels and frees ressources of the pending requests
		 */
		void freeAll()
		{
			for ( int i = 0  ;  i < _reqs.size()  ;  i++ )
			{
				if ( _reqJobs[i] == _jobs.end() )
					continue;
				
				// Cancel operation
				int err = MPI_Cancel( &_reqs[i] );
				
				// free request
				if ( 0 == err)
					err = MPI_Request_free( &_reqs[i] );
				
				if ( err ) // error checking
				{
					char errStr[ MPI_MAX_ERROR_STRING ];
					int errLen;
					if ( !MPI_Error_string( err, errStr, &errLen ) )
						std::cerr << errStr << std::endl;
					throw std::bad_exception();
				}
			}
		}
		
		/**
		 * @brief doIt try to complete some of the stored jobs
		 */
		void testSome()
		{
			if ( _reqs.empty() )
				return;
			
			int count;
			std::vector<int> indexes( _reqs.size() );
			int err = MPI_Testsome( _reqs.size(), &_reqs[0], &count,
								   &indexes[0], MPI_STATUSES_IGNORE );
			if ( err ) // error checking
			{
				char errStr[ MPI_MAX_ERROR_STRING ];
				int errLen;
				if ( !MPI_Error_string( err, errStr, &errLen ) )
					std::cerr << errStr << std::endl;
				throw std::bad_exception();
			}
			
			if ( count != MPI_UNDEFINED )// bruno meaninglful?
                            for ( int i = 0  ;  i < count  ;  i++ )
                                {
                                    if ( _jobs.end() != _reqJobs[ indexes[i] ] ) // finally completed
					postProcessRequest(  indexes[i]  );
                                }

                        
                        // if no MPI completed, we go to sleep to avoid
                        // going to often in the critical section, taking the associate lock
                        // and preventing  other threads to post send/recv requests
                        if (count == 0)
                            {
                                timespec treq;
                                treq.tv_sec = (time_t) (0);
                                treq.tv_nsec = (long) (0);
                                nanosleep(&treq, NULL); 
                            }
                        
//			// resize request vectors
//			_reqs.resize( end ); // you MUST update _freeIndexes too
//			_reqJobs.resize( end );
		}
		
		/**
		 * @brief ...
		 */
		void waitSome()
		{
			int count;
			std::vector<int> indexes( _reqs.size() );
			int err = MPI_Waitsome( _reqs.size(), &_reqs[0], &count,
									&indexes[0], MPI_STATUSES_IGNORE );
			
			// error checking
			if ( err )
			{
				char errStr[ MPI_MAX_ERROR_STRING ];
				int errLen;
				if ( !MPI_Error_string( err, errStr, &errLen ) )
					std::cerr << errStr << std::endl;
				throw std::bad_exception();
			}
			
			if ( count != MPI_UNDEFINED ) // this test shouldn't be necessary
			for ( int i = 0  ;  i < count  ;  i++ )
			{
				if ( _jobs.end() != _reqJobs[ indexes[i] ] ) // finally completed
					postProcessRequest(  indexes[i]  );
			}
		}

		/**
		 * @brief beginNewJobs start newly inserted jobs
		 *
		 * This methods reserved elements in the \c _req and \c _reqJobs vectors
		 * before effectively initiating the MPI requests.
		 */

		void beginNewJobs()
		{
			// find free space in the '_reqs'/'_reqJobs' vectors
			size_t queRSize = _reqs.size(); // queue: size of requests vector
			size_t i; // index inside the job's requests-vector
			size_t nFree = _freeIndexes.size(); 
			
			// Resize vectors right now to ensure to use valid pointers when
			// begining the jobs
			size_t neededSize = 0;
			size_t jobRMax = 0;
			for ( std::list< NetMPI::mpiThread::Job >::const_iterator it = _newJob
				  ; it != _jobs.end() ; ++it )
			{
				neededSize += it->_bufs.size();
				if ( jobRMax < it->_bufs.size() )
					jobRMax = it->_bufs.size();
			}
			if ( neededSize > _freeIndexes.size() ) {
				// the new jobs need more requests than currently free
				size_t newSize = queRSize + neededSize - _freeIndexes.size() ;
				_reqs.resize( newSize );
				_reqJobs.resize( newSize );
			}
			// create a temporary vector storing pointers to the requests
			std::vector<MPI_Request*> jobR; // used for MPI calls
			jobR.reserve( jobRMax );
			
			// fit entire jobs while enough free indexes
			for ( /* it == #1 new job */ ; _newJob != _jobs.end() ; ++_newJob )
			{
				size_t jobRSize = _newJob->_bufs.size(); // job: n of requests
				jobR.resize( jobRSize );
				if ( jobRSize > nFree ) // this job won't fit
					break;				// break to the next for loop
				
				// we need to store each request separatly
				for ( i = 0  ;  i < jobRSize  ;  i++ )
				{
					// get the oldest free index
					size_t index = _freeIndexes.front();
					_freeIndexes.pop_front();
					// keep a pointer to the request for an MPI call
					jobR[i] = &_reqs[index];
					// TodoList stores the job iterator
					_reqJobs[index] = _newJob;
				}
				nFree -= jobRSize;
				
				// begin the job (MPI_Isend, MPI_Irecv)
				if ( _newJob->isRecv() )
					_nRecv++;
				_newJob->beginIt( jobR );
			}
			
			// fit what you can from this job (known not to fit entirely)
			if ( _newJob != _jobs.end() )
			{
				// we need to store each request separatly
				for ( i = 0  ;  nFree /* (&& i < jobRSize) */ ;  i++, nFree-- )
				{
					size_t index = _freeIndexes[i];
					// keep a pointer to the request for an MPI call
					jobR[i] = &_reqs[index];
					// TodoList stores requests and the job iterator
					_reqJobs[index] = _newJob;
				}
				// we reached the end of our (the TodoList) requests vector
				_freeIndexes.clear();
			}
			
			// fit what's left in the back of the previously resized vectors
			for ( size_t index = queRSize /* it == first new job */
				  ;  _newJob != _jobs.end() ;  ++_newJob  )
			{
				// we need to store each request separatly
				size_t jobRSize = _newJob->_bufs.size();
				jobR.resize( jobRSize );
				for ( /* i == first request */ ; i < jobRSize ; i++, index++ )
				{
					// keep a pointer to the request for an MPI call
					jobR[i] = &_reqs[index];
					// TodoList stores requests and the job iterator
					_reqJobs[index] = _newJob;
				}
				
				// begin the job (MPI_Isend, MPI_Irecv)
				if ( _newJob->isRecv() )
					_nRecv++;
				_newJob->beginIt( jobR );
				i = 0; // re-init i for next loop
			}
		}
		
		/// \brief postProcessRequest updates the job associed with the (newly
		///	completed) request stored at index \p i
		void postProcessRequest( int i )
		{
			// keep the job aware that one of its requests is done
			_reqJobs[i]->update( 1 );
			if ( _reqJobs[i]->isDone() )
			{
				// the job has now all its requests completed, end it
				_reqJobs[i]->endIt( _performer );
				// forget it
				if ( _reqJobs[i]->isRecv() )
					_nRecv--;
				_jobs.erase( _reqJobs[i] );
			}
			_reqJobs[i] = _jobs.end();	// mark the request as done
			_reqs[i] = MPI_REQUEST_NULL;// necessary?
			_freeIndexes.push_back( i );// keep trac of the newly free index
		}

		
	private:
		
		/// the MPI thread that will do the MPI calls
		NetMPI::mpiThread *                            _performer;
		
		/// a list of pending jobs
		std::list< NetMPI::mpiThread::Job >            _jobs;
		/// begining of the newly inserted jobs
		std::list< NetMPI::mpiThread::Job >::iterator  _newJob;
		
		/// a contiguous (23.2.4.1) array of MPI requests
		std::vector< MPI_Request >                     _reqs;
		/// the associed jobs
		std::vector< std::list< NetMPI::mpiThread::Job >::iterator > _reqJobs;
		
		/** \brief indexes free inside the \c _reqs and \c _reqJobs vectors
		 *  \note we use a \c deque to ensure each free index will be used again
		 *    later, so the vectors can be resized down.
		 */
		std::deque< unsigned >                         _freeIndexes;
		
//		int _nSend;		///< number of Send requests in the TodoList
		int _nRecv;		///< number of Receive requests in the TodoList

	};
	
	namespace
	{
		/**
		 * @brief little helper for find a SendActionHandler based on an IP
		 * @private
		 */
		class _equals : public std::unary_function<const NetMPI::SendActionHandler*, void>
		{
		public:
			_equals( const sockaddr_in &_me )
			: me(_me) {}

			/**
			 * @brief uses the comparison operator define in SendActionHandler
			 * @see SendActionHandler::operator==
			 */
			bool operator()( const NetMPI::SendActionHandler *other ) const
			{
				return (*other == me);
			}
			sockaddr_in me;
		};

		/**
		 * @brief representation of a number of message as a number of multibuffers.
		 * Adds additionally the sending of multibuffers but respecting the IOV_MAX
		 * limitation of writev (if used).
		 * Its main purpose is to keep the memory that is to be sent over IP until it
		 * is finally sent. As we want to pass a list of multibuffers to the TCP layer,
		 * we got to keep the memory of
		 * -# the multibuffers themselves
		 * -# all MsgHeader structures we send
		 * -# all Messages that were processed (keep ref-count in shm for the buffers)
		 * -# the tree information and the additional size encoding for the segment tree
		 */
		class SendBlock
		{
		public:
			SendBlock()
			{
				m_headers.reserve(TCPMAXMSG);  // we can give a bound on the headers by the number of
				                            // MAXMSG, as we are not going to work on more than that
				                            // per iteration
			}

			bool empty() const { return m_tcpbuffers.empty() /*(redoundant) && m_mpibuffers.empty()*/; }

			std::vector<MsgHeader>       m_headers; /**< keep a list of MsgHeaders for each message */
			std::vector<multibuf>        m_tcpbuffers; /**< keep list of multibuffers to send through TCP */
			std::vector<flowvr::Message> m_tcppayload; /**< keep a reference to each message that is to be sent */

			std::vector<flowvr::Message> m_mpipayload; /**< keep a reference to each message that is to be sent through MPI */
			std::vector<MPI_Request>     m_mpirequests; /**< list of mpi requests */
			std::vector<multibuf>        m_mpibuffers; /**< keep list of multibuffers to send */
			
			
			/*
			 * note that we use lists here as we do not know about the dynamic structures of the tree in the
			 * message. As we store pointers to elements of the list, we have to make sure, we maintain
			 * memory. If we use vectors (as was before) we most likely lose all the references when we have
			 * to do a resize of the vector. Using lists is a bit more stressing on dynamic memory, but
			 * at least we can grow as much as we like.
			 */
			std::list<size_t>   m_sizes; /**< sizes for each buffer in the segment tree */

			/**
			 * @brief adds a buffer to the state variables of this send block.
			 * This call may be recursive on the segment tree. It will however store
			 * its data in the members of this SendBlock, so in the end we have a list
			 * of multibuffers to send in that order.
			 * This method will change
			 * - the multibuffer vector
			 * - the tree vector
			 * - the sizes vector
			 *
			 * @param buffer the buffer to encode in the list of multibuffers
			 */
			void addMultibufFromBuffer( const Buffer &buffer, bool keepSegments )
			{
				// flowvr-protocol (tcp):
				// 1) total-size (size_t)
				// 2) # of segments (size_t) = n
				// 3) n * ( # of bytes per segment + payload of segment )
				multibuf mb;

				// always send the total size first, so we can construct
				// the buffer on the other side in one big memory block
				m_sizes.push_back( buffer.getSize(Buffer::ALLSEGMENTS) );
				mb.iov_base = &m_sizes.back();
				mb.iov_len  = sizeof(size_t);
				m_tcpbuffers.push_back(mb);

				// send number of segments
				m_sizes.push_back( buffer.getNumberOfSegments() );
				mb.iov_base = &m_sizes.back();
				mb.iov_len  = sizeof(size_t);
				m_tcpbuffers.push_back(mb);

				const std::vector<Buffer::_bufferDesc> &segments = buffer.getSegments();
				for( size_t n = 0; n < buffer.getNumberOfSegments(); ++n )
				{
					const Buffer::_bufferDesc &segment = segments[n];
					m_sizes.push_back( segment.size );
					mb.iov_base = &m_sizes.back();
					mb.iov_len  = sizeof(size_t);
					m_tcpbuffers.push_back(mb);

					mb.iov_base = segment.pointer;
					mb.iov_len  = segment.size;
					m_mpibuffers.push_back(mb);
				}
			}


			/**
			 * @brief adds a message to the block, regarding the type flag.
			 * This method changes the
			 * - multibuffer vector
			 * - the MsgHeader vector
			 * - and by calling addMultibufFromBuffer all other vectors as well (as a secondary effect)
			 *
			 * @param m the message to add to the SendBlock
			 * @param bControl true if this message is a control message for other daemons, false else
			 * @return always true
			 */
			bool addMessageToBlock( const Message &m, bool bControl, int msgflags )
			{
				bool keepsegments = msgflags bitand StampList::SYS_FLG_KEEPSEGMENTS;
				bool mValid = m.data.valid();
				size_t dataSize = keepsegments ? ~0 : m.data.getSize(Buffer::ALLSEGMENTS);
				size_t numSegs  = keepsegments ? m.data.getNumberOfSegments() : 1;


				m_headers.push_back(MsgHeader( ( bControl ? MsgHeader::CONTROL : MsgHeader::STANDARD ),
												 m.stamps.getSize(),
												 // stamps-only / invalid messages are marked by (x,y,~0,~0)
												 mValid ? dataSize : ~0,
												 mValid ? numSegs  : ~0 ) );

				multibuf mb;
				mb.iov_base = &m_headers.back();
				mb.iov_len  = sizeof( MsgHeader );
				m_tcpbuffers.push_back(mb); // push header

				mb.iov_base = const_cast<ubyte*>( m.stamps.readAccess() );
				mb.iov_len  = m.stamps.getSize();
				m_tcpbuffers.push_back(mb); // push header for stamps

				if( ! m.data.valid() )
					m_tcppayload.push_back(m); // keep reference to message while we are sending
				else
				{
					m_mpipayload.push_back(m); // keep reference to message while we are sending
					addMultibufFromBuffer( m.data, (msgflags bitand StampList::SYS_FLG_KEEPSEGMENTS ? true:false) );
				}
					

				return true;
			}


			/**
			 * @brief segmented send of buffers
			 * This method sends this block given a socket. It will <b>not</b> clear the block,
			 * so this gives an option, lets say for resending the block, in case we use a different
			 * transport than TCP. The
			 */
			bool sendTCPBlock( int sock, TcpTools::Stats *stats = NULL )
			{
				// send the headers, stamps, and sub-sizes through TCP
				if( TcpTools::multisendBlocks( sock, &m_tcpbuffers[0], m_tcpbuffers.size(), stats ) == false )
					throw TcpTools::SendException( __FILE__, errno );

				return true;
			}

			bool sendMPIBlock( NetMPI::mpiThread * performer, int peerRank )
			{
				// send the messages data itself through MPI
				if ( ! m_mpibuffers.empty() ) {
					performer->enqueue(
						NetMPI::mpiThread::Job::newSend(
									m_mpipayload, m_mpibuffers, peerRank
								)							
					);
				}
				return true;
			}
			
			void clearBlock()
			{
				m_headers.clear();
				m_tcppayload.clear();
				m_tcpbuffers.clear();
				m_mpipayload.clear();
				m_mpibuffers.clear();
				m_mpirequests.clear();
				m_sizes.clear();
			}
		};

	} // anonymous namespace

	// ############################################################################
	// mpiThread
	// ############################################################################
	
	// create a Send job
	NetMPI::mpiThread::Job NetMPI::mpiThread::Job::newSend(
			const std::vector<flowvr::Message> & msgs,
			const std::vector<multibuf> &bufs,
			const int peerRank )
	{
		return Job( msgs, bufs, peerRank );
	}
	NetMPI::mpiThread::Job::Job( const std::vector<flowvr::Message> & msg, const std::vector<multibuf>& bufs, int peerRank )
		: _msgs( msg )
		, _bufs( bufs )
		, _rank( peerRank )
		, _nLeft( _bufs.size() )
		, _msgType()
		, _what( Send )
	{
	}

	// create a Receive job
	NetMPI::mpiThread::Job NetMPI::mpiThread::Job::newRecv(
			const TaggedMessage& msg,
			const std::vector<multibuf> &bufs,
			const int peerRank )
	{
		return Job( msg, bufs, peerRank );
	}
	NetMPI::mpiThread::Job::Job( const TaggedMessage& msg, const std::vector<multibuf>& bufs, int peerRank )
		: _msgs( 1 )
		, _bufs( bufs )
		, _rank( peerRank )
		, _nLeft( _bufs.size() )
		, _msgType( msg.m_bControlMessage )
		, _what( Recv )
	{
		_msgs[0] = msg.m_msg;
	}
	
	void NetMPI::mpiThread::Job::beginIt( const std::vector<MPI_Request*> & jobR  )
	{
		switch( _what )
		{
		case Recv:	return beginRecv( jobR );
		case Send:	return beginSend( jobR );
		}
	}
	void NetMPI::mpiThread::Job::endIt( NetMPI::mpiThread *thread )
	{
		switch( _what )
		{
		case Recv:	return endRecv( thread );
		case Send:  return endSend( thread );
		}
	}
	
	void NetMPI::mpiThread::Job::beginSend( const std::vector<MPI_Request*> & jobR  )
	{
		// send the messages themselves through MPI
		for ( int i = 0 ; i < _nLeft ; ++i  )
		{
			int err =
			MPI_Isend( _bufs[i].iov_base,
					   _bufs[i].iov_len, MPI_UNSIGNED_CHAR,
					   _rank, 42/*MPI_ANY_TAG*/, MPI_COMM_WORLD,
					   jobR[i]
					 );
			
			if ( err )
			{
				char errStr[ MPI_MAX_ERROR_STRING ];
				int errLen;
				if ( !MPI_Error_string( err, errStr, &errLen ) )
					std::cerr << errStr << std::endl;
				throw std::bad_exception();
			}
		}
	}	
	void NetMPI::mpiThread::Job::beginRecv( const std::vector<MPI_Request*> & jobR  )
	{
		// post request to receive the messages themselves through MPI
		for ( int i = 0 ; i < _nLeft ; ++i  )
		{
			int err =
			MPI_Irecv( _bufs[i].iov_base,
					   _bufs[i].iov_len, MPI_UNSIGNED_CHAR,
					   _rank, MPI_ANY_TAG, MPI_COMM_WORLD,
					   jobR[i]
					 );
			
			if ( err )
			{
				char errStr[ MPI_MAX_ERROR_STRING ];
				int errLen;
				if ( !MPI_Error_string( err, errStr, &errLen ) )
					std::cerr << errStr << std::endl;
				throw std::bad_exception();
			}
		}
	}
	
	void NetMPI::mpiThread::Job::endRecv( NetMPI::mpiThread *thread )
	{
		// the buffers have all been received
		switch ( _msgType )
		{
		case MsgHeader::STANDARD:
			thread->m_dispatcher->process( _msgs[0] );
			break;
		case MsgHeader::CONTROL:
			thread->m_dispatcher->processRemoteControlMessage( _msgs[0] );
			break;
		default:
			std::cerr << "NetMPI: Received message with unknown type "
					  << _msgType << std::endl;
		}
	}
	
	NetMPI::mpiThread::mpiThread( NetMPI* parent, Dispatcher* dispatcher )
	: flowvr::Thread()
	, _signalQueue("NetMPI::mpiThread")
	, _lockThread("NetMPI::mpiThread")
	, stop(false)
	, m_parent( parent )
	, m_dispatcher( dispatcher )
	, m_stats()
	, _rank( -1 )
	, _ip2rank()
	{
		start();
		m_stats.m_bTakeTime = parent->getTakeStats();
	}
	
	
	void NetMPI::mpiThread::init_MPI()
	{
		std::cout << "Initalization of MPI within NetMPI started" << std::endl;
		
		// init MPI
		int provided;
		try
		{
			/////////////////////////////////////////////////
			//	Init MPI
			
			std::cout << " MPI_Init_thread\n" << std::endl;
			int err =
			MPI_Init_thread( DaemonInterface::getArgc(),
							 DaemonInterface::getArgv(),
							 MPI_THREAD_MULTIPLE, &provided
							 );
			if ( err ) throw err;
			// check for errors
			if ( provided == MPI_THREAD_SINGLE ) // we need more
			{
				std::cerr << "MPI_THREAD_FUNNELED unsupported\n" << std::endl;
				throw 0;
			}
			
			err = MPI_Comm_rank( MPI_COMM_WORLD, &_rank );
			if ( err ) throw err;
			
			/////////////////////////////////////////////////
			//	prepare the mapping (ip) --> (rank)
			
                        //			std::cout << (_rank?"\t\t\t-":"-") << "MPI_Comm_size\n" << std::endl;
			int worldSize;
			err = MPI_Comm_size( MPI_COMM_WORLD, &worldSize );
			if ( err ) throw err;
			
			// get the number of interfaces
			int nlocal = m_parent->localIp.size();
                        //			std::cout << (_rank?"\t\t\t-":"-") << _rank << " nInterfaces == " << nlocal << std::endl;
			std::vector< int > nInterfaces( worldSize );
			//std::cout << (_rank?"\t\t\t-":"-") << "MPI_Allgather\n" << std::endl;
			err = MPI_Allgather( &nlocal,		  1, MPI_INT,
								 &nInterfaces[0], 1, MPI_INT, MPI_COMM_WORLD );
			if ( err ) throw err;

                        /*
			std::cout << (_rank?"\t\t\t-":"-");
			for ( int rank  ;  rank < worldSize  ;  rank++ ) {
                             std::cout << " " << rank << "-" << nInterfaces[rank] << " ";
			}
			std::cout << std::endl;
                        */
			
			// get our IPs, and store them as strings
			// we use strings to ensure to avoid any endianness problem
			//std::cout << (_rank?"\t\t\t-":"-") << " getStringFromAdress\n" << std::endl;
			std::vector< char > myIPs( nInterfaces[_rank] * INET6_ADDRSTRLEN );
			for ( int i = 0  ;  i < nInterfaces[_rank]  ;  i++  )
			{
				strcpy( &myIPs[ i * INET6_ADDRSTRLEN ],
                                        TcpTools::getStringFromAdress( m_parent->localIp[i].sin_addr ).c_str()  );
			}
			
			// compute sizes/offsets within 'allIPs' in bytes
			//std::cout << (_rank?"\t\t\t-":"-") << " compute sizes/offsets within 'allIPs' in bytes\n" << std::endl;
			std::vector< int > interfacesOffset( worldSize );
			std::vector< int > interfacesSize( worldSize );
			int bufferSize = 0;
			for ( int i = 0, offset = 0  ;  i < worldSize  ;  i++ )
			{
				// current offset
				interfacesOffset[ i ] = offset;
				// buffer size sent by i'th process
				interfacesSize[i] = nInterfaces[i] * INET6_ADDRSTRLEN;
				offset += interfacesSize[i];
				bufferSize += interfacesSize[i];
			}
			
			/////////////////////////////////////////////////
			//	create the mapping (ip) --> (rank)
			
			//std::cout << (_rank?"\t\t\t-":"-") << " MPI_Allgatherv\n" << std::endl;
			// get all the involved IPs
			std::vector< char > allIPs( bufferSize );
			err = MPI_Allgatherv( &myIPs[0],
								  nInterfaces[_rank] * INET6_ADDRSTRLEN, MPI_CHAR,
								  &allIPs[0], &interfacesSize[0], &interfacesOffset[0],
								  MPI_CHAR, MPI_COMM_WORLD );
			if ( err ) throw err;
			
			//std::cout << (_rank?"\t\t\t-":"-") << " create the map itself\n" << std::endl;
			// create the map itself
			for ( int rank = 0, i = 0  ;  rank < worldSize  ;  rank++ )
				for ( int j = 0  ;  j < nInterfaces[rank]  ;  j++ , i++ )
					_ip2rank[ &allIPs[ i * INET6_ADDRSTRLEN ] ] = rank;

		}
		catch ( int err )
		{
			if ( err )
			{
				char errStr[ MPI_MAX_ERROR_STRING ];
				int errLen;
				if ( ! MPI_Error_string( err, errStr, &errLen ) )
					std::cerr << errStr << std::endl;
			}
			close();
			throw std::bad_exception();
		}
		
		
		/////////////////////////////////////////////////
		//	info output
                /*
		for ( std::map< std::string, int >::const_iterator it = _ip2rank.begin()
			  ;	 it != _ip2rank.end()  ;  ++it )
		{
                    std::cout << (_rank?"\t\t\t-":"-") << ' ' << it->first << " --> " << it->second << std::endl;
		}
                std::cout << (_rank?"\t\t\t-":"-") << "Initalization of MPI within NetMPI done\n" << std::endl;
                */
	}

	void NetMPI::mpiThread::close()
	{
		stop = true;
		_signalQueue.notify();

		MPI_Finalize(); // TODO, Is it really ok to do that here?
		
		wait();
	}
	
	/// Main thread function.
	int NetMPI::mpiThread::run()
	{
		if ( -1 == _rank )
			init_MPI();
		
		std::cout << "NetMPI mpiThread ready" << std::endl;
		
		StampList slist;
		
		/// a list of jobs to complete
		TodoList todoList( this );
		
		for (;;)
		{
			// get a few jobs to do
			{
				// claim lock
				ipc::ScopedMTLock locker(_lockThread, "pre-run");
				
				// If there is a pending receive, the \c TodoList will perform a
				// passive wait itself, so you don't need one here.
				// If you have no pending receive, then you wait for a new job.
				// A pending send can't stall the data-flow: it can wait.
//				if ( 0 == todoList.nRecv() )
				// Using MVAPICH2, it seems an immediate receive takes time to
				// complete if the corresponding send request is still pending.
				// Thus, wait is only performed using MPI_Waitsome.
				// \see \c NetMPI::mpiThread::TodoList::doIt
				if ( todoList.empty() )
					while ( _newJobs.empty() && !stop )
						_signalQueue.wait(_lockThread);

				// normally, we own lockThread now

				if (stop) // user gave stop signal
					break;

				// splice the new jobs to the todo-list
				todoList.splice( _newJobs );
			}
			
			// lock is released... get to work
			todoList.doIt();
		}

		// leave while-loop
		std::cout << "NetMPI mpiThread stopped"
				  << std::endl;

		return 0;
	}
	
	int NetMPI::mpiThread::getRank( in_addr addr ) const
	{
		// get a string of the adress
		const std::string addr_str( TcpTools::getStringFromAdress( addr ) );
		// look for this address in the table created at initialization
		const std::map< std::string, int >::const_iterator it = _ip2rank.find( addr_str );
		// return result
		if ( _ip2rank.end() != it )
			return it->second;
		std::cerr << "Error in NetMPI plugin:"
				  << " Trying to get rank associed with the unknown ip address:"
				  << " \"" <<  addr_str << "\"" << std::endl;
		return -1;
	}
			
	void NetMPI::mpiThread::enqueue( const Job& job )
	{
	#ifdef DEBUG
	// TODO: fix error: ‘ip’ was not declared in this scope
            // std::cout << "Queueing message to "<<TcpTools::getHostFromAddress(ip)<<std::endl;
	#endif
	  ipc::ScopedMTLock locker(_lockThread,"enqueue");

	  _newJobs.push_back( job );
	  _signalQueue.notify();
	}
	
	
	void NetMPI::mpiThread::splice( std::list< Job > & jobs )
	{
	#ifdef DEBUG
	// TODO: fix error: ‘ip’ was not declared in this scope
    //        std::cout << "Queueing message to "<<TcpTools::getHostFromAddress(ip)<<std::endl;
	#endif
	  ipc::ScopedMTLock locker(_lockThread,"enqueue");

	  _newJobs.splice( _newJobs.end(), jobs );
	  _signalQueue.notify();
	}
	
	// ############################################################################
	// SendThread
	// ############################################################################

	NetMPI::SendThread::SendThread(NetMPI* _parent, struct sockaddr_in _ip, int _sock)
	: flowvr::Thread()
	, signalQueue("NetMPI::SendThread")
	, lockThread("NetMPI::SendThread")
	, stop(false)
	, parent(_parent)
	, ip(_ip)
	, sock(_sock)
	, m_stats()
	{
	  if (sock!=-1)
		  start();

	  m_stats.m_bTakeTime = parent->getTakeStats();
	}

	void NetMPI::SendThread::close()
	{
		if (sock != -1)
		{
			stop = true;
			signalQueue.notify();

			wait();
		}

		::close( sock );
	}

	/// Main thread function.
	int NetMPI::SendThread::run()
	{
		std::cout << "NetMPI Sender to " << TcpTools::getHostFromAddress(ip) << " ready" << std::endl;

		SendBlock block;
		StampList slist;

		// get rank of the peer
		_peerRank = parent->mpiPerformer->getRank( ip.sin_addr );
//		// send our own rank to the peer
//		int32_t rank = parent->mpiPerformer->getRank();
//		TcpTools::sendBlock(sock, &rank, sizeof(rank), NULL);
		
		if ( _peerRank != -1 )
		for (;;)
		{
			int nmsg = 0;
			int nbuf = 0;
			{
				// claim lock
				ipc::ScopedMTLock locker(lockThread, "run");
				while (msgQueue.empty() && !stop)
					signalQueue.wait(lockThread);

				// normally, we own lockThread now

				if (stop)
					break; // user gave stop signal

				// proceed with message sending...

				// we need to lock the queue only as long as
				// we are taking off messages from it
				while (!msgQueue.empty() && nmsg < TCPMAXMSG)
				{
					// we pop off only MAXMSG messages. The idea is to
					// open the queue again with an upper bound on message that we will send
					// otherwise, we bulk in many messages that need to be send (takes a long time)
					// so ideally, we can a bit interleave message production and sending.
					// I guess that this is a coarse heuristic, but we give it a try.

					bool control = msgQueue.front().m_bControlMessage;
					Message m    = msgQueue.front().m_msg;
					msgQueue.pop_front(); // dispose msg from queue

					// extract system flags
					int flags = 0;
					m.stamps.read( slist.sysflags, flags );

					// add this message to the whole block that is to be sent
					block.addMessageToBlock( m, control, flags );
				}
			}

			try
			{
				if( block.empty() )
					continue;
				
				// send the headers, stamps, and sub-sizes through TCP
				if(!block.sendTCPBlock( this->getSocket(), &this->m_stats ) )
					break;
				
				// send the data itself through MPI
				if( !block.sendMPIBlock( this->parent->mpiPerformer, _peerRank ) )
					break;
				
				// dispose old references and stuff
				block.clearBlock();
			}
			catch( TcpTools::SendException & e )
			{
				e.print();
				break;
			}
		}

		// leave while-loop
		std::cout << "NetMPI Sender to " << TcpTools::getHostFromAddress(ip) << " stopped"
				  << std::endl;

		// close socket
		::close(sock);
		return 0;
	}

	void NetMPI::SendThread::enqueue(const flowvr::Message& msg, bool control)
	{
	#ifdef DEBUG
            std::cout << "Queueing message to "<<TcpTools::getHostFromAddress(ip)<<std::endl;
	#endif
	  ipc::ScopedMTLock locker(lockThread,"enqueue");

	  TaggedMessage    newmsg;
	  newmsg.m_bControlMessage = control;
	  newmsg.m_msg             = msg;


	  msgQueue.push_back(newmsg);
	  signalQueue.notify();
	}

	// ##########################################################################
	// RecvThread
	// ##########################################################################
	NetMPI::RecvThread::RecvThread(NetMPI *parent, Dispatcher* _dispatcher, int _sock, struct sockaddr_in _ip)
	  : flowvr::Thread()
	, dispatcher(_dispatcher)
	, stop(false)
	, sock(_sock)
	, ip(_ip)
	, m_parent(parent)
	{
		if( sock != -1 )
		  start();

		this->m_stats.m_bTakeTime = parent->getTakeStats();
	}


	void NetMPI::RecvThread::close()
	{
		if( sock == -1 )
		  return;

		stop = true;

		terminate();
		wait();

		::close(sock);
	}


	Buffer NetMPI::RecvThread::receiveBuffer( std::vector<multibuf> &bufs, bool keepsegments)
	{
		// receive data size first
		size_t    bufSize = ~0; // read 1) totalsize
		if( !TcpTools::receiveBlock(sock, &bufSize, sizeof(size_t), &m_stats ) )
		{
			// read error on socket!
			// should normally throw, but does not seem to be the case
			return Buffer();
		}


		BufferWrite b = Allocator::getAllocator()->alloc(bufSize);
		if(!b.valid())
			throw std::bad_alloc();

		size_t numSegments = ~0;
		if( !TcpTools::receiveBlock(sock, &numSegments, sizeof(size_t), &m_stats ) )
			throw std::exception();
		bufs.resize( numSegments );

		// we create a conditional return value here (we return it in case we keep segments)
		Buffer r;
		size_t offset=0;
		for( size_t n = 0; n < numSegments; ++n )
		{
			// receive size of the segment with TCP
			size_t segmentSize = ~0;
			TcpTools::receiveBlock(sock, &segmentSize, sizeof(size_t), &m_stats );
			
			// the segment itself will be received with MPI
			bufs[n].iov_base = b.getWrite<void>(offset);
			bufs[n].iov_len = segmentSize;
			//TcpTools::receiveBlock(sock, b.getWrite<void>(offset), segmentSize, &m_stats );

			if( keepsegments )
			{
				// add a new segment to buffer r
				r += Buffer( b, offset, segmentSize );
			}

			// advance the offset in the storage array
			offset += segmentSize;
		}

		if( !keepsegments )
			return b; // return the one big block
		else
			return r;
	}

	int NetMPI::RecvThread::run()
	{
		// get rank of the peer
//			int32_t rank;
//			TcpTools::receiveBlock(sock, &rank, sizeof(rank), NULL);
//			_peerRank = rank;
		_peerRank = m_parent->mpiPerformer->getRank( ip.sin_addr );
		if ( _peerRank != -1 )
		try
		{
			std::cout << "NetMPI Receiver ready" << std::endl;
			StampList slist; // need that for stamp-access later
			while (!stop)
			{
				// first thing to expect is a MsgHeader (always)
				MsgHeader header;

				// always take time on receiving data
				m_stats.m_bTakeTime = true;
				if (!TcpTools::receiveBlock(sock, &header, sizeof(header), &m_stats))
					break;
				m_stats.m_bTakeTime = false;

				flowvr::utils::microtime n0 = flowvr::utils::getNtpTimeStamp();

				// now load the message followed by the header.
				MessageWrite msg;

				BufferWrite buffer = Allocator::getAllocator()->alloc(header.stamps_size);
				if( !buffer.valid() )
					throw std::bad_alloc();

				if (!TcpTools::receiveBlock(sock, buffer.writeAccess(), header.stamps_size, &m_stats))
					break;

				// alloc stamps using window constructor
				msg.stamps = BufferWrite(buffer, 0, header.stamps_size);

				Message disp;
				std::vector<multibuf> bufs;
				// compose the final message passed to the dispatcher
				// as a normal Message (remove write-privileges)
				disp.stamps = msg.stamps; // assign stamps

				// process the below only for FULL / valid() messages
				if( !(header.data_size == ~0 and header.num_segments == ~0 ) )
				{
					bool keepsegments = (header.data_size == ~0 ? true : false);
					if( keepsegments == false )
					{
						// sending buffer was not asked to merge the segments
						// could be that this is just a 'normal' / unsegmented buffer, or
						// a segmented buffer that was merged.
						// reald the 'old' flags
						int flags;
						msg.stamps.read( slist.sysflags, flags );
						if( flags bitand StampList::SYS_FLG_KEEPSEGMENTS )
						{
							// we have lost the segments, so re-set the flag on the buffer.
							StampsWrite w(msg.stamps);
							w.write( slist.sysflags, flags & ~StampList::SYS_FLG_KEEPSEGMENTS );
						}
					}
					disp.data = receiveBuffer( bufs, keepsegments ); // create / allocate / fill and assign buffer

					this->m_parent->mpiPerformer->enqueue(
							NetMPI::mpiThread::Job::newRecv(
								TaggedMessage(disp, header.type), bufs, _peerRank
								)
							);
				} else {
					switch (header.type)
					{
					case MsgHeader::STANDARD:
						dispatcher->process(disp);
						break;
					case MsgHeader::CONTROL:
						dispatcher->processRemoteControlMessage(disp);
						break;
					default:
						std::cerr << "NetMPI: Received message with unknown type "
								  << header.type << std::endl;
					}
				}

				flowvr::utils::microtime n1 = flowvr::utils::getNtpTimeStamp();
				m_stats.m_dTimeNeeded += (n1-n0);

			}
		} // try
		catch( TcpTools::ReceiveException &e )
		{
			e.print();
		}
		catch( std::exception &x )
		{
			std::cerr << "Caught std::exception() -- " << x.what() << std::endl;
		}

		std::cout << "NetMPI Receiver from " << TcpTools::getHostFromAddress(ip) << " stopped" << std::endl;

		m_parent->remRevcThread(this);
		return 0;
	}


	// ##########################################################################
	// StateThread
	// ##########################################################################
	  NetMPI::StateThread::StateThread( NetMPI *parent )
	  : flowvr::Thread()
	  , m_parent(parent)
	  {
	  }

	  int NetMPI::StateThread::run()
	  {
		  return m_parent->run();
	  }

	// ##########################################################################
	// SendActionHandler
	// ##########################################################################
	NetMPI::SendActionHandler::SendActionHandler(NetMPI* _parent, struct sockaddr_in _ip, int _sock)
	: ActionHandler()
	, nbref(1)
	, parent(_parent)
	, ip(_ip)
	, queue(_parent, _ip, _sock)
	{
	}

	void NetMPI::SendActionHandler::close()
	{
		queue.close();
	}

	bool NetMPI::SendActionHandler::operator==( const SendActionHandler &other ) const
	{
		return (*this == other.ip);
	}

	bool NetMPI::SendActionHandler::operator==( const sockaddr_in &other ) const
	{
		return (this->ip.sin_addr.s_addr == other.sin_addr.s_addr) and (this->ip.sin_port == other.sin_port);
	}

	void NetMPI::SendActionHandler::doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
	{
		enqueue(msg,false);
	}

	void NetMPI::SendActionHandler::remove()
	{
		if (nbref<=0)
		  return;

		if( nbref.dec_and_test_null() )
		  delete this;
	}


	void NetMPI::SendActionHandler::enqueue(const flowvr::Message& msg, bool control)
	{
		queue.enqueue(msg,control);
	}

	/// Create an ActionHandler for batch mode action execution.
	ActionHandler* NetMPI::SendActionHandler::createAction(xml::DOMElement* xmlRoot)
	{
	  if (nbref==0)
		  return NULL;

	  ++nbref;
	  return this;
	}

	// ##########################################################################
	// ControlActionHandler
	// ##########################################################################

	NetMPI::ControlActionHandler::ControlActionHandler(NetMPI* _parent)
	  : ActionHandler()
	, lock("NetMPI::ControlActionHandler")
	, nbref(1)
	, parent(_parent)
	{}

	ActionHandler* NetMPI::ControlActionHandler::createAction(xml::DOMElement* xmlRoot)
	{
		if (nbref==0)
		  return NULL;

		++nbref;
		return this;
	}

	void NetMPI::ControlActionHandler::doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
	{
	  ipc::ScopedMTLock locker(lock,"doIt");
	  if (parent!=NULL)
		parent->processControlMessage(msg, dispatcher);
	}

	void NetMPI::ControlActionHandler::remove()
	{
	  if (nbref<=0)
		  return;

	  if( nbref.dec_and_test_null() )
		  delete this;
	}

	// ##########################################################################
	// NetMPI
	// ##########################################################################



		GenClass<NetMPI> NetMPIClass("flowvr.plugins.NetMPI", "" );

		/// Constructor.
		NetMPI::NetMPI(const std::string &objID)
		  : Object(objID)
		, lockSenders("NetMPI.lockSenders")
		, lockReceivers("NetMPI.lockReceivers")
		, stateThread(this)
		, m_DaemonPort(0)
		, m_acceptSocket(0)
		, threadDispatcher(NULL)
		{
		}


		NetMPI::~NetMPI()
		{
		}

		Class* NetMPI::getClass() const
		{
		  return &NetMPIClass;
		}

		Result NetMPI::init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
		{
			try
			{
				Result res = Object::init(xmlRoot, dispatcher);
				if (res.error())
					return res;

				TcpTools::getIPAddresses( localIp );

				int ID = localIp.empty() ? 0 : localIp[0].sin_addr.s_addr;

				DaemonInterface::setHostName( TcpTools::getNodeHostName(), ID );

				std::string param = xmlRoot->getTextContent();
				if(!param.empty())
				{
					m_DaemonPort = atoi( param.c_str() );
				}
				else
					m_DaemonPort = DaemonInterface::getDaemonID();

				sockaddr_in any;
				TcpTools::createListenOnAllAddresses(any, m_DaemonPort);

				m_acceptSocket = TcpTools::createTcpServerSocket( (sockaddr&)any, true, 64, false );

				//copy of the dispatcher
				threadDispatcher     = dispatcher->threadCopy();
				controlActionHandler = new ControlActionHandler(this);

				stateThread.start();
				
				// create mpi thread
				mpiPerformer = new mpiThread( this, threadDispatcher );

				return res;
			}
			catch( TcpTools::TcpException &e )
			{
				e.print();
			}

			return Result::ERROR;
		}

		///NetMPI object awaits to create new connections
		int NetMPI::run()
		{
			try
			{
			  int s;
			  sockaddr_in src;
			  while( (s=TcpTools::acceptNextTcpClient(m_acceptSocket, src)) )
			  {
				std::string source = TcpTools::getHostFromAddress(src);
				std::cout<<"Accepting new connection from "<<source<<std::endl;
				ipc::ScopedMTLock locker(lockReceivers,"run");
				RecvThread* recvT = new RecvThread(this, threadDispatcher->threadCopy(), s, src);
				receivers.push_back(recvT);

				recvT->detach(); // we will not clean up when it ends, so let's collect this thread
			  }
			}
			catch( TcpTools::TcpException &e )
			{
				e.print();
			}


		  std::cout << "Accept thread terminates." << std::endl;
		  return 0;
		}


		/// Create an ActionHandler for batch mode action execution.
		ActionHandler* NetMPI::createAction(xml::DOMElement* xmlRoot)
		{
			xml::DOMElement* child = xmlRoot->FirstChildElement();
			if (child == NULL)
				return NULL;

			if (!strcmp(child->getNodeName(), "dest"))
			{
				// standard message send operation
				std::string hostname = child->getTextContent();
				struct sockaddr_in destip;
				if (!TcpTools::getNodeAddress(hostname, destip))
				{
					std::cerr << "NetMPI::createAction: getNodeIp failed" << std::endl;
					return NULL;
				}

				if (TcpTools::isLocal(destip, localIp))
				{
					std::cout << "NetMPI: detected connection to myself." << std::endl;
					return ActionHandler::LoopBackActionHandler;
				}

				destip.sin_port = htons(m_DaemonPort);

				SendActionHandler* sender = getSendActionHandler(destip);

				if (sender == NULL)
				{
					std::cerr << "NetMPI::createAction: getSendActionHandler for ["
							<< TcpTools::getHostNameFromAddress(destip) << "] on port [" << m_DaemonPort << "] failed" << std::endl;
					return NULL;
				}

				return sender->createAction(xmlRoot);
			}
			else if (!strcmp(child->getNodeName(), "control"))
			{
				// control message send operation
				return controlActionHandler->createAction(xmlRoot);
			}
			else
			{
				std::cerr << "NetMPI::createAction: Unknown action" << std::endl;
				return NULL;
			}
		}

		void NetMPI::processControlMessage(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
		{
		  // we must read the dest from the message stamps
		  static StampListControl stamps;
		  std::string hostname;
		  msg.stamps.read(stamps.dest,hostname);

		  if (flowvr::daemon::verboseLevel>=1)
			std::cout << "NetMPI: control message dest="<<hostname<<std::endl;

		  sockaddr_in destip;

		  if ( !TcpTools::getNodeAddress(hostname, destip) )
			return;

		  if (flowvr::daemon::verboseLevel>=1)
			std::cout << "NetMPI: control message ip=" << TcpTools::getHostFromAddress(destip) << std::endl;

		  if (TcpTools::isLocal(destip, localIp))
		  {
			// This message is for the  local daemon. Redispatch it as if it
			// was remotely received.
			dispatcher->processRemoteControlMessage(msg);
		  }
		  else
		  {
			destip.sin_port = htons( m_DaemonPort );
			SendActionHandler* sender = getSendActionHandler(destip);
			if (sender==NULL)
			{
			  std::cerr << "NetMPI::processControlMessage: getSendActionHandler failed"<<std::endl;
			  return;
			}
			sender->enqueue(msg,true);
		  }
		}

		/// Find or Create an SendActionHandler given the dest ip
		NetMPI::SendActionHandler* NetMPI::getSendActionHandler(sockaddr_in &dest)
		{
			if ( TcpTools::isLocal(dest, localIp) )
			{
				std::cerr << "NetMPI: detected connection to myself."<<std::endl;
				return NULL; // do not create connection
			}

			// else check if an outgoing connection already exists
			{
				ipc::ScopedMTLock locker(lockSenders,"getSendActionHandler");
				std::vector<SendActionHandler*>::const_iterator it = std::find_if( senders.begin(), senders.end(), _equals( dest ) );
				if( it != senders.end() )
					return *it;
			}

			try
			{
				// log connection
				std::cout << "Connecting to "<< TcpTools::getHostFromAddress(dest) << std::endl;
				// create a new sender
				int sock;

				sock = TcpTools::connectTo( dest, true );

				ipc::ScopedMTLock locker(lockSenders,"getSendActionHandler:2");
				SendActionHandler* sender = new SendActionHandler(this, dest, sock);
				senders.push_back(sender);
				return sender;
			}
			catch( TcpTools::TcpException &e )
			{
				e.print();
			}
			return NULL;
		}

		bool NetMPI::remRevcThread( RecvThread *thr )
		{
			ipc::ScopedMTLock locker(lockReceivers,"remRecvThread");
			receivers.erase(std::remove( receivers.begin(), receivers.end(), thr ), receivers.end() );
			toClaim.push_back(thr); // can/should dispose memory later
			return true;
		}

		Result NetMPI::doAction(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
		{
			// get command
			xml::DOMElement *cmd = xmlRoot->FirstChildElement("cmd");
			if( cmd )
			{
				std::string id = notNULL(cmd->Attribute("id"));
				if( id.empty() )
					return Result::ERROR;

				if( id == "togglestats")
				{
					toggleStats();
					return Result( Result::OK, getTakeStats() ? "Statistics enabled" : "Statistics disabled" );
				}
				else if( id == "sendstats" )
				{
					// create stats
					std::string sendstats = createSendStatsString();
					return Result( Result::OK, sendstats );
				}
				else if( id == "recvstats" )
				{
					// create stats
					std::string sendstats = createRecvStatsString();
					return Result( Result::OK, sendstats );
				}
				else if( id == "connections")
				{
					// create stats
					std::string sendstats = createConnectionsString();
					return Result( Result::OK, sendstats );
				}
			}

			return Result::OK;
		}

		  bool NetMPI::toggleStats()
		  {
			  bTakeStats = !bTakeStats;
			  std::for_each( senders.begin(), senders.end(), _setStats<SendActionHandler>( bTakeStats ) );
			  std::for_each( receivers.begin(), receivers.end(), _setStats<RecvThread>(bTakeStats) );

			  return bTakeStats;
		  }

		  bool NetMPI::getTakeStats() const
		  {
			  return bTakeStats;
		  }

		  std::string NetMPI::createSendStatsString() const
		  {
			  _createStatsString<SendActionHandler> s;
			  s = std::for_each( senders.begin(), senders.end(), s );
			  return s.m_text;
		  }

		  std::string NetMPI::createRecvStatsString() const
		  {
			  _createStatsString<RecvThread> s;
			  s = std::for_each( receivers.begin(), receivers.end(), s );
			  return s.m_text;
		  }

		  std::string NetMPI::createConnectionsString() const
		  {
			  std::string res("\nSending to:\n");

			  _createConString<SendActionHandler> s;
			  s = std::for_each( senders.begin(), senders.end(), s );

			  res += s.m_text;

			  _createConString<RecvThread> sR;
			  sR = std::for_each( receivers.begin(), receivers.end(), sR );

			  res += std::string("\nReceiving from:\n") + sR.m_text;

			  return res;
		  }

	} // namespace plugins

} // namespace flowvr

