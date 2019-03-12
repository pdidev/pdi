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
* File: src/plugins/flowvr.plugins.NetTCP.cpp                     *
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


#include <deque>
#include <sys/types.h>
#include <vector>
#include <limits.h>
#include <functional>
#include <algorithm>
#include <sstream>


#define MAXMSG  IOV_MAX/3

#define notNULL(A) A == NULL ? std::string("") : std::string(A)


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
	 * @brief the NetTCP plugin to the daemon.
	 * It encapsulates the send and receive protocol for messages sent from one host to
	 * one another on a different IP.
	 * In case a message is detected to be for the host this NetTCP object is running on
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
	class NetTCP : public flowvr::plugd::Object
	{
	public:
	  NetTCP(const std::string &objID);
	  virtual ~NetTCP();

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

	  class RecvThread;
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

	  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  // Helper classes, threads for sending and receiving
	  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	  // ---------------------------------------------------------------------
	  // SendThread
	  // ---------------------------------------------------------------------
	  class SendThread : public flowvr::Thread
	  {
	  public:
		SendThread(NetTCP* _parent, struct sockaddr_in _ip, int _sock);


		std::deque<TaggedMessage> msgQueue;
		ipc::MTSignal         signalQueue;
		ipc::MTLock           lockThread;

		struct sockaddr_in ip;
		NetTCP             *parent;

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
		RecvThread(NetTCP *parent, Dispatcher* _dispatcher, int _sock, struct sockaddr_in _ip);
		void close();

		Dispatcher* dispatcher;
		bool stop;
		int  sock;
		struct sockaddr_in ip;

		NetTCP *m_parent;

		TcpTools::Stats m_stats;
	  protected:
		virtual int run();
		Buffer receiveBuffer(bool keepSegments);
	  };

	  // ---------------------------------------------------------------------
	  // StateThread
	  // ---------------------------------------------------------------------
	  class StateThread : public flowvr::Thread
	  {
	  public:
		  StateThread( NetTCP *parent );

	  protected:
		  virtual int run();
		  NetTCP *m_parent;
	  };

	  // ---------------------------------------------------------------------
	  // SendActionHandler
	  // ---------------------------------------------------------------------
	  class SendActionHandler : public ActionHandler
	  {
	  public:
		SendActionHandler(NetTCP* _parent, struct sockaddr_in _ip, int _sock, int _sock2);

		ipc::MTAtomicInt   nbref;
		NetTCP            *parent;
		struct sockaddr_in ip;

		SendThread queueFull;
		SendThread queueStamps;

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
		ControlActionHandler(NetTCP* _parent);

		ipc::MTLock      lock;
		ipc::MTAtomicInt nbref;
		NetTCP     *parent;

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
	void _setStats<NetTCP::SendActionHandler>::operator()( NetTCP::SendActionHandler *el ) const
	{
		el->queueFull.m_stats.m_bTakeTime = m_val;
		el->queueStamps.m_stats.m_bTakeTime = m_val;
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
	void _createConString<NetTCP::SendActionHandler>::operator()( NetTCP::SendActionHandler *el )
	{
		m_text += (TcpTools::getHostFromAddress( el->ip ) + std::string("\n")
		       + TcpTools::getHostFromAddress( el->queueFull.ip ) + std::string("\n")
		       + TcpTools::getHostFromAddress( el->queueStamps.ip ) + std::string("\n") );
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
	void _createStatsString<NetTCP::SendActionHandler>::operator()( NetTCP::SendActionHandler *el )
	{
		std::stringstream in;
		in << "\n" << TcpTools::getHostFromAddress(el->ip) << "\n"
			   << "Full queue:\n"
		       << "Total time : " << el->queueFull.m_stats.m_dTimeNeeded << "\n"
			   << "Data size  : " << el->queueFull.m_stats.m_dataSize    << "\n"
			   << "Num batches: " << el->queueFull.m_stats.m_nNumBatches << "\n"
			   << "Stamps queue:\n"
			   << "Total time : " << el->queueStamps.m_stats.m_dTimeNeeded << "\n"
			   << "Data size  : " << el->queueStamps.m_stats.m_dataSize    << "\n"
			   << "Num batches: " << el->queueStamps.m_stats.m_nNumBatches << "\n";
		m_text += in.str();
	}

//################################################################################
// IMPLEMENTATION BLOCK
//################################################################################

	namespace
	{
		/**
		 * @brief little helper for find a SendActionHandler based on an IP
		 * @private
		 */
		class _equals : public std::unary_function<const NetTCP::SendActionHandler*, void>
		{
		public:
			_equals( const sockaddr_in &_me )
			: me(_me) {}

			/**
			 * @brief uses the comparison operator define in SendActionHandler
			 * @see SendActionHandler::operator==
			 */
			bool operator()( const NetTCP::SendActionHandler *other ) const
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
				m_headers.reserve(MAXMSG);  // we can give a bound on the headers by the number of
				                            // MAXMSG, as we are not going to work on more than that
				                            // per iteration
			}

			std::vector<multibuf>        m_buffers; /**< keep list of multibuffers to send */
			std::vector<MsgHeader>       m_headers; /**< keep a list of MsgHeaders for each message */
			std::vector<flowvr::Message> m_payload; /**< keep a reference to each message that is to be sent */

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
				m_buffers.push_back(mb);

				// send number of segments
				m_sizes.push_back( buffer.getNumberOfSegments() );
				mb.iov_base = &m_sizes.back();
				mb.iov_len  = sizeof(size_t);
				m_buffers.push_back(mb);

				const std::vector<Buffer::_bufferDesc> &segments = buffer.getSegments();
				for( size_t n = 0; n < buffer.getNumberOfSegments(); ++n )
				{
					const Buffer::_bufferDesc &segment = segments[n];
					m_sizes.push_back( segment.size );
					mb.iov_base = &m_sizes.back();
					mb.iov_len  = sizeof(size_t);
					m_buffers.push_back(mb);

					mb.iov_base = (void*)segment.pointer;
					mb.iov_len  = segment.size;
					m_buffers.push_back(mb);
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
				m_payload.push_back(m); // keep reference to message while we are sending

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
				m_buffers.push_back(mb); // push header

				mb.iov_base = (void*)m.stamps.readAccess();
				mb.iov_len  = m.stamps.getSize();
				m_buffers.push_back(mb); // push header for stamps

				if( m.data.valid() )
					addMultibufFromBuffer( m.data, (msgflags bitand StampList::SYS_FLG_KEEPSEGMENTS ? true:false) );

				return true;
			}


			/**
			 * @brief segmented send of buffers
			 * This method sends this block given a socket. It will <b>not</b> clear the block,
			 * so this gives an option, lets say for resending the block, in case we use a different
			 * transport than TCP. The
			 */
			bool sendBlock( int sock, TcpTools::Stats *stats = NULL )
			{
				if( TcpTools::multisendBlocks( sock, &m_buffers[0], m_buffers.size(), stats ) == false )
					throw TcpTools::SendException( __FILE__, errno );

				return true;
			}

			void clearBlock()
			{
				m_payload.clear();
				m_buffers.clear();
				m_headers.clear();
				m_sizes.clear();
			}
		};

	} // anonymous namespace

	// ############################################################################
	// SendThread
	// ############################################################################

	NetTCP::SendThread::SendThread(NetTCP* _parent, struct sockaddr_in _ip, int _sock)
	: flowvr::Thread()
	, signalQueue("NetTCP::SendThread")
	, lockThread("NetTCP::SendThread")
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

	void NetTCP::SendThread::close()
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
	int NetTCP::SendThread::run()
	{
		std::cout << "NetTCP Sender to " << TcpTools::getHostFromAddress(ip) << " ready" << std::endl;

		SendBlock block;
		StampList slist;

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
				while (!msgQueue.empty() && nmsg < MAXMSG)
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
				if( block.m_buffers.empty() )
					continue;

				// send the whole block
				if(!block.sendBlock( this->getSocket(), &this->m_stats ) )
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
		std::cout << "NetTCP Sender to " << TcpTools::getHostFromAddress(ip) << " stopped"
				  << std::endl;

		// close socket
		::close(sock);
		return 0;
	}

	void NetTCP::SendThread::enqueue(const flowvr::Message& msg, bool control)
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
	NetTCP::RecvThread::RecvThread(NetTCP *parent, Dispatcher* _dispatcher, int _sock, struct sockaddr_in _ip)
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


	void NetTCP::RecvThread::close()
	{
		if( sock == -1 )
		  return;

		stop = true;

		terminate();
		wait();

		::close(sock);
	}


	Buffer NetTCP::RecvThread::receiveBuffer(bool keepsegments)
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

		// we create a conditional return value here (we return it in case we keep segments)
		Buffer r;
		size_t offset=0;
		for( size_t n = 0; n < numSegments; ++n )
		{
			size_t segmentSize = ~0;
			TcpTools::receiveBlock(sock, &segmentSize, sizeof(size_t), &m_stats );
			TcpTools::receiveBlock(sock, b.getWrite<void>(offset), segmentSize, &m_stats );

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

	int NetTCP::RecvThread::run()
	{
		std::cout << "NetTCP Receiver ready" << std::endl;
		try
		{
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
							w.write( slist.sysflags, flags bitand ~StampList::SYS_FLG_KEEPSEGMENTS );
						}
					}
					disp.data   = receiveBuffer(keepsegments); // create / allocate / fill and assign buffer
				}

				flowvr::utils::microtime n1 = flowvr::utils::getNtpTimeStamp();
				m_stats.m_dTimeNeeded += (n1-n0);

				switch (header.type)
				{
				case MsgHeader::STANDARD:
					dispatcher->process(disp);
					break;
				case MsgHeader::CONTROL:
					dispatcher->processRemoteControlMessage(disp);
					break;
				default:
					std::cerr << "NetTCP: Received message with unknown type "
							  << header.type << std::endl;
				}
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

		std::cout << "NetTCP Receiver from " << TcpTools::getHostFromAddress(ip) << " stopped" << std::endl;

		m_parent->remRevcThread(this);
		return 0;
	}


	// ##########################################################################
	// StateThread
	// ##########################################################################
	  NetTCP::StateThread::StateThread( NetTCP *parent )
	  : flowvr::Thread()
	  , m_parent(parent)
	  {
	  }

	  int NetTCP::StateThread::run()
	  {
		  return m_parent->run();
	  }

	// ##########################################################################
	// SendActionHandler
	// ##########################################################################
	NetTCP::SendActionHandler::SendActionHandler(NetTCP* _parent, struct sockaddr_in _ip, int _sock, int _sock2)
	: ActionHandler()
	, nbref(1)
	, parent(_parent)
	, ip(_ip)
	, queueFull(_parent, _ip, _sock)
	, queueStamps(_parent, _ip, _sock2)
	{
	}

	void NetTCP::SendActionHandler::close()
	{
	  queueFull.close();
	  queueStamps.close();
	}

	bool NetTCP::SendActionHandler::operator==( const SendActionHandler &other ) const
	{
		return (*this == other.ip);
	}

	bool NetTCP::SendActionHandler::operator==( const sockaddr_in &other ) const
	{
		return (this->ip.sin_addr.s_addr == other.sin_addr.s_addr) and (this->ip.sin_port == other.sin_port);
	}

	void NetTCP::SendActionHandler::doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
	{
		enqueue(msg,false);
	}

	void NetTCP::SendActionHandler::remove()
	{
		if (nbref<=0)
		  return;

		if( nbref.dec_and_test_null() )
		  delete this;
	}


	void NetTCP::SendActionHandler::enqueue(const flowvr::Message& msg, bool control)
	{
		if ((control || !msg.data.valid()) && queueStamps.sock!=-1)
			queueStamps.enqueue(msg,control);
		else
			queueFull.enqueue(msg,control);
	}

	/// Create an ActionHandler for batch mode action execution.
	ActionHandler* NetTCP::SendActionHandler::createAction(xml::DOMElement* xmlRoot)
	{
	  if (nbref==0)
		  return NULL;

	  ++nbref;
	  return this;
	}

	// ##########################################################################
	// ControlActionHandler
	// ##########################################################################

	NetTCP::ControlActionHandler::ControlActionHandler(NetTCP* _parent)
	  : ActionHandler()
	, lock("NetTCP::ControlActionHandler")
	, nbref(1)
	, parent(_parent)
	{}

	ActionHandler* NetTCP::ControlActionHandler::createAction(xml::DOMElement* xmlRoot)
	{
		if (nbref==0)
		  return NULL;

		++nbref;
		return this;
	}

	void NetTCP::ControlActionHandler::doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
	{
	  ipc::ScopedMTLock locker(lock,"doIt");
	  if (parent!=NULL)
		parent->processControlMessage(msg, dispatcher);
	}

	void NetTCP::ControlActionHandler::remove()
	{
	  if (nbref<=0)
		  return;

	  if( nbref.dec_and_test_null() )
		  delete this;
	}

	// ##########################################################################
	// NetTCP
	// ##########################################################################



		GenClass<NetTCP> NetTCPClass("flowvr.plugins.NetTCP", "" );

		/// Constructor.
		NetTCP::NetTCP(const std::string &objID)
		  : Object(objID)
		, lockSenders("NetTCP.lockSenders")
		, lockReceivers("NetTCP.lockReceivers")
		, stateThread(this)
		, m_DaemonPort(0)
		, m_acceptSocket(0)
		, threadDispatcher(NULL)
		{
		}


		NetTCP::~NetTCP()
		{
		}

		Class* NetTCP::getClass() const
		{
		  return &NetTCPClass;
		}

		Result NetTCP::init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
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

                std::cout << "Using TCP port : " << m_DaemonPort << std::endl;

				sockaddr_in any;
				TcpTools::createListenOnAllAddresses(any, m_DaemonPort);

				m_acceptSocket = TcpTools::createTcpServerSocket( (sockaddr&)any, true, 64, false );

				//copy of the dispatcher
				threadDispatcher     = dispatcher->threadCopy();
				controlActionHandler = new ControlActionHandler(this);

				stateThread.start();

				return res;
			}
			catch( TcpTools::TcpException &e )
			{
				e.print();
			}

			return Result::ERROR;
		}

		///NetTCP object awaits to create new connections
		int NetTCP::run()
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
		ActionHandler* NetTCP::createAction(xml::DOMElement* xmlRoot)
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
					std::cerr << "NetTCP::createAction: getNodeIp failed" << std::endl;
					return NULL;
				}

				if (TcpTools::isLocal(destip, localIp))
				{
					std::cout << "NetTCP: detected connection to myself." << std::endl;
					return ActionHandler::LoopBackActionHandler;
				}

				destip.sin_port = htons(m_DaemonPort);

				SendActionHandler* sender = getSendActionHandler(destip);

				if (sender == NULL)
				{
					std::cerr << "NetTCP::createAction: getSendActionHandler for ["
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
				std::cerr << "NetTCP::createAction: Unknown action" << std::endl;
				return NULL;
			}
		}

		void NetTCP::processControlMessage(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher)
		{
		  // we must read the dest from the message stamps
		  static StampListControl stamps;
		  std::string hostname;
		  msg.stamps.read(stamps.dest,hostname);

		  if (flowvr::daemon::verboseLevel>=1)
			std::cout << "NetTCP: control message dest="<<hostname<<std::endl;

		  sockaddr_in destip;

		  if ( !TcpTools::getNodeAddress(hostname, destip) )
			return;

		  if (flowvr::daemon::verboseLevel>=1)
			std::cout << "NetTCP: control message ip=" << TcpTools::getHostFromAddress(destip) << std::endl;

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
			  std::cerr << "NetTCP::processControlMessage: getSendActionHandler failed"<<std::endl;
			  return;
			}
			sender->enqueue(msg,true);
		  }
		}

		/// Find or Create an SendActionHandler given the dest ip
		NetTCP::SendActionHandler* NetTCP::getSendActionHandler(sockaddr_in &dest)
		{
			if ( TcpTools::isLocal(dest, localIp) )
			{
				std::cerr << "NetTCP: detected connection to myself."<<std::endl;
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
				int sock1, sock2;

				sock1 = TcpTools::connectTo( dest, true );
				sock2 = TcpTools::connectTo( dest, true );

				ipc::ScopedMTLock locker(lockSenders,"getSendActionHandler:2");
				SendActionHandler* sender = new SendActionHandler(this, dest, sock1, sock2);
				senders.push_back(sender);
				return sender;
			}
			catch( TcpTools::TcpException &e )
			{
				e.print();
			}
			return NULL;
		}

		bool NetTCP::remRevcThread( RecvThread *thr )
		{
			ipc::ScopedMTLock locker(lockReceivers,"remRecvThread");
			receivers.erase(std::remove( receivers.begin(), receivers.end(), thr ), receivers.end() );
			toClaim.push_back(thr); // can/should dispose memory later
			return true;
		}

		Result NetTCP::doAction(xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
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

		  bool NetTCP::toggleStats()
		  {
			  bTakeStats = !bTakeStats;
			  std::for_each( senders.begin(), senders.end(), _setStats<SendActionHandler>( bTakeStats ) );
			  std::for_each( receivers.begin(), receivers.end(), _setStats<RecvThread>(bTakeStats) );

			  return bTakeStats;
		  }

		  bool NetTCP::getTakeStats() const
		  {
			  return bTakeStats;
		  }

		  std::string NetTCP::createSendStatsString() const
		  {
			  _createStatsString<SendActionHandler> s;
			  s = std::for_each( senders.begin(), senders.end(), s );
			  return s.m_text;
		  }

		  std::string NetTCP::createRecvStatsString() const
		  {
			  _createStatsString<RecvThread> s;
			  s = std::for_each( receivers.begin(), receivers.end(), s );
			  return s.m_text;
		  }

		  std::string NetTCP::createConnectionsString() const
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

