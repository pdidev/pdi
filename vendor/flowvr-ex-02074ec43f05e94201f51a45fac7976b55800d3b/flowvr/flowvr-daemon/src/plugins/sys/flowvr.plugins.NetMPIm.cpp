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
* File: src/plugins/flowvr.plugins.NetMPIm.cpp                    *
*                                                                 *
* Contacts:                                                       *
*  03/07/2013 Jeremy Jaussaud <Jeremy.Jaussaud@inria.fr>          *
*                                                                 *
******************************************************************/
#include <flowvr/config.h>
#include <flowvr/daemondata.h>
#include <flowvr/daemon.h>
#include <flowvr/stamp.h>
#include <flowvr/message.h>
#include <flowvr/thread.h>
#include <flowvr/ipc/mtrwlock.h>
#include <flowvr/ipc/mtlock.h>
#include <flowvr/ipc/mtsignal.h>
#include <flowvr/plugd/class.h>
#include <flowvr/plugd/object.h>

#include <flowvr/plugd/genclass.h>
#include <flowvr/plugd/dispatcher.h>
#include <flowvr/plugd/actionhandler.h>

#include <flowvr/utils/multibuf.h>

#include <map>
#include <vector>
#include <sstream>

#include <cmath>

#include <mpi.h>

#include <arpa/inet.h>
#include <ifaddrs.h>
#include <netdb.h>

#define notNULL(A) ((A) == NULL ? std::string("") : std::string(A))

#define FLOWVR_MPI_CALL( call )                         \
do if ( int err = (call) ) {                            \
    char errStr[ MPI_MAX_ERROR_STRING + 1 ];            \
    int errLen;                                         \
    if ( ! MPI_Error_string( err, errStr, &errLen ) ) { \
        errStr[ errLen ] = '\0';                        \
        std::cerr << errStr << std::endl;               \
    }                                                   \
    throw std::bad_exception();                         \
} while(false)


namespace
{
/**
 * @brief getAddresses supply a vector of the IP of the machine.
 *
 *  Gather IP adresses of the machine in both IPv4 and IPv6 using \c getifaddrs
 * then serialize them using \c inet_ntop. The result is added to the back of
 * the \p vaddr parameter.
 *
 * @param vaddr a vector to which adresses are appended
 */
static void getAddresses( std::vector< std::string > & vaddr )
{
    struct ifaddrs *myaddrs;
    // gather ip adresses
    const bool error = getifaddrs( & myaddrs );
    // read them
    if ( ! error ) {
        // preallocate storage for 'inet_ntop' output
        vaddr.push_back( std::string() );
        vaddr.back().resize( INET6_ADDRSTRLEN );
        // for each returned adress
        for ( ifaddrs* ifa = myaddrs  ;  ifa != NULL  ;  ifa = ifa->ifa_next ) {
            void * p = NULL;
            // check validity of address
            if ( ifa->ifa_addr != NULL ) {
                switch( ifa->ifa_addr->sa_family ) {
                case AF_INET:
                    p = & ((sockaddr_in*)ifa->ifa_addr)->sin_addr;
                    break;
                case AF_INET6:
                    p = & ((sockaddr_in6*)ifa->ifa_addr)->sin6_addr;
                    break;
                }
            }
            // convert it to a string
            if ( p && inet_ntop(ifa->ifa_addr->sa_family, p, &vaddr.back()[0], INET6_ADDRSTRLEN)) {
                // do not resize. keep extra storage for MPI send operation
                // pre-allocate new storage
                vaddr.push_back( std::string() );
                vaddr.back().resize( INET6_ADDRSTRLEN );
            }
        }
        vaddr.pop_back(); // forget pre-allocated storage
        freeifaddrs(myaddrs);
    }
}

/**
 * @brief getAddresses supply a vector of IP of a machine.
 *
 *  Gather IP adresses of the host identified by \p name, in both IPv4 and IPv6
 * using \c getaddrinfo then serialize them using \c inet_ntop. The result is
 * added to the back of the \p vaddr parameter.
 *
 * @param name a string identifying a host
 * @param vaddr a vector to which adresses are appended
 */
static void getAddresses( const std::string & name, std::vector< std::string > & vaddr )
{
    // get ip adresses
    struct addrinfo *result;
    const bool error = getaddrinfo( name.c_str(), NULL, NULL, &result );
    // read them
    if ( ! error ) {
        // preallocate storage for 'inet_ntop' output
        vaddr.push_back( std::string() );
        vaddr.back().resize( INET6_ADDRSTRLEN );
        // for each returned adress
        for ( addrinfo* rp = result  ;  rp != NULL  ;  rp = rp->ai_next ) {
            void * p = NULL;
            // check address family
            switch( rp->ai_family ) {
            case AF_INET:
                p = & ((sockaddr_in*) rp->ai_addr)->sin_addr;
                break;
            case AF_INET6:
                p = & ((sockaddr_in6*)rp->ai_addr)->sin6_addr;
                break;
            }
            // convert it to a string
            if ( p && inet_ntop( rp->ai_family, p, &vaddr.back()[0], INET6_ADDRSTRLEN ) ) {
                // clamp unwritten characters
                vaddr.back() = vaddr.back().data();
                // pre-allocate new storage
                vaddr.push_back( std::string() );
                vaddr.back().resize( INET6_ADDRSTRLEN );
            }
        }
        vaddr.pop_back(); // forget pre-allocated storage
        freeaddrinfo( result );
    }
}
}

namespace flowvr
{
namespace plugins
{

using namespace flowvr::plugd;

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

/**
 * @brief the NetMPIm plugin to the daemon.
 *
 * \attention This plugin requires an MPI_THREAD_MULTIPLE capable
 *  implementation of MPI.
 * 
 * It encapsulates the send and receive protocol for messages sent from one
 * host to one another.
 * In case a message is detected to be for the host this NetMPIm object is
 * running on it will bypass sending and directly put it back to the daemon
 * for further dispatching.
 * 
 * It creates 3 threads:
 * - a dispatcher thread to which incoming messages are forwarded.
 * - a sending thread that actually send the messages through MPI after
 *   sending a header describing it (flags, stamp/data sizes, segments)
 * - a receiving thread that care care of receiving expected messages as
 *   well as waiting for incoming message headers.
 *
 * Wait type (active/passive) depends mostly on the MPI implementation.
 * \see sendThread, recvThread
 */
class NetMPIm
        : public flowvr::plugd::Object
{
public:
    
    NetMPIm( const std::string & objID );
    virtual ~NetMPIm();
    
    virtual Result init(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);
    
    virtual Class*         getClass() const;
    virtual ActionHandler* createAction(xml::DOMElement* xmlRoot);
    
    virtual Result doAction(xml::DOMElement* xmlRoot, Dispatcher* dispatcher);
    
    /// return the rank of the daemon
    int getRank() const         { return _rank; }
    int getWorldSize() const    { return _worldSize; }
    /// return the rank of the daemon whose host is named \p name
    int getRank( const std::string & name );
  
public:
  
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Helper classes, threads for sending and receiving
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    class Job;
    template< class JOB >
    class TodoListT;
    
    class SendThread;
    class RecvThread;
    class SendActionHandler;
    class ControlActionHandler;

private:

    Dispatcher *                      _dispThread;
    RecvThread *                      _recvThread;
    SendThread *                      _sendThread;
    ControlActionHandler *            _controlActionHandler;
    std::vector<SendActionHandler*>   _senders;
    
    void processControlMessage( const Message& msg, plugd::Dispatcher* dispatcher );
    
    ///////////////////////////////////
    // MPI related members
    
    typedef  std::map< std::string, int > rkMap_t;
    std::string   _rkstr;
    int           _rank;
    int           _worldSize;
    rkMap_t       _str2rank;
    ipc::MTrwLock _str2rank_lock;
    
    void init_MPI();
  
};

// ---------------------------------------------------------------------
// SendThread
// ---------------------------------------------------------------------
class NetMPIm::SendThread : public flowvr::Thread
{
public:
    SendThread( NetMPIm* parent );
    
    void enqueue( const flowvr::Message& msg, bool control, int rank );
    void close();
  
protected:
  
    virtual int run();
  
private:
    
    class Job;
    
    std::list<Job>  _queue;
    ipc::MTSignal   _signal;
    ipc::MTLock     _lock;
    NetMPIm *       _parent;
    bool            _stop;
};

// ---------------------------------------------------------------------
// RecvThread
// ---------------------------------------------------------------------
class NetMPIm::RecvThread : public flowvr::Thread
{
public:
    RecvThread( NetMPIm* parent, Dispatcher* dispatcher );
    
    void close();
    
protected:
    
    virtual int run();
    
private:
    
    class Job;
    
    NetMPIm *   _parent;
    Dispatcher* _dispatcher;
    bool        _stop;
};

// ---------------------------------------------------------------------
// SendActionHandler
// ---------------------------------------------------------------------
class NetMPIm::SendActionHandler : public ActionHandler
{
public:
    SendActionHandler( NetMPIm::SendThread * sendThread, int rank );
    
    /// Create an ActionHandler for batch mode action execution.
    virtual ActionHandler* createAction(xml::DOMElement* xmlRoot);
    virtual void enqueue(const flowvr::Message& msg, bool control);
    virtual void doIt(const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher);
    virtual void remove();
    void close();

private:
    NetMPIm::SendThread *   _queue;
    ipc::MTAtomicInt        _nbref;
    int                     _rank;

};

// ---------------------------------------------------------------------
// ControlActionHandler
// ---------------------------------------------------------------------
class NetMPIm::ControlActionHandler : public ActionHandler
{
public:
    ControlActionHandler( NetMPIm* parent );
    
    virtual ActionHandler* createAction( xml::DOMElement* xmlRoot );
    virtual void doIt( const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher );
    virtual void remove();
  
private:
    ipc::MTLock      _lock;
    ipc::MTAtomicInt _nbref;
    NetMPIm *        _parent;
};

// ##########################################################################
// JOBS
// ##########################################################################

struct NetMPIm::Job
{
    enum {
        control_flag        = 1 << 0,
        keepsegments_flag   = 1 << 1
    };

    typedef long long unsigned int size_type;
    static inline MPI_Datatype size_enum() { return MPI_UNSIGNED_LONG_LONG; }
    
    enum { HEADER = 1337, DATA = 1134 }; // random tags
    enum { MAX_SEGMENTS = 4096/sizeof(size_type) - 4 };
};

/**
 * @brief The mpiThread::Job class represent a data send/recv to
 *  perform usign MPI.
 *
 * Instances are created using the \c newSend and \c newRecv static
 * functions before being inserted in a \c TodoListT. You don't have to 
 */
class NetMPIm::SendThread::Job : public NetMPIm::Job
{
    friend class TodoListT< Job >;
public:
    
    ///////////////////////////////////
    //  Public methods
    
    Job( const flowvr::Message & m, bool bControl, int rank )
        : _msg( m )
        , _rank( rank )
        , _nLeft( 1 /*header*/ + 1 /*stamps*/ + m.data.getNumberOfSegments() )
    {
        // extract system flags
        StampList slist;
        int flags = 0;
        m.stamps.read( slist.sysflags, flags );
        bool keepsegments = flags bitand StampList::SYS_FLG_KEEPSEGMENTS;
        // create header announcing data sending
        _header.resize( 4 + m.data.getNumberOfSegments() );
        _header[0] = bControl * control_flag | keepsegments * keepsegments_flag ;
        _header[1] = m.stamps.getSize();
        _header[2] = m.data.getSize( Buffer::ALLSEGMENTS ) ;
        _header[3] = m.data.getNumberOfSegments();
        for ( int i = 0  ;  i < m.data.getNumberOfSegments()  ;  i++ ) {
            _header[ i+4 ] = m.data.getSegmentSize( i );
        }
    }
    
    /// return \c true if the Job is completed
    bool isDone() const { return 0 == _nLeft; }
    
protected:
    
    ///////////////////////////////////
    //  Protected methods
    
    /// begin the Job, creating the MPI requests
    inline void beginIt( const std::vector<MPI_Request*> & JobR )
    {
        // send header
        FLOWVR_MPI_CALL( MPI_Isend(
                    &_header[0], _header.size(), size_enum(),
                    _rank, HEADER, MPI_COMM_WORLD, JobR[0] ) );
        // send stamps
        FLOWVR_MPI_CALL( MPI_Isend(
                    const_cast< ubyte* >( _msg.stamps.readAccess() ),
                    _msg.stamps.getSize(), MPI_UNSIGNED_CHAR,
                    _rank, DATA, MPI_COMM_WORLD, JobR[1] ) );
        // send data itself
        for ( int i = 0  ;  i < _nLeft-2  ;  ++i ) {
            FLOWVR_MPI_CALL( MPI_Isend(
                        const_cast< ubyte* >( _msg.data.readAccess( i ) ),
                        _msg.data.getSize( i ), MPI_UNSIGNED_CHAR,
                        _rank, DATA, MPI_COMM_WORLD, JobR[i+2] ) );
        }
    }	
    /// end the Job after MPI request are completed, doing any required 
    /// post-process (giving a received message to the dispatcher)
    inline void endIt() { /* no post-process */ }
    
    /// update the Job after \p nDone MPI request have been completed
    /// \attention no range-check is performed  
    void update( int nDone ) { _nLeft -= nDone; }
    
private:
    
    ///////////////////////////////////
    //  Private members
    
    std::vector< size_type > _header;   ///< header of the message
    Message _msg;   ///< copy of the msg (ref counting)
    int     _rank;  ///< MPI rank of the destination
    int     _nLeft; ///< number of requests you still must complete
};
    
/**
 * @brief The mpiThread::Job class represent a data send/recv to
 *  perform usign MPI.
 *
 *  represent both msg-header waiting and data receiving
 *
 * Instances are created using the \c newSend and \c newRecv static
 * functions before being inserted in a \c TodoListT. You don't have to 
 */
class NetMPIm::RecvThread::Job : public NetMPIm::Job
{
    friend class TodoListT< Job >;
public:
    
    ///////////////////////////////////
    //  Public methods
    
    /// header receiving job
    Job( int rank, Dispatcher * dispatcher, std::list< Job > & newJobs )
        : _msg( /* not used here */ )
        , _dispatcher( dispatcher )
        , _newJobs( newJobs )
        , _header()
        , _bufs( /* not used here */ )
        , _msgflags(  /* not used here */ )
        , _rank( rank )
        , _nLeft( 1 )
        , _step( HEADER )
    {}
    
    /// data receiving job
    Job( const std::vector< size_type > & header,
         int rank, Dispatcher * dispatcher, std::list< Job > & newJobs )
        : _msg()
        , _dispatcher( dispatcher )
        , _newJobs( newJobs )
        , _header( /* not used here */ )
        , _bufs()
        , _msgflags( header[0] )
        , _rank( rank )
        , _nLeft()
        , _step( DATA )
    {
        const bool keepsegments   = header[0] & keepsegments_flag;
        const size_t stamps_size  = header[1];
        const size_t data_size    = header[2];
        const size_t num_segments = header[3];

        // stamps
        // allocate
        _msg.stamps = Allocator::getAllocator()->alloc( stamps_size );
        if( ! _msg.stamps.valid() )
            throw std::bad_alloc();
        // remember
        multibuf buf = { _msg.stamps.writeAccess(), _msg.stamps.getSize() };
        _bufs.push_back( buf );

        // data : process the below only for FULL / valid() messages
        if( 0 != num_segments )
        {
            // allocate memory
            _msg.data = Allocator::getAllocator()->alloc( data_size );
            if ( ! _msg.data.valid() )
                throw std::bad_alloc();
            _bufs.resize( 1 + num_segments );
            // prepare _bufs (to create MPI request when calling beginit)
            // we create a conditional return value here (in case we keep segments)
            BufferWrite s;
            size_t offset=0;
            for( size_t n = 0; n < num_segments; ++n )
            {
                size_t segment_size = header[ 4+n ];
                // the segment itself will be received with MPI
                _bufs[1+n].iov_base = _msg.data.getWrite<void>( offset );
                _bufs[1+n].iov_len = segment_size;
                // preserve the segmentation if asked
                if( keepsegments ) {
                    s += BufferWrite( _msg.data, offset, segment_size );
                }
                // advance the offset in the storage array
                offset += segment_size;
            }
            // preserve the segmentation if asked
            if ( keepsegments ) {
                _msg.data = s;
            }
        }
        // one MPI request per contiguous segment
        _nLeft = _bufs.size();
    }
    
    /// return \c true if the Job is completed
    bool isDone() const { return 0 == _nLeft; }
    
protected:
    
    ///////////////////////////////////
    //  Protected methods
    
    /// begin the Job, creating the MPI requests
    inline void beginIt( const std::vector<MPI_Request*> & JobR )
    {
        if ( _step == HEADER ) {
            header_beginIt( JobR );
        } else {
            data_beginIt( JobR );
        }
            
    }
    
    /// end the Job after MPI request are completed, doing any required 
    /// post-process
    inline void endIt()
    {
        if ( _step == HEADER ) {
            header_endIt();
        } else {
            data_endIt();
        }
    }
    
    /// begin a header receiving job
    inline void header_beginIt( const std::vector<MPI_Request*> & JobR )
    {
        _header.resize( 4 + MAX_SEGMENTS );
        FLOWVR_MPI_CALL( MPI_Irecv(
                    &_header[0], _header.size(), size_enum(),
                    _rank, MPI_ANY_TAG, MPI_COMM_WORLD, JobR[0] ) );
    }
    
    /// end a header receiving job
    inline void header_endIt()
    {
        // receive message of which you just got the header
        _newJobs.push_back( Job( _header, _rank, _dispatcher, _newJobs ) );
        // expect another header after that
        _newJobs.push_back( Job( _rank, _dispatcher, _newJobs ) );
    }

    /// begin a stamp+data receiving job
    inline void data_beginIt( const std::vector<MPI_Request*> & JobR )
    {
        // receive stamp+data
        for ( int i = 0  ;  i < _nLeft  ;  ++i  ) {
            FLOWVR_MPI_CALL( MPI_Irecv(
                        _bufs[i].iov_base, _bufs[i].iov_len, MPI_UNSIGNED_CHAR,
                        _rank, MPI_ANY_TAG, MPI_COMM_WORLD, JobR[i] ) );
        }
    }
    
    /// end a stamp+data receiving job
    inline void data_endIt()
    {
//        // sending buffer was not asked to merge the segments
//        // could be that this is just a 'normal' / unsegmented buffer, or
//        // a segmented buffer that was merged.
//        // reald the 'old' flags
//        if( keepsegments == false )
//        {
//            StampList slist;
//            int flags;
//            _msg.stamps.read( slist.sysflags, flags );
//            if( flags bitand StampList::SYS_FLG_KEEPSEGMENTS )
//            {
//                // we have lost the segments, so re-set the flag on the buffer.
//                StampsWrite w( _msg.stamps );
//                w.write( slist.sysflags, flags and ~StampList::SYS_FLG_KEEPSEGMENTS );
//            }
//        }
        // forward the received message to the dispatcher
        if ( _msgflags & control_flag ) {
            _dispatcher->processRemoteControlMessage( _msg );
        } else {
            _dispatcher->process( _msg );
        }
    }
    
    /// update the Job after \p nDone MPI request have been completed
    /// \attention no range-check is performed  
    void update( int nDone ) { _nLeft -= nDone; }
    
private:
    
    ///////////////////////////////////
    //  Private members
    
    flowvr::MessageWrite        _msg; ///< copy of the msgs (ref counting)
    Dispatcher *                _dispatcher;
    std::list< Job > &          _newJobs;
    std::vector< size_type >    _header;
    std::vector<multibuf>       _bufs;
    
    size_type _msgflags;///< flags sent with the message (eg, control_flag)
    int       _rank;    ///< rank of the daemon you receive messages from
    int       _nLeft;   ///< number of requests you still must complete
    int       _step;
};

// ##########################################################################
// TodoList
// ##########################################################################

/**
 * @brief The mpiThread::TodoListT class handles every MPI calls
 *  after initialization.
 *
 * The public interface is quite simple with only two methods.
 */
template< class JOB >
class NetMPIm::TodoListT
{
public:
    
    typedef JOB Job;
    
    TodoListT()
        : _Jobs()
        , _newJob( _Jobs.end() )
        , _reqs()
        , _reqJobs()
        , _freeIndexes()
//			, _nSend( 0 )
//            , _nRecv( 0 )
    {}
    
    ~TodoListT()
    {
        freeAll();
    }
    
//		int nSend() const { return _nSend; }
//        int nRecv() const { return _nRecv; }

    size_t size() const { return _Jobs.size(); }
    size_t empty() const { return _Jobs.empty(); }
    
    /**
     * @brief splice new Jobs to perform from a given list.
     *
     * This method keeps an iterator to the first newly-inserted Job,
     * updating the \c _newJob iterator if necessary.
     *
     * \attention It should be possible to splice Jobs in several calls but
     * this has neither been found necessary nor tested.
     *
     * @param Jobs list to splice to the end of the TodoListT.
     */
    void splice( std::list< Job > & Jobs )
    {
        if ( ! Jobs.empty() ) {
            // no previous new Jobs
            if ( _newJob == _Jobs.end() ) {
                _newJob = Jobs.begin();// splice doesn't invalidate any iterator
            }
            _Jobs.splice( _Jobs.end(), Jobs );
        }
    }
    
    /**
     * @brief doIt try to complete the stored Jobs.
     *
     *  It first begin the newly inserted jobs (creating new MPI requests)
     * then wait for some of those request to complete using \c waitSome
     */
    void doIt()
    {
        beginNewJobs();
        // Using MVAPICH2, it seems an immediate receive takes time to
        // complete if the corresponding send _request_ is still pending.
        // Thus, wait is only performed using MPI_Waitsome.
        // \see \c mpiThread::run
        if ( true )//|| 0 != nRecv() )
            waitSome();
        else
            testSome();
    }
    
protected:
    
    /**
     * @brief freeAll cancels and frees ressources of the pending requests
     */
    void freeAll()
    {
        for ( int i = 0  ;  i < _reqs.size()  ;  i++ ) {
            if ( _reqJobs[i] == _Jobs.end() )
                continue;
            // Cancel operation
            FLOWVR_MPI_CALL( MPI_Cancel( &_reqs[i] ) );
            FLOWVR_MPI_CALL( MPI_Request_free( &_reqs[i] ) );
        }
    }
    
    /**
     * @brief try to complete some of the stored Jobs
     */
    void testSome()
    {
        if ( _reqs.empty() )
            return;
        
        int count;
        std::vector<int> indexes( _reqs.size() );
        FLOWVR_MPI_CALL( MPI_Testsome( _reqs.size(), &_reqs[0], &count,
                               &indexes[0], MPI_STATUSES_IGNORE ) );
        
        if ( count != MPI_UNDEFINED )
        for ( int i = 0  ;  i < count  ;  i++ ) {
            if ( _Jobs.end() != _reqJobs[ indexes[i] ] ) // finally completed
                postProcessRequest(  indexes[i]  );
        }
//			// resize request vectors
//			_reqs.resize( end ); // you MUST update _freeIndexes too (costly?)
//			_reqJobs.resize( end );
    }
    
    /**
     * @brief wait for some of the stored Jobs to be completed
     */
    void waitSome()
    {
        int count;
        std::vector<int> indexes( _reqs.size() );
        FLOWVR_MPI_CALL( MPI_Waitsome( _reqs.size(), &_reqs[0], &count,
                                &indexes[0], MPI_STATUSES_IGNORE ) );
        
        if ( count != MPI_UNDEFINED ) // this test shouldn't be necessary
        for ( int i = 0  ;  i < count  ;  i++ ) {
            if ( _Jobs.end() != _reqJobs[ indexes[i] ] ) // finally completed
                postProcessRequest(  indexes[i]  );
        }
    }

    /**
     * @brief beginNewJobs start newly inserted Jobs
     *
     * This methods reserve elements in the \c _req and \c _reqJobs vectors
     * before effectively initiating the MPI requests.
     */
    void beginNewJobs()
    {
        // find free space in the '_reqs'/'_reqJobs' vectors
        size_t queRSize = _reqs.size(); // queue: size of requests vector
        size_t i; // index inside the Job's requests-vector
        size_t nFree = _freeIndexes.size(); 
        
        // Resize vectors right now to ensure to use valid pointers when
        // begining the Jobs
        size_t neededSize = 0;
        size_t JobRMax = 0;
        for ( typename std::list< Job >::const_iterator it = _newJob
              ; it != _Jobs.end() ; ++it )
        {
            neededSize += it->_nLeft;
            if ( JobRMax < it->_nLeft )
                JobRMax = it->_nLeft;
        }
        if ( neededSize > _freeIndexes.size() ) {
            // the new Jobs need more requests than currently free
            size_t newSize = queRSize + neededSize - _freeIndexes.size() ;
            _reqs.resize( newSize );
            _reqJobs.resize( newSize );
        }
        // create a temporary vector storing pointers to the requests
        std::vector<MPI_Request*> JobR; // used for JOB::beginIt() calls
        JobR.reserve( JobRMax );
        
        // fit entire Jobs while enough free indexes
        for ( /* it == #1 new Job */ ; _newJob != _Jobs.end() ; ++_newJob )
        {
            size_t JobRSize = _newJob->_nLeft; // Job: n of requests
            JobR.resize( JobRSize );
            if ( JobRSize > nFree ) // this Job won't fit
                break;				// break to the next for loop
            
            // we need to store each request separatly
            for ( i = 0  ;  i < JobRSize  ;  i++ )
            {
                // get the oldest free index
                size_t index = _freeIndexes.front();
                _freeIndexes.pop_front();
                // keep a pointer to the request for an MPI call
                JobR[i] = &_reqs[index];
                // TodoListT stores the Job iterator
                _reqJobs[index] = _newJob;
            }
            nFree -= JobRSize;
            
//                // begin the Job (MPI_Isend, MPI_Irecv)
//                if ( _newJob->isRecv() )
//                    _nRecv++;
            _newJob->beginIt( JobR );
        }
        
        // fit what you can from this Job (known not to fit entirely)
        if ( _newJob != _Jobs.end() )
        {
            // we need to store each request separatly
            for ( i = 0  ;  nFree /* (&& i < JobRSize) */ ;  i++, nFree-- )
            {
                size_t index = _freeIndexes[i];
                // keep a pointer to the request for an MPI call
                JobR[i] = &_reqs[index];
                // TodoListT stores requests and the Job iterator
                _reqJobs[index] = _newJob;
            }
            // we reached the end of our (the TodoListT) requests vector
            _freeIndexes.clear();
        }
        
        // fit what's left in the back of the previously resized vectors
        for ( size_t index = queRSize /* it == first new Job */
              ;  _newJob != _Jobs.end() ;  ++_newJob  )
        {
            // we need to store each request separatly
            size_t JobRSize = _newJob->_nLeft;
            JobR.resize( JobRSize );
            for ( /* i == first request */ ; i < JobRSize ; i++, index++ )
            {
                // keep a pointer to the request for an MPI call
                JobR[i] = &_reqs[index];
                // TodoListT stores requests and the Job iterator
                _reqJobs[index] = _newJob;
            }
            
//                // begin the Job (MPI_Isend, MPI_Irecv)
//                if ( _newJob->isRecv() )
//                    _nRecv++;
            _newJob->beginIt( JobR );
            i = 0; // re-init i for next loop
        }
    }
    
    /// \brief postProcessRequest updates the Job associed with the (newly
    ///	completed) request stored at index \p i
    void postProcessRequest( int i )
    {
        // keep the Job aware that one of its requests is done
        _reqJobs[i]->update( 1 );
        if ( _reqJobs[i]->isDone() )
        {
            // the Job has now all its requests completed, end it
            _reqJobs[i]->endIt();
//                // forget it
//                if ( _reqJobs[i]->isRecv() )
//                    _nRecv--;
            _Jobs.erase( _reqJobs[i] );
        }
        _reqJobs[i] = _Jobs.end();	// mark the request as done
        _reqs[i] = MPI_REQUEST_NULL;// necessary?
        _freeIndexes.push_back( i );// keep trac of the newly free index
    }
    
private:

    /// a list of pending Jobs
    std::list< Job >                                    _Jobs;
    /// begining of the newly inserted Jobs
    typename std::list< Job >::iterator                 _newJob;
    
    /// a contiguous (23.2.4.1) array of MPI requests
    std::vector< MPI_Request >                          _reqs;
    /// the associed Jobs
    std::vector< typename std::list< Job >::iterator >  _reqJobs;
    
    /** \brief indexes free inside the \c _reqs and \c _reqJobs vectors
     *  \note we use a \c deque to ensure each free index will be used again
     *    later, so the vectors can be resized down.
     */
    std::deque< unsigned >                              _freeIndexes;
    
//        int _nRecv;		///< number of Receive requests in the TodoListT
};

// ############################################################################
// SendThread
// ############################################################################

NetMPIm::SendThread::SendThread( NetMPIm* parent )
    : flowvr::Thread()
    , _signal( "NetMPIm::SendThread" )
    , _lock( "NetMPIm::SendThread" )
    , _parent( parent )
    , _stop( false )
{
    start();
}

void NetMPIm::SendThread::close()
{
    _stop = true;
    _signal.notify();
    wait();
}

/// Main thread function.
int NetMPIm::SendThread::run()
{
    NetMPIm::TodoListT< Job > todoList;
    while ( true )
    {
        {
            // claim lock
            ipc::ScopedMTLock locker( _lock, "run" );
            while ( _queue.empty() && !_stop )
                _signal.wait( _lock );
            // we own lockThread now

            if (_stop) // user gave stop signal
                break;
            // add new jobs to the todoList
            todoList.splice( _queue );
        }
        // lock is released: now get to work!            
        todoList.doIt();
    }
    return 0;
}

void NetMPIm::SendThread::enqueue(const flowvr::Message& msg, bool control, int rank )
{
    ipc::ScopedMTLock locker( _lock, "enqueue" );
    _queue.push_back( Job( msg, control, rank ) );
    _signal.notify();
}

// ##########################################################################
// RecvThread
// ##########################################################################

NetMPIm::RecvThread::RecvThread( NetMPIm* parent, Dispatcher* dispatcher )
    : flowvr::Thread()
    , _parent( parent )
    , _dispatcher( dispatcher )
    , _stop( false )
{
    start();
}

void NetMPIm::RecvThread::close()
{
    _stop = true;
    terminate(); //\todo
    wait();
}

int NetMPIm::RecvThread::run()
{
    NetMPIm::TodoListT< Job > todoList;
    std::list< Job > newJobs;
    
    // add job to receive the first incoming headers
    for ( int i = 0  ; i < _parent->getWorldSize()  ;  i++ ) {
        if ( i != _parent->getRank() ) {
            newJobs.push_back( Job( i, _dispatcher, newJobs ) );
        }
    }
    
    try
    {
        while ( ! _stop )
        {
            todoList.splice( newJobs );
            todoList.doIt();
        }
    } // try
    catch( std::exception &x )
    {
        std::cerr << "Caught std::exception() -- " << x.what() << std::endl;
    }
//\todo		m_parent->remRevcThread(this);
    return 0;
}

// ##########################################################################
// SendActionHandler
// ##########################################################################

NetMPIm::SendActionHandler::SendActionHandler( NetMPIm::SendThread * sendThread, int rank )
    : ActionHandler()
    , _queue( sendThread )
    , _nbref( 1 )
    , _rank( rank )
{}

void NetMPIm::SendActionHandler::close()
{
//\todo        queue.close();
}

void NetMPIm::SendActionHandler::doIt( const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher )
{
    enqueue( msg, false );
}

void NetMPIm::SendActionHandler::remove()
{
    if ( _nbref <= 0 )
        return;
    if ( _nbref.dec_and_test_null() )
        delete this;
}

void NetMPIm::SendActionHandler::enqueue( const flowvr::Message& msg, bool control )
{
    _queue->enqueue( msg, control, _rank );
}

/// Create an ActionHandler for batch mode action execution.
ActionHandler* NetMPIm::SendActionHandler::createAction( xml::DOMElement* /*xmlRoot*/ )
{
    if (_nbref==0)
        return NULL;
    ++_nbref;
    return this;
}

// ##########################################################################
// ControlActionHandler
// ##########################################################################

NetMPIm::ControlActionHandler::ControlActionHandler( NetMPIm* parent )
    : ActionHandler()
    , _lock( "NetMPIm::ControlActionHandler" )
    , _nbref( 1 )
    , _parent( parent )
{}

ActionHandler* NetMPIm::ControlActionHandler::createAction( xml::DOMElement* xmlRoot )
{
    if ( _nbref == 0 )
        return NULL;
    ++_nbref;
    return this;
}

void NetMPIm::ControlActionHandler::doIt( const flowvr::Message& msg, flowvr::plugd::Dispatcher* dispatcher )
{
    ipc::ScopedMTLock locker( _lock, "doIt" );
    if ( _parent != NULL )
        _parent->processControlMessage( msg, dispatcher );
}

void NetMPIm::ControlActionHandler::remove()
{
    if ( _nbref <= 0 )
        return;
    if ( _nbref.dec_and_test_null() )
        delete this;
}

// ##########################################################################
// NetMPIm
// ##########################################################################

GenClass<NetMPIm> NetMPImClass( "flowvr.plugins.NetMPIm", "" );

/// Constructor.
NetMPIm::NetMPIm( const std::string & objID )
    : Object(objID)
    , _dispThread(NULL)
    , _recvThread( NULL )
    , _sendThread( NULL )
    , _controlActionHandler( NULL )
    , _rank( -1 )
    , _worldSize( -1 )
    , _str2rank()
    , _str2rank_lock( "NetMPIm::_str2rank" )
{}


NetMPIm::~NetMPIm()
{
    if ( -1 != _rank )
        MPI_Finalize();
    for ( int i = 0  ;  i < _senders.size()  ;  i++ ) {
        _senders[i]->remove();
    }
}

Class* NetMPIm::getClass() const
{
    return &NetMPImClass;
}

Result NetMPIm::init( xml::DOMElement* xmlRoot, Dispatcher* dispatcher )
{
    try
    {
        Result res = Object::init(xmlRoot, dispatcher);
        if ( ! res.error() ) {
            if ( -1 == _rank )
                init_MPI();
#if 0
            std::string name = _rkstr.c_str() + 1;  // remove first '['
            name.resize( name.size() - 2 );         // remove "] "
            name += "MpiDaemon";
            DaemonInterface::setHostName( name, _rank );
#else
            char hostname[ FLOWVR_OS_HOST_NAME_MAX ];
            hostname[ sizeof(hostname)-1 ] = 0;
            if ( gethostname( hostname, sizeof(hostname)-1 ) )
                hostname[0] = '\0';
            DaemonInterface::setHostName( hostname, _rank );
#endif
            // create threads
            _dispThread = dispatcher->threadCopy();
            _recvThread = new NetMPIm::RecvThread( this, _dispThread );
            _sendThread = new NetMPIm::SendThread( this );
            // create action handlers
            _controlActionHandler = new ControlActionHandler( this );
            for ( int i = 0  ;  i < getWorldSize()  ;  i++ ) {
                _senders.push_back( new SendActionHandler( _sendThread, i ) );
            }
        }
        return res;
    }
    catch( std::exception e )
    {
        std::cerr << e.what();
    }

    return Result::ERROR;
}

/// Create an ActionHandler for batch mode action execution.
ActionHandler* NetMPIm::createAction( xml::DOMElement* xmlRoot )
{
    xml::DOMElement* child = xmlRoot->FirstChildElement();
    if (child == NULL)
        return NULL;

    if ( ! strcmp(child->getNodeName(), "dest") )
    {
        // standard message send operation
        std::string hostname = child->getTextContent();
        const int rank = getRank( hostname );
        if ( rank == -1 || rank >= getWorldSize() )
        {
            // invalid hostname
            std::ostringstream os;
            os << _rkstr << "NetMPIm::createAction: getRank returned invalid rank [" << rank
               << "] for hostname [" << hostname << "]\n";
            std::cerr << os.str();
            return NULL;
        }
        else if ( rank == getRank() )
        {
            // self-connexion
            if (flowvr::daemon::verboseLevel>=1)
                std::cout << _rkstr << "NetMPIm: detected connection to myself." << std::endl;
            return ActionHandler::LoopBackActionHandler;
        }
        else
        {
            // connexion to a remote daemon
            return _senders[rank]->createAction( xmlRoot );
        }

    }
    else if ( ! strcmp(child->getNodeName(), "control") )
    {
        // control message send operation
        return _controlActionHandler->createAction( xmlRoot );
    }
    else
    {
        std::ostringstream os;
        os << _rkstr << "NetMPIm::createAction: Unknown action" << std::endl;
        std::cerr << os.str();
        return NULL;
    }
}

void NetMPIm::processControlMessage(const Message& msg, plugd::Dispatcher* dispatcher)
{
  // we must read the dest from the message stamps
  static StampListControl stamps;
  std::string hostname;
  msg.stamps.read( stamps.dest, hostname );

  if (flowvr::daemon::verboseLevel>=1)
    std::cout<<_rkstr<<"NetMPIm: control message dest="<<hostname<<std::endl;

  const int rank = getRank( hostname );
  if ( rank == -1 || rank >= getWorldSize() )
  {
      // invalid hostname
      std::ostringstream os;
      os << _rkstr << "NetMPIm::processControlMessage: getRank returned invalid rank" << rank
         << "for hostname [" << hostname << "]\n";
      std::cerr << os.str();
  }
  else if ( rank == getRank() )
  {
      // for the local daemon. Redispatch it as if it was remotely received.
      dispatcher->processRemoteControlMessage( msg );
  }
  else
  {
      // connexion to a remote daemon
      _senders[rank]->enqueue( msg, true );
  }
}

Result NetMPIm::doAction( xml::DOMElement* xmlRoot, Dispatcher* dispatcher )
{
    // get command
    xml::DOMElement *cmd = xmlRoot->FirstChildElement("cmd");
    if( cmd )
    {
        std::string id = notNULL(cmd->Attribute("id"));
        if( id.empty() )
            return Result::ERROR;
//            if( id == "togglestats")
//            {
//                toggleStats();
//                return Result( Result::OK, getTakeStats() ? "Statistics enabled" : "Statistics disabled" );
//            }
    }
    return Result::OK;
}

  int NetMPIm::getRank( const std::string & name )
  {
      {
          // look in the table created at initialization
          ipc::MTrwLock::ScoppedRead locker( _str2rank_lock );
          const rkMap_t::const_iterator it = _str2rank.find( name );
          if ( _str2rank.end() != it )
              return it->second;
      }
      // name is probably a hostname that we didn't see yet
      std::vector< std::string > vaddr( 1, name ); // for double-check
      getAddresses( name, vaddr );
      if ( flowvr::daemon::verboseLevel > 1 ) {
          // debug output
          std::ostringstream os;
          os << _rkstr <<  "getAddresses of '" << name << "' returned " << vaddr.size() << "\n";
          for ( int i = 0  ;  i < vaddr.size()  ;  i++ ) {
              os << _rkstr << " " << vaddr[i] << "\n";
          }
          std::cout << os.str();
      }
      {
          ipc::MTrwLock::ScoppedWrite locker( _str2rank_lock );
          // You can't find name ? Search the corresponding addresses.
          for ( int i = 0  ;  i < vaddr.size()  ;  i++ ) {
              const rkMap_t::const_iterator it = _str2rank.find( vaddr[i] );
              if ( it != _str2rank.end() ) {
                  const int rank = it->second;
                  if ( i > 0 ) { // lock related double-check 
                      // remember
                      _str2rank[ name ] = rank;
                      // print some output
                      std::ostringstream os;
                      os << _rkstr << "getRank identified hostname [" << name
                         << "] with address [" << vaddr[i]
                         << "] already associed to MPI rank [" << rank << "]\n";
                      std::cout << os.str();
                  }
                  return rank;
              }
          }
      }
      const bool known_host = vaddr.size() > 1;
      std::ostringstream os;
      os << _rkstr << "getRank failed to find the rank of the "
         << (known_host?"known":"unknown") << " host: [" << name << "]\n";
      std::cout << os.str();
      return -1;
  }
  
  void NetMPIm::init_MPI()
  {
      try
      {
          /////////////////////////////////////////////////
          //	Init MPI
          
          std::cout << " MPI_Init_thread" << std::endl;
          int provided;
          int err =
          MPI_Init_thread( DaemonInterface::getArgc(),
                           DaemonInterface::getArgv(),
                           MPI_THREAD_MULTIPLE, &provided
                           );
          if ( err ) throw err;
          if ( provided != MPI_THREAD_MULTIPLE ) // we need more
          {
              std::cerr << "ERROR: MPI_THREAD_MULTIPLE unsupported!\n";
              throw 0;
          }
          
          err = MPI_Comm_rank( MPI_COMM_WORLD, &_rank );
          if ( err ) throw err;
          
          err = MPI_Comm_size( MPI_COMM_WORLD, &_worldSize );
          if ( err ) throw err;
          
          {   // create the _rkstr string used as a prefix for std outputs.
              const int iworld = _worldSize-1 ? std::log10(_worldSize-1) : 0;
              const int irank = _rank ? std::log10(_rank) : 0;
              std::ostringstream os;
              for ( int i = 0  ;  i < iworld-irank  ;  ++i ) {
                  os << " ";
              }
              os << _rank;
              _rkstr = "\[rk" + os.str() + "] ";
          }
          
          /////////////////////////////////////////////////
          //	prepare the mapping (ip) --> (rank)
          
          std::cout << _rkstr << "Get adresses" << std::endl;
          std::vector< std::string > vaddr;
          getAddresses( vaddr );
          
          // get the number of interfaces
          int nlocal = vaddr.size();
          std::cout << _rkstr << "nAdresses == " << nlocal << std::endl;
          std::vector< int > nInterfaces( _worldSize );
          std::cout << _rkstr << "MPI_Allgather" << std::endl;
          err = MPI_Allgather( &nlocal,		  1, MPI_INT,
                               &nInterfaces[0], 1, MPI_INT, MPI_COMM_WORLD );
          if ( err ) throw err;
          
          // get our IPs, and store them as strings
          std::vector< char > myIPs( nInterfaces[_rank] * INET6_ADDRSTRLEN );
          for ( int i = 0  ;  i < nInterfaces[_rank]  ;  i++  )
          {
              strcpy( &myIPs[ i * INET6_ADDRSTRLEN ], vaddr[i].data() );
          }
          
          // compute sizes/offsets within 'allIPs' in bytes
          std::vector< int > interfacesOffset( _worldSize );
          std::vector< int > interfacesSize( _worldSize );
          int bufferSize = 0;
          for ( int i = 0, offset = 0  ;  i < _worldSize  ;  i++ )
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
          
          std::cout << _rkstr << "MPI_Allgatherv" << std::endl;
          // get all the involved IPs
          std::vector< char > allIPs( bufferSize );
          err = MPI_Allgatherv( &myIPs[0],
                                nInterfaces[_rank] * INET6_ADDRSTRLEN, MPI_CHAR,
                                &allIPs[0], &interfacesSize[0], &interfacesOffset[0],
                                MPI_CHAR, MPI_COMM_WORLD );
          if ( err ) throw err;
          
          // create the map itself
          for ( int rank = 0, i = 0  ;  rank < _worldSize  ;  rank++ ) {
              for ( int j = 0  ;  j < nInterfaces[rank]  ;  j++ , i++ ) {
                  std::string ip( &allIPs[ i * INET6_ADDRSTRLEN ] );
                  _str2rank[ ip ] = rank;
              }
          }
          for ( int i = 0  ;  i < vaddr.size()  ;  i++ ) {
              _str2rank[ vaddr[i].data() ] = _rank;
          }
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
          throw std::bad_exception();
      }
      
      /////////////////////////////////////////////////
      //	info output
      std::ostringstream os;
      os << _rkstr << "'Address -> rank' map created\n";
      for ( rkMap_t::const_iterator it = _str2rank.begin()
            ;	 it != _str2rank.end()  ;  ++it ) {
          os << _rkstr << ' ' << it->first << " --> " << it->second << "\n";
      }
      std::cout << os.str();
  }

} // namespace plugins

} // namespace flowvr
