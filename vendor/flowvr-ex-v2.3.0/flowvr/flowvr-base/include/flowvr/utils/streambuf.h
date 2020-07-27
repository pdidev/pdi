/******* COPYRIGHT ************************************************
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
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremy Jaussaud.                                             *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/utils/streambuf.h                                 *
*                                                                 *
* Contacts: 01/02/2013 Jeremy Jaussaud <jeremy.jaussaud@free.fr>  *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_STREAMBUF_H
#define FLOWVR_UTILS_STREAMBUF_H

#include <flowvr/buffer.h>

//#include <locale.h>
#include <iostream>

namespace flowvr
{

namespace utils
{

/**
 * @brief implements the std::streambuf abstract class to handle a FlowVR buffer
 *  using an iostream.
 *
 * Known limitations:
 *  * the size of the buffer must be me multiple of sizeof(char_type)
 */
template< typename charT, typename traits = std::char_traits< charT > >
class basic_streambuf
    : public std::basic_streambuf< charT >
{
    typedef std::basic_streambuf< charT > parent_type;
    
public:
    
    //////////////////////////////////////////
    // Public types
    
    typedef charT char_type;
    typedef traits traits_type;
    
    typedef std::locale locale;
    typedef std::streamsize streamsize;
    typedef std::ios_base ios_base;
    
    typedef typename traits_type::int_type int_type;
    typedef typename traits_type::pos_type pos_type;
    typedef typename traits_type::off_type off_type;
    
    //////////////////////////////////////////
    // Public construction / destruction
    
    basic_streambuf( std::ios_base::openmode which )
        : _buffer()
        , _openmode( which )
        , _gsegment( NOSEGMENT )
        , _psegment( NOSEGMENT )
    {
    }
    
    basic_streambuf( const flowvr::BufferWrite & buf, std::ios_base::openmode mode = std::ios_base::out|std::ios_base::in )
        : _buffer( buf )
        , _openmode( mode )
        , _gsegment( NOSEGMENT )
        , _psegment( NOSEGMENT )
    {
        buildSegments();
        // get first non empty buffer
        if ( ! _segmentv.empty() )
        {
            setSegment( 0, _openmode );
        }
    }
    
    basic_streambuf( const flowvr::Buffer & buf, std::ios_base::openmode which = std::ios_base::in )
        : _buffer()
        , _openmode( which )
        , _gsegment( NOSEGMENT )
        , _psegment( NOSEGMENT ) // can't use a mere 'Buffer' as output
    {
        _buffer.Buffer::operator = ( buf ); // const_cast
        buildSegments();
        // get first non empty segment
        if ( ! _segmentv.empty() )
        {
            setSegment( 0, std::ios_base::in );
        }
    }
    
    //////////////////////////////////////////
    // Public methods
    
    const Buffer & gBuffer() const
    {
        return _buffer;
    }
    
    const BufferWrite & gBufferWrite() const
    {
        return _buffer;
    }
    
    const Buffer pBuffer() const
    {
        return Buffer( _buffer, 0, _psegOffset + (this->pptr()) - this->pbase());
    }
    
    const BufferWrite pBufferWrite() const
    {
        return BufferWrite( _buffer, 0, _psegOffset + (this->pptr()) - this->pbase());
    }
    
    void bufferWrite( const BufferWrite & buf )
    {
        _buffer = buf;
        buildSegments();
        // get first non empty segment
        if ( _segmentv.empty() )
        {
            unsetSegment( _openmode );
        }
        else
        {
            setSegment( 0, _openmode );
        }
        _psegOffset = 0;
    }
    
    void buffer( const Buffer & buf )
    {
        _buffer.Buffer::operator = ( buf ); // const_cast
        buildSegments();
        // get first non empty segment
        if ( _segmentv.empty() )
        {
            unsetSegment( _openmode );
        }
        else
        {
            // can't use a mere 'Buffer' as output
            unsetSegment( std::ios_base::out );
            setSegment( 0, std::ios_base::in );
        }
        _psegOffset = 0;
    }

    
protected:
    
    //////////////////////////////////////////
    // protected virtual methods overriding

//    virtual void imbue(const locale& loc);
    
    virtual parent_type* setbuf(char_type*, streamsize)
    {
        // always synchronized, with regular access time, so no buffer
        return this->parent_type::setbuf( NULL, 0 );
    }
    
//    virtual pos_type seekoff(off_type off,
//                            ios_base::seekdir dir,
//                            ios_base::openmode mode = ios_base::in | ios_base::out)
//    {
////        return parent_type::seekoff( off, dir, mode );
//    }
    
//    virtual pos_type seekpos(pos_type pos,
//                            ios_base::openmode mode = ios_base::in | ios_base::out)
//    {
////        return parent_type::seekpos( pos, mode );
//    }
    
    
    // virtual int sync() { return parent_type::sync(); } // always synchronized
    
    /// get number of available characters available in the controlled sequence
    virtual std::streamsize showmanyc()
    {
        const size_t end = _segmentv.size();
        // count the number of available characters in current 'get' segment
        size_t left = this->egptr() - this->gptr();
        // add the total sizes of the following segments
        for ( size_t i = _gsegment + 1 ;  i < end  ;  i++ )
        {
            left += _segmentv[i].end - _segmentv[i].begin;
        }
        return left;
    }
    
//    /// Get sequence of characters
//    virtual streamsize xsgetn(char_type *, streamsize);
    
    /// segment completly read, use the next one
    virtual int_type underflow()
    {
        if ( _gsegment + 1 < _segmentv.size() )
        {
             char_type *p = setSegment( _gsegment+1, std::ios_base::in );
             return traits::to_int_type( *p );
        }
        return traits_type::eof();
    }
    
    /// segment completely written, use the next one
    virtual int_type overflow( int_type c = traits_type::eof() )
    {
        // a segment is available
        if ( _psegment + 1 < _segmentv.size() )
        {
            // update offset for the new segment (by adding size of the previous one)
            _psegOffset += _segmentv[ _psegment ].size();
            // set new segment
            char_type *p = setSegment( _psegment+1, std::ios_base::out );
            // consume the character given in parameter
            if ( c != traits_type::eof() ) {
                this->sputc( c );
            }
            return traits::to_int_type( *p ); // return "not eof"
        }
        // it's possible to resize (if we have segments, we have an allocator)
        else if ( _psegment != NOSEGMENT )
        {
            flowvr::Allocator *allocator = _buffer.getAllocator();
            if ( allocator != NULL )
            {
                const size_t newSize = _buffer.getSize( 0 ); //flowvr::Buffer::ALLSEGMENTS );
                const BufferWrite buf = allocator->alloc( newSize );
                if ( buf.valid() && buf.getSize() > 0 )
                {
                    // update offset for the new segment (by adding size of the previous one)
                    _psegOffset += _segmentv[ _psegment ].size();
                    // append new segment
                    _buffer += buf;
                    // set new segment
                    buildSegments();
                    char_type *p = setSegment( _psegment+1, std::ios_base::out );
                    // consume the character given in parameter
                    if ( c != traits_type::eof() ) {
                        this->sputc( c );
                    }
                    return traits::to_int_type( *p ); // return "not eof"
                }
            }
        }
        // couldn't supply a new put area/segment
        return traits_type::eof();
    }
  
//    /// uderflow and advance pointer
//    virtual int_type uflow();

    
    /// Put character back in the case of backup underflow
    virtual int_type pbackfail( int_type c = traits_type::eof() )
    {
        if ( 0 != _gsegment && NOSEGMENT != _gsegment )
        {
            const segment_t & segment = _segmentv[ _gsegment-1 ];// prev segment
            char_type * const p = segment.end-1;// last element of prev segment
            // if you're asked to write a character
            if ( traits::not_eof( c ) && !traits::eq( c, *p ) )
            {
                // then you need the right to do so
                if ( (! _openmode) & ios_base::out ) // added intern parenthesis to avoid ambiguity- Not 100% sure this is right
                    return traits_type::eof();
                traits::assign( *p, c );
            }
            // set new area / gptr
            _gsegment = _gsegment-1;
            this->setg( segment.begin, p, segment.end );
            return traits::to_int_type( c );
        }
        return traits_type::eof();
    }

//    /// Write sequence of characters 
//    virtual streamsize xsputn( const char_type *, streamsize );
    
    
private:
    
    //////////////////////////////////////////
    // Private types
    
    struct segment_t
    {
        char_type * begin;
        char_type * end;
        inline streamsize size() const { return end - begin; }
    };
    
    /// nbSegments < NOSEGMENT <= SIZE_MAX
    /// nbSegments < NOSEGMENT+1 <= SIZE_MAX
    enum { NOSEGMENT = size_t(-2) };
    
    //////////////////////////////////////////
    // Private members
    
    flowvr::BufferWrite     _buffer;        ///< associated character sequence
    std::vector<segment_t>  _segmentv;      ///< vector of segments
    std::ios_base::openmode _openmode;      ///< open mode (i,o,io)
    streamsize              _psegment;      ///< current put segment
    streamsize              _gsegment;      ///< current get segment
    size_t                  _psegOffset;    ///< psegment offset within buffer
    
    //////////////////////////////////////////
    // Private method
    
    /// unset an accessible controlled i/o sequence
    void unsetSegment( std::ios_base::openmode which )
    {
        if ( which & std::ios_base::in )
        {
            _gsegment = NOSEGMENT;
            this->setg( NULL, NULL, NULL );
        }
        if ( which & std::ios_base::out )
        {
            _psegment = NOSEGMENT;
            this->setp( NULL, NULL );
        }
    }
    
    /// set one segment as the new accessible controlled i/o sequence
    /// _openmode is checked before performing any modification
    char_type* setSegment( size_t s, std::ios_base::openmode which )
    {
        const segment_t & segment = _segmentv[ s ];
        // set up get area
        if ( _openmode & which & std::ios_base::in )
        {
            _gsegment = s;
            this->setg( segment.begin, segment.begin, segment.end );
        }
        // set up put area
        if ( _openmode & which & std::ios_base::out )
        {
            _psegment = s;
            this->setp( segment.begin, segment.end );
        }
        return segment.begin;
    }
    
    /// build \c _segmentv from \c _bufferFlowvr, filtering invalid/empty segments
    void buildSegments()
    {
        const std::vector<Buffer::_bufferDesc> &segv = _buffer.getSegments();
        _segmentv.clear();
        _segmentv.resize( _buffer.getSegments().size() );
        size_t j = 0;
        for ( size_t i = 0, end = segv.size()  ;  i < end  ;  i++ )
        {
            if ( segv[ i ].valid()  &&  0 != segv[ i ].size )
            {
                char_type * p = _buffer.getWrite< char_type >( 0, i );
                const size_t nbytes = _buffer.getSegmentSize( i );
                const size_t size = nbytes / sizeof( char_type );
                const int_type leftover = nbytes % sizeof( char_type );
                if ( 0 != leftover )
                    std::cerr << "WARNING: buffer used for flowvr::streambuf has some leftover" << std::endl;
                _segmentv[j].begin = p;
                _segmentv[j].end = p + size;
                j++;
            }
        }
        _segmentv.resize( j );
    }
};


typedef basic_streambuf< char > streambuf;


} // namespace utils

} // namespace flowvr

#endif // FLOWVR_UTILS_STREAMBUF_H

