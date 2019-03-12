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
* File: include/utils/iostream.h                                  *
*                                                                 *
* Contacts: 01/02/2013 Jeremy Jaussaud <jeremy.jaussaud@free.fr>  *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_IOSTREAM_H
#define FLOWVR_UTILS_IOSTREAM_H

#include <flowvr/utils/streambuf.h>

//#include <locale.h>
#include <iostream>

namespace flowvr
{

namespace utils
{


template< typename charT, typename traits = std::char_traits< charT > >
class basic_streambuf_wrapper
{
protected:
    typedef basic_streambuf< charT, traits > streambuf_type;
    
    ///////////////////////////////////
    // constructors
    
    basic_streambuf_wrapper( std::ios_base::openmode mode )
        : _streambuf( mode )
    {}
    
    basic_streambuf_wrapper( const flowvr::Buffer & buf, std::ios_base::openmode mode )
        : _streambuf( buf, mode )
    {}

    basic_streambuf_wrapper( const flowvr::BufferWrite & buf, std::ios_base::openmode mode )
        : _streambuf( buf, mode )
    {}
    
    ///////////////////////////////////
    // accessor
        
    streambuf_type& get_streambuf() const { return _streambuf; }
    
private:
    
    mutable streambuf_type _streambuf;
};



//////////////////////////////////////////////////////////////////////
// input stream
//////////////////////////////////////////////////////////////////////

/**
 * @brief basic_bufferstream allow to access a FlowVR Buffer through a
 *  std::istream, in the fashion of std::istringstream.
 */
template< typename charT, typename traits = std::char_traits< charT > >
class basic_ibufferstream
        : private basic_streambuf_wrapper< charT, traits >
        , public std::basic_istream< charT, traits >
{
    typedef basic_streambuf_wrapper< charT, traits > streambuf_wrapper_type;
    typedef typename std::basic_istream< charT, traits > parent_type;
public:
    
    /// alias of the underlying streambuf type used
    typedef typename streambuf_wrapper_type::streambuf_type streambuf_type;
    
    ///////////////////////////////////
    // constructor
    
    basic_ibufferstream( const Buffer & buf )
        : streambuf_wrapper_type( buf, std::ios_base::in )
        , parent_type( rdbuf() )
    {}
    
    ///////////////////////////////////
    // accessors
    
    /// Access underlying streambuf
    streambuf_type* rdbuf() const { return & this->get_streambuf(); }
    
    /// Access underlying buffer
    const Buffer& buffer() const { return rdbuf()->buffer(); }
    
    ///////////////////////////////////
    // mutator
    
    /// Change underlying buffer
    void buffer( const Buffer & buf ) { rdbuf()->buffer( buf ); }
};

//////////////////////////////////////////////////////////////////////
// output stream
//////////////////////////////////////////////////////////////////////

/**
 * @brief basic_bufferstream allow to access a FlowVR Buffer through a
 *  std::ostream, in the fashion of std::ostringstream.
 */
template< typename charT, typename traits = std::char_traits< charT > >
class basic_obufferstream
        : private basic_streambuf_wrapper< charT, traits >
        , public std::basic_ostream< charT, traits >
{
    typedef basic_streambuf_wrapper< charT, traits > streambuf_wrapper_type;
    typedef typename std::basic_ostream< charT, traits > parent_type;
public:
    
    /// alias of the underlying streambuf type used
    typedef typename streambuf_wrapper_type::streambuf_type streambuf_type;
    
    ///////////////////////////////////
    // constructor
    
    basic_obufferstream( const BufferWrite & buf )
        : streambuf_wrapper_type( buf, std::ios_base::out )
        , parent_type( rdbuf() )
    {}
    
    ///////////////////////////////////
    // accessors
    
    /// Access underlying streambuf
    streambuf_type* rdbuf() const { return & this->get_streambuf(); }
    
    ///@}
    /// Access underlying buffer
    const Buffer& buffer() const { return rdbuf()->buffer(); }
    /// Access underlying buffer
    const BufferWrite& bufferWrite() const { return rdbuf()->bufferWrite(); }
    ///@}
    
    ///////////////////////////////////
    // mutators
    
    /// Change underlying buffer
    void bufferWrite( const BufferWrite & buf ) { rdbuf()->bufferWrite( buf ); }
};


//////////////////////////////////////////////////////////////////////
// input/output stream
//////////////////////////////////////////////////////////////////////

/**
 * @brief basic_bufferstream allow to access a FlowVR Buffer through a
 *  std::iostream, in the fashion of std::stringstream.
 */
template< typename charT, typename traits = std::char_traits< charT > >
class basic_iobufferstream
        : private basic_streambuf_wrapper< charT, traits >
        , public std::basic_iostream< charT, traits >
{
    typedef basic_streambuf_wrapper< charT, traits > streambuf_wrapper_type;
    typedef typename std::basic_iostream< charT, traits > parent_type;
public:
    
    /// alias of the underlying streambuf type used
    typedef typename streambuf_wrapper_type::streambuf_type streambuf_type;
    
    ///////////////////////////////////
    // constructor
    
    basic_iobufferstream( const BufferWrite & buf, std::ios_base::openmode mode = std::ios_base::out|std::ios_base::in )
        : streambuf_wrapper_type( buf, mode )
        , parent_type( rdbuf() )
    {}
    
    ///////////////////////////////////
    // accessors
    
    /// Access underlying streambuf
    streambuf_type* rdbuf() const { return & this->get_streambuf(); }
    
    ///@{
    /// Access underlying buffer
    const Buffer& buffer() const { return rdbuf()->buffer(); }
    const BufferWrite& bufferWrite() const { return rdbuf()->bufferWrite(); }
    ///@}
    
    ///////////////////////////////////
    // mutators
    
    ///@{
    /// Change underlying buffer
    void buffer( const Buffer & buf ) { rdbuf()->buffer( buf ); }
    void bufferWrite( const BufferWrite & buf ) { rdbuf()->bufferWrite( buf ); }
    ///@}
};



//////////////////////////////////////////////////////////////////////
// aliases
//////////////////////////////////////////////////////////////////////

typedef basic_ibufferstream< char > ibufferstream;
typedef basic_obufferstream< char > obufferstream;
typedef basic_iobufferstream< char > iobufferstream;


} // namespace utils

} // namespace flowvr

#endif

