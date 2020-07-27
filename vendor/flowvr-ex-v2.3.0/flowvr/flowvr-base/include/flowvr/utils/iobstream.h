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
#ifndef FLOWVR_UTILS_IOBSTREAM_H
#define FLOWVR_UTILS_IOBSTREAM_H

#include <flowvr/utils/iostream.h>

namespace flowvr
{

namespace utils
{


//////////////////////////////////////////////////////////////////////
// input
//////////////////////////////////////////////////////////////////////

template< class istreamT,
          typename charT = typename istreamT::char_type,
          typename traits = typename istreamT::traits_type >
class basic_ibstream
        : protected virtual istreamT
{
    typedef typename traits::int_type int_type;
    typedef typename traits::char_type char_type;
    typedef typename traits::pos_type pos_type;
    typedef typename traits::off_type off_type;
    
public:
    
    struct const_bdata
    {
        template< class T >
        const_bdata( const T& t )
            : _ptr( reinterpret_cast< charT* >(  const_cast< T* >( &t )  ) )
            , _size( sizeof( T ) )
        {}
        const charT* ptr() const { return _ptr; }
        size_t size() const { return _size; }
    protected:
        charT* _ptr;
        size_t _size;
    private:
        const_bdata();
        const_bdata(const const_bdata &);
    };
    
    struct bdata : public const_bdata
    {
        template< class T >
        bdata( T& x )
            : const_bdata( x )
        {}
        charT* ptr() const { return this->_ptr; }
    private:
        bdata();
        bdata(const bdata &);
    };
    
    ///////////////////////////////////
    // constructor
    
    basic_ibstream( std::streambuf * buf )
        : istreamT( buf )
    {}
    
    basic_ibstream( std::istream & stream )
        : istreamT( stream.rdbuf() )
    {}
    
    ///////////////////////////////////
    // extraction operators methods
    
    basic_ibstream& operator>> ( const bdata & data )
    {
        this->istreamT::read( data.ptr(), data.size() );
        return *this;
    }

    basic_ibstream& operator>> (bool& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (short& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (unsigned short& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (int& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (unsigned int& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (long& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (unsigned long& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (float& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (double& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (long double& val ) { return *this >> bdata(val); }
    basic_ibstream& operator>> (void*& val ) { return *this >> bdata(val); }
     
    basic_ibstream& operator>> (std::streambuf* sb ) { return *this >> sb; }
    
    basic_ibstream& operator>> (std::istream& ( *pf )(std::istream&)) { return *this >> pf, *this; }
    basic_ibstream& operator>> (std::ios& ( *pf )(std::ios&)) { return *this >> pf, *this; }
    basic_ibstream& operator>> (std::ios_base& ( *pf )(std::ios_base&)) { return *this >> pf, *this; }
    
    ///////////////////////////////////
    // extraction operators functions
    
    friend basic_ibstream& operator>> (basic_ibstream& is, char& ch ) { return is >> bdata(ch); }
    friend basic_ibstream& operator>> (basic_ibstream& is, signed char& ch ) { return is >> bdata(ch); }
    friend basic_ibstream& operator>> (basic_ibstream& is, unsigned char& ch ) { return is >> bdata(ch); }
     
    friend basic_ibstream& operator>> (basic_ibstream& is, char* str ) { return is >> str; }
    friend basic_ibstream& operator>> (basic_ibstream& is, signed char* str ) { return is >> str; }
    friend basic_ibstream& operator>> (basic_ibstream& is, unsigned char* str ) { return is >> str; }
    
    ///////////////////////////////////
    // Public Methods
    
    basic_ibstream& readBool ( bool& val ) { return *this >> bdata(val); }
    basic_ibstream& readShort ( short& val ) { return *this >> bdata(val); }
    basic_ibstream& readUShort ( unsigned short& val ) { return *this >> bdata(val); }
    basic_ibstream& readInt ( int& val ) { return *this >> bdata(val); }
    basic_ibstream& readUInt ( unsigned int& val ) { return *this >> bdata(val); }
    basic_ibstream& readLong ( long& val ) { return *this >> bdata(val); }
    basic_ibstream& readULong ( unsigned long& val ) { return *this >> bdata(val); }
    basic_ibstream& readFloat ( float& val ) { return *this >> bdata(val); }
    basic_ibstream& readDouble ( double& val ) { return *this >> bdata(val); }
    basic_ibstream& readLDouble ( long double& val ) { return *this >> bdata(val); }
    basic_ibstream& readPointer ( void*& val ) { return *this >> bdata(val); }

};


//////////////////////////////////////////////////////////////////////
// output
//////////////////////////////////////////////////////////////////////




template< class ostreamT,
          typename charT = typename ostreamT::char_type,
          typename traits = typename ostreamT::traits_type >
class basic_obstream
        : protected virtual ostreamT
{
public:
    
    typedef typename basic_ibstream< ostreamT >::bdata bdata;
    typedef typename basic_ibstream< ostreamT >::const_bdata const_bdata;
    
    ///////////////////////////////////
    // constructor
    
    basic_obstream( std::streambuf * buf )
        : ostreamT( buf )
    {}
    
    basic_obstream( std::ostream & stream )
        : ostreamT( stream.rdbuf() )
    {}
    
    ///////////////////////////////////
    // extraction operators methods
    
    basic_obstream& operator<< ( const const_bdata & data )
    {
        this->ostreamT::write( data.ptr(), data.size() );
        return *this;
    }

    basic_obstream& operator<< (bool val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (short val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (unsigned short val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (int val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (unsigned int val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (long val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (unsigned long val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (float val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (double val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (long double val ) { return *this << const_bdata(val); }
    basic_obstream& operator<< (void* val ) { return *this << const_bdata(val); }
     
    basic_obstream& operator<< (std::streambuf* sb ) { return *this << sb; }
    
    basic_obstream& operator<< (std::istream& ( *pf )(std::istream&)) { return *this << pf, *this; }
    basic_obstream& operator<< (std::ios& ( *pf )(std::ios&)) { return *this << pf, *this; }
    basic_obstream& operator<< (std::ios_base& ( *pf )(std::ios_base&)) { return *this << pf, *this; }
    
    ///////////////////////////////////
    // extraction operators functions
    
    friend basic_obstream& operator<< (basic_obstream& out, char ch ) { return out << const_bdata(ch); }
    friend basic_obstream& operator<< (basic_obstream& out, signed char ch ) { return out << const_bdata(ch); }
    friend basic_obstream& operator<< (basic_obstream& out, unsigned char ch ) { return out << const_bdata(ch); }
     
    friend basic_obstream& operator<< (basic_obstream& out, const char* str ) { return out << str; }
    friend basic_obstream& operator<< (basic_obstream& out, const signed char* str ) { return out << str; }
    friend basic_obstream& operator<< (basic_obstream& out, const unsigned char* str ) { return out << str; }
    
    ///////////////////////////////////
    // Public Methods
    
    basic_obstream& writeBool ( bool val ) { return *this << const_bdata(val); }
    basic_obstream& writeShort ( short val ) { return *this << const_bdata(val); }
    basic_obstream& writeUShort ( unsigned short val ) { return *this << const_bdata(val); }
    basic_obstream& writeInt ( int val ) { return *this << const_bdata(val); }
    basic_obstream& writeUInt ( unsigned int val ) { return *this << const_bdata(val); }
    basic_obstream& writeLong ( long val ) { return *this << const_bdata(val); }
    basic_obstream& writeULong ( unsigned long val ) { return *this << const_bdata(val); }
    basic_obstream& writeFloat ( float val ) { return *this << const_bdata(val); }
    basic_obstream& writeDouble ( double val ) { return *this << const_bdata(val); }
    basic_obstream& writeLDouble ( long double val ) { return *this << const_bdata(val); }
    basic_obstream& writePointer ( void* val ) { return *this << const_bdata(val); }
    
    basic_obstream& write ( const char* s , std::streamsize n )
    {
        this->ostreamT::write( s, n );
        return *this;
    }

};


//////////////////////////////////////////////////////////////////////
// input/output
//////////////////////////////////////////////////////////////////////


template< class iostreamT,
          typename charT = typename iostreamT::char_type,
          typename traits = typename iostreamT::traits_type >
class basic_iobstream
        : public basic_ibstream< iostreamT >
        , public basic_obstream< iostreamT >
{
public:
    
    typedef typename basic_ibstream< iostreamT >::bdata bdata;
    typedef typename basic_ibstream< iostreamT >::const_bdata const_bdata;

    ///////////////////////////////////
    // constructor
    
    basic_iobstream( std::streambuf * buf )
        : basic_ibstream< iostreamT >( buf )
        , basic_obstream< iostreamT >( buf )
    {}
    
    basic_iobstream( iostreamT & stream )
        : basic_ibstream< iostreamT >( stream.rdbuf() )
        , basic_obstream< iostreamT >( stream.rdbuf() )
    {}

};








//////////////////////////////////////////////////////////////////////
// aliases
//////////////////////////////////////////////////////////////////////

typedef basic_ibstream< std::istream > ibstream;
typedef basic_obstream< std::ostream > obstream;
typedef basic_iobstream< std::iostream > iobstream;

typedef basic_ibstream< ibufferstream > ibufferbstream;
typedef basic_obstream< obufferstream > obufferbstream;
typedef basic_iobstream< iobufferstream > iobufferbstream;

//template class basic_ibstream< std::istream >;
//template class basic_obstream< std::ostream >;
//template class basic_iobstream< std::iostream >;


} // namespace utils

} // namespace flowvr


#endif
