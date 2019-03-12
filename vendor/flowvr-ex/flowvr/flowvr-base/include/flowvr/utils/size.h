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
*    Jeremy Jaussaud.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/utils/size.h                                      *
*                                                                 *
* Contacts: 20/01/2013 Jeremy Jaussaud <jeremy.jaussaud@free.fr>  *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_SIZE_H
#define FLOWVR_UTILS_SIZE_H

#include"ftl/type.h"

#include<iostream>

namespace flowvr
{
namespace utils
{

/**
 * @brief The Size class allow to manipulate sizes (in bytes) using larger units
 *  (K for Kilobyte, M for Megabyte and G for Gigabyte)
 */
class Size
{
public:
    
    //////////////////////////////////////////////////////////////////////
    // Static function
    
    /// allow to use it as a template argument of \c flowvr::utils::Option 
    static ftl::Type::Type getType() { return ftl::Type::get( std::string() ); }
    
    //////////////////////////////////////////////////////////////////////
    // Construction / destruction

    /// default constructor
    Size()
    {
        invalidate();
    }
    
    /// copy constructor
    Size( const Size & s )
        : _count( s._count )
        , _unit_shift( s._unit_shift )
    {}
    
    /// construct from a simple \c size_t
    Size( const size_t & s )
        : _count( s )
        , _unit_shift( 0 )
    {
        findUnit();
    }
    
    /// construct, from a std::string, either a valid or an invalided instance
    Size( const std::string & s ) { *this = s.c_str(); }
    
    /// construct, from a c string, either a valid or an invalided instance
    Size( const char * s ) { *this = s; }
    
    //////////////////////////////////////////////////////////////////////
    // Accessors
    
    size_t getSize() const { return _count << _unit_shift; } /// get the stored size
    size_t getCount() const { return _count; }  /// get the stored size in units
    char   getUnit() const { return shift2unit( _unit_shift ); } /// get the units currently used
    bool   isValid() const { return -1 != _unit_shift; } /// return true if the instance is valid
    
    //////////////////////////////////////////////////////////////////////
    // casting operators
    
    /// cast a valid isntance to the built-in type \c size_t
    operator size_t () const { return getSize(); }
    
    //////////////////////////////////////////////////////////////////////
    // assignment operators
    
    Size& operator = ( const Size & s );
    Size& operator = ( const size_t & s );
    Size& operator = ( const std::string & s ) { *this = s.c_str(); return *this;}
    Size& operator = ( const char * s );
    
    //////////////////////////////////////////////////////////////////////
    // Public methods
    
    Size& invalidate() { _unit_shift = -1; return *this; }
    
    /**
     * @brief round round using the bigger unit giving a non-zero truncation
     */
    void round();
    
    /**
     * @brief round 
     * @param unit_c the unit to which you want to round
     */
    void round( char unit_c );

    
protected:
    
    //////////////////////////////////////////////////////////////////////
    // Protected static functions
        
    static size_t maxShift() { return 30; }
    
    static char shift2unit( size_t shift )
    {
        switch ( shift )
        {
        default: return 'b';
        case 10: return 'K';
        case 20: return 'M';
        case 30: return 'G';
        }
    }
    
    static char unit2shift( char unit )
    {
        switch ( unit )
        {
        default:   return  0;
        case 'K' : return 10;
        case 'M' : return 20;
        case 'G' : return 30;
        }
    }
    
    //////////////////////////////////////////////////////////////////////
    // Protected method
    
    /**
     * @brief findUnit find the biggest unit you can used without rounding
     */
    void findUnit();

    
private:
    
    //////////////////////////////////////////////////////////////////////
    // Private members
    
    size_t _count;      ///< size in the given unit (bytes, Kb, Mb, Gb)
    size_t _unit_shift; ///< bitwise shift needed to get the number of bytes
    
    //////////////////////////////////////////////////////////////////////
    // Private Constructor
    
    Size( size_t count, size_t unit_shift )
        : _count( count )
        , _unit_shift( unit_shift )
    {}
    
};

//////////////////////////////////////////////////////////////////////
// stream operators

inline
std::ostream& operator << ( std::ostream& o, const flowvr::utils::Size & s ) { return o << s.getCount() << s.getUnit(); }
std::istream& operator >> ( std::istream& i, flowvr::utils::Size & s );


} // namespace flowvr
} // namespace utils


#endif
