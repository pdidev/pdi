#include"flowvr/utils/size.h"

#include <climits>
#include <cstdlib>

namespace flowvr
{
namespace utils
{

//////////////////////////////////////////////////////////////////////
// assignment operators

Size& Size::operator = ( const Size & s )
{
    _count = s._count;
    _unit_shift = s._unit_shift;
    return *this;
}

Size& Size::operator = ( const size_t & s )
{
    // affect value
    _count = s;
    _unit_shift = 0;
    // find out what unit to use
    findUnit();
    return *this;
}

Size& Size::operator = ( const char * s )
{
    // read string
    long long int count = -1; // number read by \c strtol
    char *c_p;         // first non-numerical character
    if ( s != NULL )
        count = strtoll( s, &c_p, 10 );
    
    // handle any invalid input
    if ( s == NULL /* no input */ || c_p == s           // read error
         || count == LLONG_MIN || count == LLONG_MAX    // overflow error
         || count < 0 || count > size_t(-1) )           // range error
        return this->invalidate();
    
    // no unit (postfix alphabetic character) given
    if ( ! isalpha( *c_p ) )
        return *this = count; // simply affect the size_t value
    
    // a unit has been given
    _count = count;
    _unit_shift = unit2shift( toupper( *c_p ) ); // (ignores invalid units)
    return *this;
}

//////////////////////////////////////////////////////////////////////
// Public methods

/**
 * @brief round round using the bigger unit giving a non-zero truncation
 */
void Size::round()
{
    long int shift;
    long int size = *this;
    // compute the maximum unit you can use
    for ( shift = maxShift()  ;  shift >= 0  ;  shift -= 10 )
    {
        if ( size >> shift != 0 )
            break;
    }
    // round to this unit
    round( shift2unit( shift ) );
}

/**
 * @brief round 
 * @param unit_c the unit to which you want to round
 */
void Size::round( char unit_c )
{
    size_t shift = unit2shift( unit_c );
    // add half a unit
    size_t halfUnit = (size_t(1) << shift) / 2;
    size_t size = *this + halfUnit;
    // truncate
    _count = size >> shift;
    _unit_shift = shift;
}


/**
 * @brief findUnit find the biggest unit you can used without rounding
 */
void Size::findUnit()
{
    size_t shift;
    size_t size = *this;
    // compute the maximum unit you can use
    for ( shift = 10  ;  shift <= maxShift()  ;  shift += 10 )
    {
        if ( (size >> shift) == 0 )
        {
            shift -= 10;
            break;
        }
    }
    // if needed, update each member accordingly
    if ( shift <= maxShift() && shift != _unit_shift )
    {
        _count = size >> shift;
        _unit_shift = shift;
    }
}

//////////////////////////////////////////////////////////////////////
// stream operator

std::istream& operator >> ( std::istream& i, flowvr::utils::Size & s )
{
    // return immediatly if the stream is not good for i/o operations
    if ( ! i.good() )
        return i;
    
    // extract a string
    std::string str;
    str.reserve( 32 );
    do
    {
        char c = i.peek();
        // extract digits
        if ( isdigit( c ) )
            str.push_back( i.get() );
        // ignore prefix space characters
        else if ( str.empty() && isspace( c ) )
            i.get();
        // terminal caracter
        else
        {
            // keep an optionnal one-letter suffix (the unit)
            if ( isalpha( c ) )
                str.push_back( i.get() );
            break;
        }
    }
    while( i.good() );

    // interpret the string
    s = str;
    if ( ! s.isValid() ) // error checking
        i.setstate( std::istream::failbit );
    return i;
}


} // namespace utils
} // namespace flowvr
