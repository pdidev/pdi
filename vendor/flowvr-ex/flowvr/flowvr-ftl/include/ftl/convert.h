/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Application Library                         *
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
 * File: ./include/ftl/convert.h                                   *
 *                                                                 *
 * Contacts:  Bruno Raffin                                         *
 *                                                                 *
 ******************************************************************/

/**
 * \file convert.h
 * \brief utility class for converting  from/to std::string
 */


#ifndef __FTL_CONVERT_H
#define __FTL_CONVERT_H


#include <string>
#include <sstream>
#include <stdexcept>
#include <iostream>
#include <algorithm>

namespace ftl
{

    class BadConversion : public std::runtime_error
    {
        public:
            BadConversion(const std::string& s): std::runtime_error(s)
            { }
    };


    template<typename T>
        inline bool convert(const std::string& s, T& x,
                            bool failIfLeftoverChars = true)
    {
        std::istringstream i(s);
        char c;
        if (!(i >>std::boolalpha >> x) || (failIfLeftoverChars && i.get(c)))
            return false;
        return true;
    }




	template<typename T>
        inline T convertTo(const std::string& s,
                           bool failIfLeftoverChars = true)
    {
        T x;
        if(convert(s, x, failIfLeftoverChars) == false)
        	throw BadConversion(std::string("failed to convert [") + s + std::string("]"));
        return x;
    }


    template < class T >
    std::string toString( const T& data )
    {
        std::stringstream strm;
        strm << std::boolalpha <<data;
        return strm.str();
    }

	inline std::string &ltrim( std::string &s )
	{
		s.erase( s.begin(), std::find_if( s.begin(), s.end(), std::not1(std::ptr_fun<int,int>(std::isspace))));
		return s;
	}

	inline std::string &rtrim( std::string &s )
	{
		s.erase( std::find_if( s.rbegin(), s.rend(), std::not1( std::ptr_fun<int,int>(std::isspace))).base(), s.end() );
		return s;
	}

	inline std::string &trim( std::string &s )
	{
		return ltrim( rtrim(s) );
	}


}; // end namespace

#endif //__FTL_CONVERT_H
