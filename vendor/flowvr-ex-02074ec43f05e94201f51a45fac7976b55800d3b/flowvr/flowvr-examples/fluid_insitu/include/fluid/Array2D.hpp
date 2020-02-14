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
* File: ./Array2D.hpp                                             *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#ifndef HPP_ARRAY2D
#define HPP_ARRAY2D


#include <cassert>
#include <cstddef>


template <class T>
class const_Array2D
{
public :
    
    inline 
    const_Array2D( size_t nr, size_t nc, const T* data )
        : _nrow( nr )
        , _ncol( nc )
        , _data( data )
    {}
    
    inline 
    ~const_Array2D() {}
    
    inline const T* operator [] ( size_t i) const { 
        assert( _data != NULL );
        assert( i < _nrow );
        return &_data[i * _ncol];
    }
    inline const T& operator () ( size_t x, size_t y ) const {
        assert( _data != NULL );
        assert( x < _nrow );
        assert( y < _ncol );
        return (*this)[x][y];
    }
    
    // Public const methods
    inline size_t       height() const  { return _nrow; }
    inline size_t       width() const   { return _ncol; }
    inline size_t       size() const    { return _nrow * _ncol; }
    inline const T *    begin() const   { return _data; }
    inline const T *    end() const     { return _data + size(); }
    
private :

    size_t      _nrow; // number of rows
    size_t      _ncol; // number of columns
    const T *   _data; // dynamic array containing the values
};




template <class T>
struct Array2D : public const_Array2D<T>
{
    inline 
    Array2D( size_t nr, size_t nc, T* data )
        : const_Array2D<T>( nr, nc, data )
    {}
    
    inline 
    ~Array2D() {}
    
    inline T* operator [] ( size_t i) { 
        return const_cast< T* >( this->const_Array2D<T>::operator[](i) );
    }
    inline T& operator () ( size_t x, size_t y ) {
        return const_cast< T& >( this->const_Array2D<T>::operator()(x,y) );
    }
    
    inline T* begin() {
        return const_cast< T* >( this->const_Array2D<T>::begin() );
    }
    inline T* end() {
        return const_cast< T* >( this->const_Array2D<T>::end() );
    }
};



#endif // guard
