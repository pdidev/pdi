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
*                                                                 * 
*******************************************************************
*                                                                 *
* File: buffer-test.cpp
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include <flowvr/allocator.h>
#include <flowvr/buffer.h>
#include <flowvr/mem/memorymanager.h>

#include <flowvr/utils/timing.h>
#include <flowvr/utils/hexdump.h>
#include <unistd.h>
#include <cmath>

#include <iostream>
#include <algorithm>
#include <vector>

#include <string.h>

using namespace flowvr;
using namespace flowvr::mem;

// ############################################################################
// UTIL STUFF
// ############################################################################
void init()
{
	double y, seed = flowvr::utils::getNtpTimeStamp();
	modf(seed,&y);
	srand( seed );
}

int genRand( int nMin, int nMax )
{
	return nMin + (int)( (float)( nMax-nMin + 1 ) * ( rand() / (RAND_MAX + 1.0) ) );
}

// ############################################################################
// TEST DRIVER
// ############################################################################


bool state( const char *pcName, bool bCompleted )
{
	std::cout << pcName << (bCompleted ? ": OK" : ": FAILED") << std::endl;
	return bCompleted;
}

#define S_TEST(func) _test(#func, func)
typedef bool (*tfunc)();


struct _test
{
	_test(const char *_funcName, tfunc foo )
	: funcName(_funcName)
	, test(foo) {}

	const char *funcName;
	tfunc       test;
};



class _comp : public std::unary_function<const _test &, void>
{
public:
	_comp( const std::string &strName )
	: m_strName( strName ) {}

	std::string m_strName;

	bool operator()( const _test &other ) const
	{
		return (m_strName == other.funcName);
	}
};

tfunc getFunctionByName( const std::string &strfunction );

void leave( int ret )
{
	delete Allocator::getAllocator();
	exit( ret );
}

// ############################################################################
// TESTS
// ############################################################################

bool testCreateInvalidBuffer()
{
	Buffer b;

	return (b.valid() == false);
}

bool testCreateInvalidBufferWrite()
{
	BufferWrite b;

	return (b.valid() == false);
}

bool test0AllocedBufferValidity()
{
	BufferWrite b = Allocator::getAllocator()->alloc(0);
	return ( b.valid() == true );
}

bool test1ByteBufferNoImp()
{
	BufferWrite b = Allocator::getAllocator()->alloc(1);
	return (b.valid() and b.getSize() == 1);
}

bool testSizedAlloc()
{
	Buffer b = Allocator::getAllocator()->alloc(12345);
	return (b.valid() and b.getSize() == 12345);
}

bool testUnique()
{
	Buffer b = Allocator::getAllocator()->alloc(6675);
	return b.unique();
}

bool testNonUnique()
{
	Buffer b = Allocator::getAllocator()->alloc(7782);
	Buffer c(b);

	return !b.unique();
}


bool testEmptyBuffer()
{
	Buffer b = Allocator::getAllocator()->alloc(555);
	Buffer c;
	Buffer d = Allocator::getAllocator()->alloc(0);

	return c.empty() and !b.empty() and d.empty();

}
bool testSubWindow()
{
	BufferWrite a = Allocator::getAllocator()->alloc( 100 * sizeof(int ) );
	int *i = a.getWrite<int>();
	for( int n = 0; n < 100; ++n )
		i[n] = n;

	// create a window starting at 10, containing 10 ints
	Buffer b( a, 10*sizeof(int), 10*sizeof(int) );

	const int *j = b.getRead<int>();

	int sumA = 0;
	int sumB = 0;
	for( int n=0; n < b.getSize()/sizeof(int); ++n )
	{
		sumA += j[n];
		sumB += (10 + n);
	}

	return (sumA == sumB);
}

bool testNestedSubWindow()
{
	BufferWrite a = Allocator::getAllocator()->alloc( 100 * sizeof(int ) );
	int *i = a.getWrite<int>();
	for( int n = 0; n < 100; ++n )
		i[n] = n;

	// create a window starting at 10, containing 10 ints
	Buffer b( a, 10*sizeof(int), 10*sizeof(int) );
	Buffer c( b,  5*sizeof(int),  3*sizeof(int) );

	const int *j = b.getRead<int>();
	const int *l = c.getRead<int>();

	if( l-j != 5 )
		return false;



	int sumA = 0;
	int sumB = 0;
	for( int n=0; n < 3; ++n )
	{
		sumA += j[n+5];
		sumB += l[n];
	}

	return (sumA == sumB);
}

bool testReference()
{
	Buffer b(Allocator::getAllocator()->alloc(100));

	int nRef = b.getImp()->getCount();

	{
		std::vector<Buffer> vBufs;
		for( int n=0; n < 100; ++n)
			vBufs.push_back(Buffer(b));
	}

	return (nRef == b.getImp()->getCount());
}

bool testResize()
{
	BufferWrite b(Allocator::getAllocator()->alloc(1));

	b.resize(2, true);

	return (b.getSize() == 2);
}

bool testExpand()
{
	BufferWrite b(Allocator::getAllocator()->alloc(1));

	b.expand(2);

	return (b.getSize() == 2);
}

bool testFailedReserve()
{
	BufferWrite b(Allocator::getAllocator()->alloc(10));

	// down-size buffer (stating that down-sizing is not allowed)
	return (!b.reserve(1, true));
}

bool testCopyConstructionEq()
{
	Buffer b(Allocator::getAllocator()->alloc(100));

	Buffer c(b);

	return (c == b);
}

bool testAssignment()
{
	Buffer b(Allocator::getAllocator()->alloc(100));

	Buffer c = b;

	return (c==b);
}

bool testCopyConstructionUnEq()
{
	Buffer b(Allocator::getAllocator()->alloc(100));
	Buffer c(b);

	return !(c != b);
}

bool testComparisonUnEqEq()
{
	// test to see whether we can compare alloc(0) buffers to invalid buffers
	Buffer b;
	Buffer c(Allocator::getAllocator()->alloc(0));

	return (b!=c);
}


bool testSubWindowTooLarge()
{
	Buffer b(Allocator::getAllocator()->alloc(100));
	Buffer c( b, 10, 100 );

	return (c.getSize() == 90);
}

bool testZeroSizeSubWindow()
{
	Buffer b(Allocator::getAllocator()->alloc(100));
	Buffer c( b, 10, 0 );

	return (c.getSize() == 0);
}

bool testBufferToStringOperator()
{
	std::string TestString("This is just a test string!");
	BufferWrite b = Allocator::getAllocator()->alloc(TestString.size());
	memcpy( b.writeAccess(), TestString.c_str(), TestString.size() );

	std::string sOut = (std::string)b;

	return (sOut == TestString);
}

bool testSizeCalculation()
{
	BufferWrite b = Allocator::getAllocator()->alloc(10);

	// get current size (is internally cached)
	size_t n1 = b.getSize();

	// should re-calc size
	b.resize(100);

	return (n1 == 10 and b.getSize() == 100);
}

bool testGetSizeForInvalidBuffer()
{
	Buffer b;

	return (b.getSize(Buffer::ALLSEGMENTS) == 0 and b.getSize() == 0);
}
// ############################################################################
// SEGMENT TESTS
// ############################################################################
class _IFiller : public std::unary_function<int&,void>
{
public:
	virtual void operator()( int &fillMe ) {};

	virtual void fill( int &fillMe ) { (*this)( fillMe ); }
};

class _filler : public _IFiller
{
public:
	_filler(size_t startAt)
	: m_value(startAt) {}

	virtual void operator()( int &fillMe )
	{
		fillMe = m_value++;
	}

	size_t m_value;
};

bool testSegmentCreation()
{
	BufferWrite b0 = Allocator::getAllocator()->alloc(10);
	BufferWrite b1 = Allocator::getAllocator()->alloc(20);

	b0 += b1;

	return (b0.getNumberOfSegments()==2 and b0.getSize(Buffer::ALLSEGMENTS) == 30);
}

bool testSegmentedSubWindow()
{
	BufferWrite b0 = Allocator::getAllocator()->alloc(10);
	BufferWrite b1 = Allocator::getAllocator()->alloc(20);
	BufferWrite b2 = Allocator::getAllocator()->alloc(10);

	b0 += (b1+b2);

	Buffer window( b0, 5, 30 ); // span a subwindow over b0-b2;

	return (window.getNumberOfSegments() == 3) and (window.getSize(Buffer::ALLSEGMENTS) == 30);
}

bool testSegmentedLinearize()
{
	BufferWrite b0 = Allocator::getAllocator()->alloc(10*sizeof(int));
	_filler filler(0);

	for( size_t n = 0; n < b0.getSize()/sizeof(int); ++n )
		filler.fill( *(b0.getWrite<int>( n * sizeof( int ) ) ) );

	filler = _filler(10);
	BufferWrite b1 = Allocator::getAllocator()->alloc(20*sizeof(int));
	for( size_t n = 0; n < b1.getSize()/sizeof(int); ++n )
		filler.fill( *(b1.getWrite<int>( n * sizeof( int ) ) ) );

	filler = _filler(30);
	BufferWrite b2 = Allocator::getAllocator()->alloc(10*sizeof(int));
	for( size_t n = 0; n < b2.getSize()/sizeof(int); ++n )
		filler.fill( *(b2.getWrite<int>( n * sizeof( int ) ) ) );

	b0 += (b1+b2);

	Buffer linearized = b0.linearize();

	size_t sumA = 0;
	size_t sumB = 0;

	for( size_t n = 0; n < b0.getNumberOfSegments(); ++n )
	{
		// for each segment do:
		for( size_t sz = 0; sz < b0.getSegmentSize(n)/sizeof(int); ++sz)
			sumA += *b0.getRead<int>(sz*sizeof(int),n);
	}

	for( size_t n = 0; n < linearized.getNumberOfSegments(); ++n )
	{
		// for each segment do:
		for( size_t sz = 0; sz < linearized.getSegmentSize(n)/sizeof(int); ++sz)
			sumB += *linearized.getRead<int>(sz*sizeof(int),n);
	}

	return (linearized.getNumberOfSegments() == 1) and (linearized.getSize() == 40*sizeof(int))
		and (sumA == sumB);
}


bool testSegmentedSubWindowLinearize()
{
	BufferWrite b0 = Allocator::getAllocator()->alloc( 5 * sizeof( int ) );
	_filler filler(0);
	for( size_t n = 0; n < b0.getSize()/sizeof(int); ++n )
		filler.fill( *(b0.getWrite<int>( n * sizeof( int ) ) ) );

	BufferWrite b1 = Allocator::getAllocator()->alloc( 10* sizeof( int ) );
	for( size_t n = 0; n < b1.getSize()/sizeof(int); ++n )
		filler.fill( *(b1.getWrite<int>( n * sizeof( int ) ) ) );

	BufferWrite b2 = Allocator::getAllocator()->alloc( 3 * sizeof( int ) );
	for( size_t n = 0; n < b2.getSize()/sizeof(int); ++n )
		filler.fill( *(b2.getWrite<int>( n * sizeof( int ) ) ) );

	const int *fb0 = b0.getRead<int>();
	const int *fb1 = b1.getRead<int>();
	const int *fb2 = b2.getRead<int>();

	size_t sumb0 = fb0[2] + fb0[3] + fb0[4];
	size_t sumb1 = fb1[5] + fb1[6] + fb1[7];
	size_t sumb2 = fb2[0] + fb2[1];

	Buffer c3( b0, 2*sizeof(int), 3*sizeof(int) ); // cut [2-5)
	Buffer c4( b1, 5*sizeof(int), 3*sizeof(int) ); // cut index [5-8)
	Buffer c5( b2, 0, 2*sizeof(int) ); // cut [0-2)

	const int *fc3 = c3.getRead<int>();
	const int *fc4 = c4.getRead<int>();
	const int *fc5 = c5.getRead<int>();

	size_t sumc3 = fc3[0] + fc3[1] + fc3[2];
	size_t sumc4 = fc4[0] + fc4[1] + fc4[2];
	size_t sumc5 = fc5[0] + fc5[1];


	Buffer x = (c3 + (c4 + c5)).linearize();

	const int *fx = x.getRead<int>();

	size_t sumx = 0;
	for( size_t n = 0; n < x.getSize(Buffer::ALLSEGMENTS)/sizeof(int); ++n )
	{
		sumx += fx[n];
	}

	size_t xs = x.getSize(Buffer::ALLSEGMENTS);
	size_t cs = (c3.getSize()+c4.getSize()+c5.getSize());

	return ( xs == cs )
			and (x.getNumberOfSegments() == 1 )
			and (sumx == (sumc3+sumc4+sumc5) )
			and ((sumc3 + sumc4 + sumc5) == (sumb0+sumb1+sumb2));
}

bool testSegmentedOperatorPlus()
{
	Buffer b0 = Allocator::getAllocator()->alloc(100);
	Buffer b1 = Allocator::getAllocator()->alloc(20);

	Buffer c = b0 + b1;

	return (c.getSize(Buffer::ALLSEGMENTS) == b0.getSize()+b1.getSize() and c.getNumberOfSegments() == 2);
}

bool testSegmentedOperatorMinus()
{
	Buffer b0 = Allocator::getAllocator()->alloc(22);
	Buffer b1 = Allocator::getAllocator()->alloc(121);
	Buffer b2 = Allocator::getAllocator()->alloc(556);
	Buffer b3 = Allocator::getAllocator()->alloc(1024);

	Buffer c;

	c += b0+b1+b2+b3;

	Buffer d;
	d += b1;

	Buffer e = c-d;

	c -= b3;

	return  ( c.getNumberOfSegments() == 3 and ( c.getSize(Buffer::ALLSEGMENTS) == (b0.getSize() + b1.getSize() + b2.getSize()) ))
		and ( e.getNumberOfSegments() == 3 and ( e.getSize(Buffer::ALLSEGMENTS) == b0.getSize() + b2.getSize() + b3.getSize() ) );
}

bool testSegmentedOperatorEqualPlus()
{
	Buffer b0 = Allocator::getAllocator()->alloc(22);
	Buffer b1 = Allocator::getAllocator()->alloc(121);

	Buffer c;

	c+=b0;
	c+=b1;


	return (c.getSize(Buffer::ALLSEGMENTS) == (b0.getSize()+b1.getSize()))
			and (c.getNumberOfSegments() == 2);
}

bool testSegmentedOperatorEqualMinus()
{
	Buffer b0 = Allocator::getAllocator()->alloc(22);
	Buffer b1 = Allocator::getAllocator()->alloc(121);

	Buffer c( b0 + b1 );

	c -= b1;

	return ( c.getNumberOfSegments() == 1 ) and (c.getSize(Buffer::ALLSEGMENTS) == b0.getSize());
}

bool testSegmentedOperatorMinusNoMatch()
{
	Buffer b0 = Allocator::getAllocator()->alloc(42);
	Buffer b1 = Allocator::getAllocator()->alloc(142);

	Buffer c = b0;

	Buffer d = c - b1;

	return (d.getNumberOfSegments() == 1) and (d.getSize(Buffer::ALLSEGMENTS) == b0.getSize());
}

bool testSegmentedUnique()
{
	Buffer c;
	{
		Buffer b0 = Allocator::getAllocator()->alloc(42);
		Buffer b1 = Allocator::getAllocator()->alloc(142);
		c += (b0+b1);
	}

	return c.unique(Buffer::ALLSEGMENTS);
}

bool testSegmentedNonUnique()
{
	Buffer b0 = Allocator::getAllocator()->alloc(42);
	Buffer b1 = Allocator::getAllocator()->alloc(142);

	Buffer c(b0);

	return !c.unique(Buffer::ALLSEGMENTS);
}

bool testOneSegmentLinearize()
{
	Buffer b0 = Allocator::getAllocator()->alloc( 256 );

	Buffer c = b0.linearize();

	return (c.getNumberOfSegments() == 1 and c.getImp(0) == b0.getImp(0) and c == b0);
}

bool testCopyToOperation()
{
	BufferWrite b0 = Allocator::getAllocator()->alloc(10);
	BufferWrite b1 = Allocator::getAllocator()->alloc(10);

	ubyte *u0 = b0.getWrite<ubyte>();
	ubyte *u1 = b1.getWrite<ubyte>();

	for( size_t n = 0; n < 10; ++n )
	{
		u0[n] = ubyte(n);
		u1[n] = ubyte(n)+10;
	}

	Buffer c = b0 + b1;

	std::vector<ubyte> mem( c.getSize(Buffer::ALLSEGMENTS) );

	c.copyTo( &mem[0] );

	for( size_t n = 0; n < mem.size(); ++n )
	{
		if( mem[n] != n )
			return false;
	}

	return true;
}

bool testSegmentedToStringOperator()
{
	std::string hello("hello");
	std::string world("world");

	BufferWrite b0 = Allocator::getAllocator()->alloc(hello.size());
	BufferWrite b1 = Allocator::getAllocator()->alloc(world.size());

	std::copy( hello.begin(), hello.end(), b0.writeAccess() );
	std::copy( world.begin(), world.end(), b1.writeAccess() );


	Buffer c = b0 + b1;

	std::string helloWorld = (std::string)c;

	return (helloWorld == "helloworld");
}
// ############################################################################
// ############################################################################


_test tests[] =
{
		  S_TEST(testCreateInvalidBuffer)
		, S_TEST(testSizedAlloc)
		, S_TEST(testSizeCalculation)
		, S_TEST(testGetSizeForInvalidBuffer)
		, S_TEST(testEmptyBuffer)
		, S_TEST(testUnique)
		, S_TEST(testNonUnique)

		, S_TEST(testCreateInvalidBufferWrite)
		, S_TEST(test1ByteBufferNoImp)
		, S_TEST(testBufferToStringOperator)
		, S_TEST(testSubWindow)
		, S_TEST(testNestedSubWindow)
		, S_TEST(testReference)
		, S_TEST(testResize)
		, S_TEST(testExpand)
		, S_TEST(testFailedReserve)
		, S_TEST(testCopyConstructionEq)
		, S_TEST(testCopyConstructionUnEq)
		, S_TEST(testAssignment)
		, S_TEST(testComparisonUnEqEq)
		, S_TEST(testSubWindowTooLarge)
		, S_TEST(testZeroSizeSubWindow)

		, S_TEST(testSegmentCreation)
		, S_TEST(testSegmentedSubWindow)
		, S_TEST(testSegmentedLinearize)
		, S_TEST(testSegmentedSubWindowLinearize)
		, S_TEST(testSegmentedOperatorPlus)
		, S_TEST(testSegmentedOperatorMinus)
		, S_TEST(testSegmentedOperatorEqualPlus)
		, S_TEST(testSegmentedOperatorEqualMinus)
		, S_TEST(testSegmentedOperatorMinusNoMatch)
		, S_TEST(testSegmentedUnique)
		, S_TEST(testSegmentedNonUnique)
		, S_TEST(testOneSegmentLinearize)
		, S_TEST(testCopyToOperation)
		, S_TEST(testSegmentedToStringOperator)
		, S_TEST(test0AllocedBufferValidity)
};

tfunc getFunctionByName( const std::string &strfunction )
{
	std::vector<_test> v( &tests[0], &tests[sizeof(tests)/sizeof(tests[0])] );
	std::vector<_test>::const_iterator it = std::find_if(v.begin(), v.end(), _comp( strfunction ) );

	if(  it == v.end() )
		return NULL;
	return (*it).test;
}

// ############################################################################
// ############################################################################


int main(int argc, char *argv[])
{
	Allocator::setAllocator( new MemoryManager );
	init();


	if( argc != 2 )
		leave(-1); // error, argument not given

	tfunc f = getFunctionByName( argv[1] );
	if( f == NULL )
		leave(-2);

	try
	{
		if( !state( argv[1], f() ) )
			leave(-3);
	}
	catch( std::exception &e )
	{
		std::cerr << e.what() << std::endl;
		leave(-4);
	}
	return 0; // ok, test worked
}

// ############################################################################

