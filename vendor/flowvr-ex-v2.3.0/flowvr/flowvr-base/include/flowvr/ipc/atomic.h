/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*----------------------------------------------------------------*
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
* File: include/flowvr/ipc/atomic.h                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_ATOMIC_H
#define FLOWVR_IPC_ATOMIC_H

#if defined(FLOWVR_USE_KERNEL_ATOMIC)

#include <asm/atomic.h>
#include <bits/atomicity.h>

namespace flowvr
{
	namespace ipc
	{
        inline void memory_barrier() { barrier(); }
    
		class MPAtomicInt
		{
		  atomic_t val;
		 public:
		  operator int() const { return atomic_read(&val); }
		  void set(int i) { atomic_set(&val,i); }
		  void add(int i) { atomic_add(i,&val); }
		  void sub(int i) { atomic_sub(i,&val); }
		  void inc() { atomic_inc(&val); }
		  void dec() { atomic_dec(&val); }
		  bool dec_and_test_null() { return atomic_dec_and_test(&val); }
		  bool add_and_test_neg(int i) { return atomic_add_negative(i,&val); }
		  void operator=(int i) { set(i); }
		  void operator+=(int i) { add(i); }
		  void operator-=(int i) { sub(i); }
		  void operator++() { inc(); }
		  void operator++(int) { inc(); }
		  void operator--() { dec(); }
		  void operator--(int) { dec(); }

		  int exchange_and_add(int i) { return __exchange_and_add((_Atomic_word*)&val,i); }
		  int exchange_and_add(int i) { return __exchange_and_add((_Atomic_word*)&val,i); }

		  static const char* getImplName() { return "Kernel (32bit)"; }
		};

	/// @todo find the co-responding 64bit implementation here...
	  typedef MTAtomicInt MTAtomicIntBw;

	} // namespace ipc

} // namespace flowvr

#elif defined(i386) || defined(__i386__) || defined(__x86_64__) || defined(__LP64__)

namespace flowvr
{
	namespace ipc
	{
		inline void memory_barrier() { __asm__ __volatile__ ("":::"memory"); }

		class MPAtomicInt
		{
		  volatile int val;
		 public:
		  operator int() const { return val; }
		  void set(int i) { val = i; }
		  void add(int i)
		  {
			__asm__ __volatile__ ("lock; add{l} {%1,%0|%0,%1}"
					  : "+m" (val) : "ir" (i) : "memory");
		  }
		  void sub(int i)
		  {
			__asm__ __volatile__ ("lock; sub{l} {%1,%0|%0,%1}"
					  : "+m" (val) : "ir" (i) : "memory");
		  }
		  void inc()
		  {
			__asm__ __volatile__ ("lock; inc{l} %0"
					  : "+m" (val) : : "memory");
		  }
		  void dec()
		  {
			__asm__ __volatile__ ("lock; dec{l} %0"
					  : "+m" (val) : : "memory");
		  }
		  bool dec_and_test_null()
		  {
			unsigned char c;
			__asm__ __volatile__("lock; dec{l} %0; sete %1"
					 :"+m" (val), "=qm" (c)
					 : : "memory");
			return c != 0;
		  }
		  bool add_and_test_neg(int i)
		  {
			unsigned char c;
			__asm__ __volatile__("lock; add{l} {%2,%0|%0,%2}; sets %1"
					 :"+m" (val), "=qm" (c)
					 :"ir" (i) : "memory");
			return c != 0;
		  }

		  int exchange_and_add(int i)
		  {
			int res;
			__asm__ __volatile__ ("lock; xadd{l} {%0,%1|%1,%0}"
					: "=r" (res), "+m" (val)
								: "0" (i)
								: "memory");
			return res;
		  }
          
          /// \return 0 if the comparison failed, non-zero otherwise
          bool compare_and_swap( int o, int n )
         {
            char res;
            __asm__ __volatile__ ("lock; cmpxchg{l} %2,%0; setz %1"
                    : "+m" (val), "=q" (res)
                    :  "r" (n),    "a" (o)
                    : "memory");
           return res;
         }

		  void operator=(int i) { set(i); }
		  void operator+=(int i) { add(i); }
		  void operator-=(int i) { sub(i); }
		  void operator++() { inc(); }
		  void operator++(int) { inc(); }
		  void operator--() { dec(); }
		  void operator--(int) { dec(); }


		#if defined(__x86_64__) || defined(__LP64__)
		  static const char* getImplName() { return "x86_64 (32bit-int)"; }
		#else
		  static const char* getImplName() { return "i386 (32bit-int)"; }
		#endif
		};


#if defined(__x86_64__) || defined(__LP64__)
	class MPAtomicIntBw
	{
	  volatile long val;
	 public:
	  operator long() const { return val; }
	  void set( long i) { val = i; }
	  void add( long i)
	  {
		__asm__ __volatile__ ("lock; add{q} {%1,%0|%0,%1}"
				  : "+m" (val) : "ir" (i) : "memory");
	  }
	  void sub( long i)
	  {
		__asm__ __volatile__ ("lock; sub{q} {%1,%0|%0,%1}"
				  : "+m" (val) : "ir" (i) : "memory");
	  }
	  void inc()
	  {
		__asm__ __volatile__ ("lock; inc{q} %0"
				  : "+m" (val) : : "memory");
	  }
	  void dec()
	  {
		__asm__ __volatile__ ("lock; dec{q} %0"
				  : "+m" (val) : : "memory");
	  }
	  bool dec_and_test_null()
	  {
		unsigned char c;
		__asm__ __volatile__("lock; dec{q} %0; sete %1"
				 :"+m" (val), "=qm" (c)
				 : : "memory");
		return c != 0;
	  }
	  bool add_and_test_neg( long i)
	  {
		unsigned char c;
		__asm__ __volatile__("lock; add{q} {%2,%0|%0,%2}; sets %1"
				 :"+m" (val), "=qm" (c)
				 :"ir" (i) : "memory");
		return c != 0;
	  }

	   long exchange_and_add( long i)
	  {
		 long res;
		__asm__ __volatile__ ("lock; xadd{q} {%0,%1|%1,%0}"
				: "=r" (res), "+m" (val)
							: "0" (i)
							: "memory");
		return res;
	  }
       
       /// \return 0 if the comparison failed, non-zero otherwise
       bool compare_and_swap( long o, long n )
      {
         char res;
         __asm__ __volatile__ ("lock; cmpxchg{q} %2,%0; setz %1"
                 : "+m" (val), "=q" (res)
                 :  "r" (n),    "a" (o)
                 : "memory");
        return res;
      }

	  void operator=( long i) { set(i); }
	  void operator+=( long i) { add(i); }
	  void operator-=( long i) { sub(i); }
	  void operator++() { inc(); }
	  void operator++(int) { inc(); }
	  void operator--() { dec(); }
	  void operator--(int) { dec(); }


	  static const char* getImplName() { return "x86_64 (64bit-long)"; }
	};

#else
  typedef MPAtomicInt MPAtomicIntBw;
#endif // __x86_64__

} // namespace ipc

} // namespace flowvr

#else
// Fall-back mode: stdc++ atomic operations (should be available on all gcc-supported platforms)
#include <ext/atomicity.h>

using namespace  __gnu_cxx;

namespace flowvr
{
	namespace ipc
	{
        inline void memory_barrier() { __sync_synchronize(); }
        
		/// Small class used for multi-process reference counting
		class MPAtomicInt
		{
		  volatile _Atomic_word val;
		 public:
		  operator int() const
		  {
			// this is the correct implementation
			//   return __exchange_and_add(&val,0);
			// but this is faster and should also work on x86 and ppc
			return val;
		  }
		  void set(int i)
		  {
			  // there seems to be no atomic set on the level of gcc
			  val = i;
		  }

		  void add(int i) { __atomic_add(&val,i); }
		  void sub(int i) { __atomic_add(&val,-i); }
		  void inc() {__atomic_add(&val,1); }
		  void dec() { __atomic_add(&val,-1); }
		  //bool sub_and_test_null(int i) { return __exchange_and_add(&val,-i)==i; }
		  bool dec_and_test_null() { return __exchange_and_add(&val,-1)==1; }
		  //bool inc_and_test_null() { return __exchange_and_add(&val,1)==-1; }
		  bool add_and_test_neg(int i) { return __exchange_and_add(&val,-i) < i; }
		  void operator=(int i) { set(i); }
		  void operator+=(int i) { add(i); }
		  void operator-=(int i) { sub(i); }
		  void operator++() { inc(); }
		  void operator++(int) { inc(); }
		  void operator--() { dec(); }
		  void operator--(int) { dec(); }

		  int exchange_and_add(int i) { return __exchange_and_add(&val,i); }
		  bool compare_and_swap(int o, int n) { return __sync_bool_compare_and_swap(&val,o,n); }

		  static const char* getImplName() { return "GLIBC (32bit-int)"; }
		};

		/// @todo find the co-responding 64bit implementation here...
		typedef MPAtomicInt MPAtomicIntBw;
	} // namespace ipc

} // namespace flowvr

#endif //ifdef FLOWVR_USE_KERNEL_ATOMIC

namespace flowvr
{
	namespace ipc
	{
		class MTAtomicInt  : public MPAtomicInt
		{
		 public:
				 MTAtomicInt() {}
				 MTAtomicInt(int i) { set(i); }
				 MTAtomicInt(const MTAtomicInt& i) { set((int)i); }
				 void operator=(const MTAtomicInt& i) { set((int)i); }
		};

		#if defined( __x86_64__ ) || defined(__LP64__)
		class MTAtomicIntBw : public MPAtomicIntBw
		{
		public:
			MTAtomicIntBw() {}
			MTAtomicIntBw( long i) { set(i); }
			MTAtomicIntBw(const MTAtomicIntBw& i) { set(( long)i); }
			void operator=(const MTAtomicIntBw& i) { set(( long)i); }
		};
	#else // _x86_64_ || __LP64__
		typedef MTAtomicInt MTAtomicIntBw;
	#endif

	} // namespace ipc

} // namespace flowvr

#endif
