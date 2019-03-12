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
* File: include/flowvr/daemondata.h                               *
*                                                                 *
* Contacts:                                                       *
*  05/24/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_TRACE_H
#define FLOWVR_TRACE_H

#include "flowvr/buffer.h"
#include "flowvr/daemondata.h"
#include "flowvr/xml.h"
#include <algorithm>
#include <sys/time.h>
/*#if defined(__ia64__) && (defined(__EDG_VERSION) || defined(__ECC))
#include <ia64intrin.h>
#endif
*/
namespace flowvr
{

/// Trace base class
class Trace
{
 public:

  Trace(const std::string &myname);

  virtual ~Trace();

  /// Name accessor.
  void setName(const std::string& n) { name=n; }

  /// Name accessor.
  const std::string& getName() const { return name; }

  /// Return the XML specification of this trace
  virtual xml::DOMElement* xmlDesc() const;

  /// Activate logging of this trace
  virtual bool start(int newId, BufferWrite newBuf, xml::DOMElement* parameters);

  /// Stop logging this trace
  virtual bool stop(xml::DOMElement* parameters);

  typedef int id_t;
  typedef timeval cycle_t; 

  static void readCycle(cycle_t* t)
  {
      gettimeofday(t, NULL);
      /*
#if defined(__ia64__)
# if defined(__EDG_VERSION) || defined(__ECC)
    t = __getReg(_IA64_REG_AR_ITC);
# else
    __asm__ __volatile__("mov %0=ar.itc" : "=r"(t) :: "memory");
#endif
#elif defined(__powerpc__) || defined(__POWERPC__)
    register unsigned long t_u;
	register unsigned long t_l;
	asm volatile ("mftbu %0" : "=r" (t_u) );
	asm volatile ("mftb %0" : "=r" (t_l) );
	t = (((cycle_t)t_u)<<32UL) | t_l;
#else
    __asm__ __volatile__ ("rdtsc" : "=A" (t));
#endif
*/
  }

 protected:
  std::string name;
  BufferWrite log;

  id_t id;

  enum { HeaderSize = sizeof(unsigned int) + sizeof(cycle_t) + sizeof(id_t) };

  bool writeBegin(BufferWrite& buf, cycle_t* t, unsigned int datasize = 0)
  {
    if (!log.valid()) return false;
	readCycle(t);
    size_t size = (HeaderSize+datasize+3)&-4;
    //t = (t << 8) | ((size >> 2)&0xff);
    buf = log.getWrite<MPLogInfo>(0)->writeData(size);
    return buf.valid();
  }

  void writeEnd(BufferWrite& buf, cycle_t& t, unsigned int datasize=0)
  {
    size_t size = (HeaderSize+datasize+3)&-4;
    *buf.getWrite<unsigned int>(0) = size;
    *buf.getWrite<id_t>(sizeof(cycle_t) + sizeof(unsigned int)) = id;
    *buf.getWrite<cycle_t>(sizeof(unsigned int)) = t;
  }

};

class EventTrace : public Trace
{
 public:
 EventTrace(const std::string &myname): Trace(myname)
 {
 }

  void write()
  {
    BufferWrite buf;
    cycle_t t;
    if (!writeBegin(buf,&t)) return;
    writeEnd(buf,t);
  }
};

template <class T>
class TypedTrace : public Trace
{
 public:
  typedef T data_t;

  TypedTrace(const std::string &myname): Trace(myname)
  {
  }

  void write(const data_t& data)
  {
    BufferWrite buf;
    cycle_t t;
    if (!writeBegin(buf,&t,dataSize(data))) return;
    writeData(buf,data);
    writeEnd(buf,t, dataSize(data));
  }

  xml::DOMElement* xmlDesc() const
  {
    xml::DOMElement* root = Trace::xmlDesc();
    xml::DOMElement* data = new xml::DOMElement("data");
    data->LinkEndChild(xmlTypeDesc());
    root->LinkEndChild(data);
    return root;
  }

 protected:

  int dataSize(const data_t& data) const
  {
    return sizeof(data_t);
  }

  void writeData(BufferWrite& buf, const data_t& data) const
  {
    //*(buf.getWrite<data_t>(HeaderSize)) = data;
    *(data_t*)(buf.writeAccess()+HeaderSize) = data;
  }

  xml::DOMElement* xmlTypeDesc() const
  {
    xml::DOMElement* elem = new xml::DOMElement("binary");
    elem->SetAttribute("size",sizeof(T));
    return elem;
  }

};

// std::string

template <>
inline int TypedTrace<std::string>::dataSize(const std::string& data) const
{
  return data.length();
}
template <>
inline void TypedTrace<std::string>::writeData(BufferWrite& buf, const std::string& data) const
{
  int s = data.length()+1;
  if (buf.getSize()-HeaderSize < s) s = buf.getSize()-HeaderSize;
  memcpy(buf.getWrite<char>(HeaderSize),data.c_str(),s);
}

template <>
inline xml::DOMElement* TypedTrace<std::string>::xmlTypeDesc() const
{
  return new xml::DOMElement("string");
}

// int

template <>
inline xml::DOMElement* TypedTrace<int>::xmlTypeDesc() const
{
  return new xml::DOMElement("int");
}

// float

template <>
inline xml::DOMElement* TypedTrace<float>::xmlTypeDesc() const
{
  return new xml::DOMElement("float");
}

} // namespace flowvr

#endif
