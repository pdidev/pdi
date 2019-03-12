/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
* File: src/plugins/flowvr.plugins.Merge2D.cpp                    *
*                                                                 *
* Contacts:                                                       *
*  01/12/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/filter.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <iostream>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// \brief  A filter  which  merge  2D grids  using  X&Y intervals  in
/// stamps.
///
/// This  filter opens  two  input ports  <b>in0,in1</b> and  produces
/// messages in an output port <b>out</b>.
///
/// Each input should receive messages containing 2D grids defined  by
/// 2 stamps:
/// - <b>P</b>: an array of at least 2 values specifying the minimum X,Y coordinate of the grid
/// - <b>N</b>: an array of at least 2 values specifying the size of the grid in X,Y
///
/// When a new  message is available on all input  ports a new message
/// is constructed by  merging the two input grids  depending of their
/// respective coordinates. Theys must  be side-by-side and their size
/// must be compatible (i.e. the  merge of the two grid should produce
/// a rectangular grid).  This message is then sent  to the <b>out</b>
/// port.
///
/// <b>Init parameters:</b> none.
///
/// <b>Input ports:</b>
/// -  <b>in0</b>
/// -  <b>in1</b>
///
/// <b>Output Ports:</b>
/// - <b>out</b>

class Merge2D : public Filter
{
 public:

  Merge2D(const std::string &objID);

  virtual ~Merge2D();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IN0=0,
    IDPORT_IN1,
    NBPORTS
  };

protected:

  StampInfo* stampP0;
  StampInfo* stampP1;

  StampInfo* stampN0;
  StampInfo* stampN1;

  BufferPool poolout;

  virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
Merge2D::Merge2D(const std::string &objID)
  : Filter(objID), stampP0(NULL), stampP1(NULL), stampN0(NULL), stampN1(NULL)
{
}

Merge2D::~Merge2D()
{
}

flowvr::plugd::Result Merge2D::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  initInputs(NBPORTS);
  inputs[IDPORT_IN0]->setName("in0");
  //inputs[IDPORT_IN0]->storeSpecification();
  inputs[IDPORT_IN1]->setName("in1");
  // Note: both ports should have the same stamps specification

  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::FULL;

  return result;
}

void Merge2D::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << objectID()<<": new input"<<mqid<<" "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  sendPendingMessages(dispatcher);
}

void Merge2D::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN0)
  {
    stampP0 = inputs[IDPORT_IN0]->getStampList()["P"];
    stampN0 = inputs[IDPORT_IN0]->getStampList()["N"];
    if (stampP0 == NULL)
      std::cerr << objectID() << ":in0 ERROR stamp P not found."<<std::endl;
#ifdef DEBUG
    else
      std::cout << objectID() << ":in0: stamp P @ "<<stampP0->getOffset()<<std::endl;
#endif
    if (stampN0 == NULL)
      std::cerr << objectID() << ":in0 ERROR stamp N not found."<<std::endl;
#ifdef DEBUG
    else
      std::cout << objectID() << ":in0: stamp N @ "<<stampN0->getOffset()<<std::endl;
#endif
  }
  else if (mqid == IDPORT_IN1)
  {
    stampP1 = inputs[IDPORT_IN1]->getStampList()["P"];
    stampN1 = inputs[IDPORT_IN1]->getStampList()["N"];
    if (stampP1 == NULL)
      std::cerr << objectID() << ":in1 ERROR stamp P not found."<<std::endl;
#ifdef DEBUG
    else
      std::cout << objectID() << ":in1: stamp P @ "<<stampP1->getOffset()<<std::endl;
#endif
    if (stampN1 == NULL)
      std::cerr << objectID() << ":in1 ERROR stamp N not found."<<std::endl;
#ifdef DEBUG
    else
      std::cout << objectID() << ":in1: stamp N @ "<<stampN1->getOffset()<<std::endl;
#endif
  }

  if (mqid == IDPORT_IN0)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif
    
    outputs[0]->stamps = inputs[IDPORT_IN0]->getStampList();
    outputs[0]->newStampSpecification(dispatcher);
  }
  sendPendingMessages(dispatcher);
}

void Merge2D::sendPendingMessages(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION

  if (!inputs[IDPORT_IN0]->stampsReceived())
  {
    if (flowvr::daemon::verboseLevel>=1)
      std::cout << objectID() << " waiting for stamps specification"<<std::endl;
    return; // still waiting for stamps specification
  }
  if (stampP0 == NULL || stampP1 == NULL || stampN0 == NULL || stampN1 == NULL)
  {
    if (flowvr::daemon::verboseLevel>=1)
      std::cout << objectID() << " waiting for good stamps"<<std::endl;
    return; // bad stamps
  }
  for(;;)
  {

    int p0x=0,p0y=0,n0x=0,n0y=0;
    int p1x=0,p1y=0,n1x=0,n1y=0;
    Message m0,m1;
    {
      if (!(inputs[IDPORT_IN0]->frontMsg().valid() && inputs[IDPORT_IN1]->frontMsg().valid()))
	break;
      // we have both messages ready
      m0 = inputs[IDPORT_IN0]->frontMsg();
      inputs[IDPORT_IN0]->eraseFront();
      m1 = inputs[IDPORT_IN1]->frontMsg();
      inputs[IDPORT_IN1]->eraseFront();
      if (!m0.data.getSize()) continue;
      if (!m1.data.getSize()) continue;

      if (!m0.stamps.read((*stampP0)[0],p0x)) continue;
      if (!m0.stamps.read((*stampP0)[1],p0y)) continue;
      if (!m0.stamps.read((*stampN0)[0],n0x)) continue;
      if (!m0.stamps.read((*stampN0)[1],n0y)) continue;

      if (!m1.stamps.read((*stampP1)[0],p1x)) continue;
      if (!m1.stamps.read((*stampP1)[1],p1y)) continue;
      if (!m1.stamps.read((*stampN1)[0],n1x)) continue;
      if (!m1.stamps.read((*stampN1)[1],n1y)) continue;
    }

    // Here is the logic of this filter

    MessageWrite m;

    if (p0x==p1x && n0x==n1x)
    {
      if (p1y==p0y+n0y)
      { // m0 is on top of m1
#ifdef DEBUG
	std::cout<<objectID()<<": merging messages:m0 is on top of m1"<<std::endl;
#endif
	m.data = poolout.alloc(getAllocator(),m0.data.getSize()+m1.data.getSize());
	if (m.data.writeAccess()!=NULL)
	{
	  memcpy(m.data.writeAccess(),m0.data.readAccess(),m0.data.getSize());
	  memcpy(m.data.writeAccess()+m0.data.getSize(),m1.data.readAccess(),m1.data.getSize());
	}
	n0y+=n1y; // add m1 height
      }
      else if (p0y==p1y+n1y)
      { // m1 is on the bottom of m0
#ifdef DEBUG
	std::cout<<objectID()<<": merging messages:m0 is on the bottom of m1"<<std::endl;
#endif
	m.data = poolout.alloc(getAllocator(),m0.data.getSize()+m1.data.getSize());
	if (m.data.writeAccess()!=NULL)
	{
	  memcpy(m.data.writeAccess(),m1.data.readAccess(),m1.data.getSize());
	  memcpy(m.data.writeAccess()+m1.data.getSize(),m0.data.readAccess(),m0.data.getSize());
	}
	p0y=p1y; // origin is p1
	n0y+=n1y; // add m1 height
      }
      else
      { // bad configuration
	std::cerr << objectID() << ": ERROR invalid messages= <"
		  <<p0x<<","<<p0y<<">+<"<<n0x<<","<<n0y<<"> <"
		  <<p1x<<","<<p1y<<">+<"<<n1x<<","<<n1y<<">"<<std::endl;
	continue;
      }
    }
    else if (p0y==p1y && n0y==n1y)
    {
      int cellsize0 = m0.data.getSize()/(n0x*n0y);
      int cellsize1 = m1.data.getSize()/(n1x*n1y);
      if (m0.data.getSize()%(n0y*n0x) || m1.data.getSize()%(n1y*n1x) || cellsize0 != cellsize1)
      {
	std::cerr << objectID() << ": ERROR invalid messages= <"
		  <<p0x<<","<<p0y<<">+<"<<n0x<<","<<n0y<<"> <"
		  <<p1x<<","<<p1y<<">+<"<<n1x<<","<<n1y<<"> sizes= "
		  <<m0.data.getSize()<<" "<<m1.data.getSize()<<std::endl;
	continue;
      }
      if (p1x==p0x+n0x)
      { // m0 is left of m1
#ifdef DEBUG
	std::cout<<objectID()<<": merging messages:m0 is left of m1"<<std::endl;
#endif
	int y,d0,d1;
	const unsigned char* src0;
	const unsigned char* src1;
	unsigned char* dest;
	m.data = poolout.alloc(getAllocator(),m0.data.getSize()+m1.data.getSize());
	if (m.data.writeAccess()!=NULL && m.data.getSize()>0)
	{
	  dest = m.data.writeAccess();
	  src0 = m0.data.readAccess(); d0 = n0x*cellsize0;
	  src1 = m1.data.readAccess(); d1 = n1x*cellsize1;
	  if (d0&3 || d1&3)
	  {
	    // unaligned version -> memcpy
	    for (y=0;y<n0y;y++)
	    {
	      memcpy(dest,src0,d0); dest+=d0; src0+=d0;
	      memcpy(dest,src1,d1); dest+=d1; src1+=d1;
	    }
	  }
	  else
	  {
	    unsigned int* idest = (unsigned int*) dest;
	    const unsigned int* isrc0 = (const unsigned int*) src0;
	    const unsigned int* isrc1 = (const unsigned int*) src1;
	    const unsigned int* iend;
	    d0>>=2;
	    d1>>=2;
	    for (y=n0y;y;--y)
	    {
	      iend = isrc0+d0;
	      do
		*(idest++) = *isrc0;
	      while (++isrc0<iend);
	      iend = isrc1+d1;
	      do
		*(idest++) = *isrc1;
	      while (++isrc1<iend);
	    }
	  }
	}
	n0x+=n1x; // add m1 width
      }
      else if (p0x==p1x+n1x)
      { // m0 is right of m1
#ifdef DEBUG
	std::cout<<objectID()<<": merging messages:m0 is right of m1"<<std::endl;
#endif
	int y,d0,d1;
	const unsigned char* src0;
	const unsigned char* src1;
	unsigned char* dest;
	m.data = poolout.alloc(getAllocator(),m0.data.getSize()+m1.data.getSize());
	if (m.data.writeAccess()!=NULL && m.data.getSize()>0)
	{
	  dest = m.data.writeAccess();
	  src0 = m0.data.readAccess(); d0 = n0x*cellsize0;
	  src1 = m1.data.readAccess(); d1 = n1x*cellsize1;
	  if (d0&3 || d1&3)
	  {
	    // unaligned version -> memcpy
	    for (y=0;y<n0y;y++)
	    {
	      memcpy(dest,src1,d1); dest+=d1; src1+=d1;
	      memcpy(dest,src0,d0); dest+=d0; src0+=d0;
	    }
	  }
	  else
	  {
	    unsigned int* idest = (unsigned int*) dest;
	    const unsigned int* isrc0 = (const unsigned int*) src0;
	    const unsigned int* isrc1 = (const unsigned int*) src1;
	    const unsigned int* iend;
	    d0>>=2;
	    d1>>=2;
	    for (y=n0y;y;--y)
	    {
	      iend = isrc1+d1;
	      do
		*(idest++) = *isrc1;
	      while (++isrc1<iend);
	      iend = isrc0+d0;
	      do
		*(idest++) = *isrc0;
	      while (++isrc0<iend);
	    }
	  }
	}
	p0x=p1x; // origin is p1
	n0x+=n1x; // add m1 width
      }
      else
      { // bad configuration
	std::cerr << objectID() << ": ERROR invalid messages= <"
		  <<p0x<<","<<p0y<<">+<"<<n0x<<","<<n0y<<"> <"
		  <<p1x<<","<<p1y<<">+<"<<n1x<<","<<n1y<<">"<<std::endl;
	continue;
      }
    }
    // set stamps and send the message
    m.stamps.clone(m0.stamps,&inputs[IDPORT_IN0]->getStampList());
    m.stamps.write((*stampP0)[0],p0x);
    m.stamps.write((*stampP0)[1],p0y);
    m.stamps.write((*stampN0)[0],n0x);
    m.stamps.write((*stampN0)[1],n0y);
    outputs[0]->put(m, dispatcher);
#ifdef DEBUG
    std::cout<<objectID()<<": sending message"<<std::endl;
#endif
  }
#ifdef DEBUG
  std::cout<<objectID()<<": waiting message "<<inputs[IDPORT_IN0]->frontNum()<<std::endl;
#endif
}

flowvr::plugd::GenClass<Merge2D> Merge2DClass("flowvr.plugins.Merge2D", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* Merge2D::getClass() const
{
  return &Merge2DClass;
}

} // namespace plugins

} // namespace flowvr
