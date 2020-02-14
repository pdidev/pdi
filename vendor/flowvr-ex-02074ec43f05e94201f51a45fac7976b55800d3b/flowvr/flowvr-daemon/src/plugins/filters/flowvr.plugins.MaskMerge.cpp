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
* File: src/plugins/flowvr.plugins.MaskMerge.cpp                  *
*                                                                 *
* Contacts:                                                       *
*  12/02/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter  which merges N messages using  the stamps from the
/// first one, optionally adding the  values of one stamp (the number
/// of elements for  example, or the number of line  of the matrix). A
/// mask stamp dynamically specifies where to read each message.
///
/// This  filter  opens  several  input ports  <b>in0,in1,...</b>  and
/// produces  messages  in an  output  port  <b>out</b>.   When a  new
/// message  is  available  on  all  input  ports  a  new  message  is
/// constructed by  merging all input messages'  data sequentially and
/// taking  the stamps of  the first  message, optionally  adding the
/// values of one  stamp. This message is then  sent to the <b>out</b>
/// port.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of inputs to merge\</nb\>
/// -  \<stamp\>name of the stamp to combine\</stamp\> <i>(optional)</i>
///
/// <b>Input ports:</b>
/// -  <b>mask</b>
/// -  <b>in<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1
///
/// <b>Output Ports:</b>
/// - <b>out</b>

class MaskMerge : public Filter
{
 public:

  MaskMerge(const std::string &objID);

  virtual ~MaskMerge();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  int NBPORTS;

protected:

  std::string stampname;
  StampInfo** stamp;

  int maskSize;
  StampInfo* stampmask;
  int* mask;

  BufferPool poolout;

  //  virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
MaskMerge::MaskMerge(const std::string &objID)
: Filter(objID), NBPORTS(0), stamp(NULL), maskSize(0), stampmask(NULL), mask(NULL)
{
}

MaskMerge::~MaskMerge()
{
  if (stamp!=NULL) delete[] stamp;
}

flowvr::plugd::Result MaskMerge::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  NBPORTS = atoi(nb.c_str());
  delete lnb;

  xml::DOMNodeList* lstamp = xmlRoot->getElementsByTagName("stamp");
  if (lstamp->getLength()>=1)
  {
    stampname = lstamp->item(0)->getTextContent();
    stamp = new StampInfo*[NBPORTS];
  }
  delete lstamp;


  initInputs(1+NBPORTS);
  inputs[0]->setName("mask");

  char buf[16];
  for (int i=0;i<NBPORTS;i++)
  {
    sprintf(buf,"in%d",i);
    inputs[1+i]->setName(buf);
  }

  //only one outputmessagequeue for this filter
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype=Message::FULL;

  return result;
}

void MaskMerge::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid==0)
  {
    stampmask = inputs[0]->getStampList()["mask"];
  }
  else if (stamp != NULL)
  {
    stamp[mqid-1] = inputs[mqid]->getStampList()[stampname];
    if (stamp[mqid-1] == NULL)
      std::cerr << objectID() << ":in"<<mqid-1<<" ERROR stamp "<<stampname<<" not found."<<std::endl;
  }
  if (mqid == 1)
  { // forward specification to out port
    outputs[0]->stamps  = inputs[mqid]->getStampList();
    outputs[0]->newStampSpecification(dispatcher);
  }
}

void MaskMerge::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
  int p;
  while ( inputs[0]->frontMsg().valid() )
  {
    const Message& msg = inputs[0]->frontMsg();

    if (stampmask!=NULL)
    {
      for (int i=0; i < maskSize; i++)
	msg.stamps.read((*stampmask)[i],mask[i]);
    }
    else
      for (int i=0; i < maskSize; i++)
	mask[i]=-1;

    for (p=0; p < NBPORTS; p++)
      if (mask[p>>5]&(1<<(p&31)))
      {
	if (!inputs[1+p]->frontMsg().valid())
	  return; // waiting for this message
      }

    // we have all messages ready
    size_t size = 0;
    int stampval = 0;
    int ndata = 0;
    int first = -1;

    for (p=0;p<NBPORTS;p++)
      if (mask[p>>5]&(1<<(p&31)))
      {
	if (first==-1) first=p;
	const Message& mread = inputs[1+p]->frontMsg();
	if (mread.data.getSize()>0)
	{
	  if (size==0) first=p; // this is the first non-empty message
	  size+=mread.data.getSize();
	  ++ndata;
	}
	if (stamp)
	{
	  int v;
	  if (mread.stamps.read(*(stamp[p]),v))
	    stampval+=v;
	}
      }
    if (first!=-1)
    {
      MessagePut m;
      // Use the first message to get the stamps
      {
	const Message& mread = inputs[1+first]->frontMsg();
	m.stamps.clone(mread.stamps,&inputs[0]->getStampList());
	m.data = mread.data;
      }

      if (ndata>1)
      {
	BufferWrite data = poolout.alloc(getAllocator(),size);
	size_t pos = 0;
	for (p=0;p<NBPORTS;p++)
	  if (mask[p>>5]&(1<<(p&31)))
	  {
	    const Message& mread = inputs[1+p]->frontMsg();
	    memcpy(data.writeAccess()+pos,mread.data.readAccess(),mread.data.getSize());
	    pos+=mread.data.getSize();
	    inputs[1+p]->eraseFront();
	  }
	m.data = data;
      }
      // set stamps and send the message
#ifdef DEBUG
      std::cout<<name()<<": sending message size="<<size;
      if (stamp) std::cout<<" "<<stampname<<"="<<stampval;
      std::cout<<std::endl;
#endif
      if (stamp)
	m.stamps.write((*(stamp[0])),stampval);
      outputs[0]->put(m,dispatcher);
    }
    for (p=0;p<NBPORTS;p++)
      if (mask[p>>5]&(1<<(p&31)))
	inputs[1+p]->eraseFront();
    inputs[0]->eraseFront();
  }
#ifdef DEBUG
  std::cout<<name()<<": waiting messages"<<std::endl;
#endif
}

flowvr::plugd::GenClass<MaskMerge> MaskMergeClass("flowvr.plugins.MaskMerge", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* MaskMerge::getClass() const
{
  return &MaskMergeClass;
}

} // namespace plugins

} // namespace flowvr
