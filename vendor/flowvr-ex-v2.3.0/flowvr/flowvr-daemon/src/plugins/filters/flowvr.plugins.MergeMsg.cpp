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
* File: src/plugins/flowvr.plugins.MergeMsg.cpp                   *
*                                                                 *
* Contacts:                                                       *
*  11/17/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter which merge successive messages in a stream
///
/// <b>Init parameters:</b>
/// -  \<number\>number of messages to merge together\</number\>
/// -  \<stamp\>name of a stamp to combine\</stamp\> <i>(optional)</i>
/// -  \<scratch\>stampname\</scratch\>  <i>(optional)</i>:  name of a
/// stamp with non-zero  value for messages  which can be discarded if
/// not the most recent one.
///
/// <b>Input ports:</b>
/// -  <b>in</b>: Messages to be filtered.
///
/// <b>Output Ports:</b>
/// - <b>out</b>: Filtered messages.

class MergeMsg : public Filter
{
 public:

  MergeMsg(const std::string objID);

  virtual ~MergeMsg();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IN=0,
    NBPORTS
  };

protected:

  std::string stampname;
  StampInfo* stamp;

  std::string scratchname;
  StampInfo* scratch;

  int number;
};

using namespace flowvr::xml;

/// Constructor.
MergeMsg::MergeMsg(const std::string objID)
: Filter(objID), stamp(NULL), scratch(NULL), number(2)
{
}

MergeMsg::~MergeMsg()
{
}

flowvr::plugd::Result MergeMsg::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lstamp = xmlRoot->getElementsByTagName("stamp");
  if (lstamp->getLength()>=1)
  {
    stampname = lstamp->item(0)->getTextContent();
  }
  delete lstamp;

  xml::DOMNodeList* lscratch = xmlRoot->getElementsByTagName("scratch");
  if (lscratch->getLength()>=1)
  {
    scratchname = lscratch->item(0)->getTextContent();
  }
  delete lscratch;

  xml::DOMNodeList* lnumber = xmlRoot->getElementsByTagName("number");
  if (lnumber->getLength()>=1)
  {
    std::string text = lnumber->item(0)->getTextContent();
    number = atoi(text.c_str());
  }
  delete lnumber;

  initInputs(NBPORTS);
  inputs[IDPORT_IN]->setName("in");

  //only one outputmessagequeue for this filter
  initOutputs(1);
  outputs[0]->setName("out");
//  outputs[0]->msgtype=Message::FULL;

  return result;
}

void MergeMsg::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
  int nbmsg = 0;
	int num0 = inputs[IDPORT_IN]->frontNum();
	while (inputs[IDPORT_IN]->get(num0 + nbmsg).valid())
	{
		++nbmsg;
		if (nbmsg == number)
		{ // we have enough messages to merge

			std::vector<Message> msgs;
			Message lastscratch;
			for (int i = 0; i < number; i++)
			{
				Message msg = inputs[IDPORT_IN]->get(num0 + i);

				int val = 0;
				if (scratch != NULL)
					msg.stamps.read(*scratch, val);
				if (val == 0)
				{
					lastscratch.clear();
					msgs.push_back(msg);
				}
				else
					lastscratch = msg;
			}
			if (lastscratch.valid())
				msgs.push_back(lastscratch);

			MessagePut newmsg;
			newmsg.stamps.clone(msgs[0].stamps,
					&inputs[IDPORT_IN]->getStampList());

			if (msgs.size() == 1)
			{ // One message
				newmsg.data = msgs[0].data;
			}
			else
			{
				size_t size = 0;
				for (unsigned int i = 0; i < msgs.size(); i++)
					size += msgs[i].data.getSize();
				if (msgs[0].data.valid())
				{
					BufferWrite newdata = alloc(size);
					size = 0;
					for (unsigned int i = 0; i < msgs.size(); i++)
					{
						memcpy(newdata.getWrite<char> (size),
								msgs[i].data.getRead<char> (0),
								msgs[i].data.getSize());
						size += msgs[i].data.getSize();
					}
					newmsg.data = newdata;
				}
				if (stamp != NULL)
				{
					// add all stamp values
					int val = 0;
					for (unsigned int i = 0; i < msgs.size(); i++)
					{
						int v = 0;
						msgs[i].stamps.read(*stamp, v);
						val += v;
					}
					newmsg.stamps.write(*stamp, val);
				}
				if (scratch != NULL)
				{
					// non discardable messages merged: force stamp scratch to 0
					newmsg.stamps.write(*scratch, 0);
				}
			}
			outputs[0]->put(newmsg, dispatcher);

			num0 += nbmsg;
			nbmsg = 0;
			inputs[IDPORT_IN]->setFront(num0);
		}
	}
}


void MergeMsg::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  {
    if (!stampname.empty())
    {
      stamp = inputs[mqid]->getStampList()[stampname];
      if (stamp == NULL)
	std::cerr << objectID() << ":in : ERROR stamp "<<stampname<<" not found."<<std::endl;
    }
    if (!scratchname.empty())
    {
      scratch = inputs[mqid]->getStampList()[scratchname];
      if (scratch == NULL)
	std::cerr << objectID() << ":in : ERROR stamp "<<scratchname<<" not found."<<std::endl;
    }
    // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif
    
    //give the Stamplist to the outputmessage queue    
    outputs[0]->stamps  = inputs[IDPORT_IN]->getStampList();
    outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
    outputs[0]->newStampSpecification(dispatcher);
  }
}

flowvr::plugd::GenClass<MergeMsg> MergeMsgClass("flowvr.plugins.MergeMsg", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* MergeMsg::getClass() const
{
  return &MergeMsgClass;
}

} // namespace plugins

} // namespace flowvr
