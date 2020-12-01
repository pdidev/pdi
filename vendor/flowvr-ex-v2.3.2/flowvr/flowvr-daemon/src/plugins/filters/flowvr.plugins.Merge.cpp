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
 * File: src/plugins/flowvr.plugins.Merge.cpp                      *
 *                                                                 *
 * Contacts:                                                       *
 *  01/16/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter  which merge N messages using  the stamps from the
/// first one, optionally adding the  values of one stamp (the number
/// of elements for example, or the number of line of the matrix).
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
/// -  <b>in<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1
///
/// <b>Output Ports:</b>
/// - <b>out</b>

class Merge: public Filter
{
public:

	Merge(const std::string &objID);

	virtual ~Merge();

	virtual Class* getClass() const;

	virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot,
			flowvr::plugd::Dispatcher* dispatcher);

	virtual void newMessageNotification(int mqid, int msgnum,
			const Message& msg, Dispatcher* dispatcher);
	virtual void newStampListSpecification(int mqid, const Message& msg,
			Dispatcher* dispatcher);

	int NBPORTS;

protected:

	std::string stampname;
	StampInfo** stamp;

	BufferPool poolout;

	virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
Merge::Merge(const std::string &objID) :
	Filter(objID), NBPORTS(0), stamp(NULL)
{
}

Merge::~Merge()
{
	if (stamp != NULL)
		delete[] stamp;
}

flowvr::plugd::Result Merge::init(flowvr::xml::DOMElement* xmlRoot,
		flowvr::plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
	if (result.error())
		return result;

	xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
	if (lnb->getLength() < 1)
		return Result(flowvr::plugd::Result::ERROR, "No nb parameter");
	std::string nb = lnb->item(0)->getTextContent();
	NBPORTS = atoi(nb.c_str());
	delete lnb;

	xml::DOMNodeList* lstamp = xmlRoot->getElementsByTagName("stamp");
	if (lstamp->getLength() >= 1)
	{
		stampname = lstamp->item(0)->getTextContent();
		if(!stampname.empty()){
			stamp = new StampInfo*[NBPORTS];
			for (int i = 0; i < NBPORTS; i++)
				stamp[i] = NULL;
		}
		else
			std::cout<<"No stamp merge require"<<std::endl;
	}
	delete lstamp;

	initInputs(NBPORTS);

	char buf[16];
	for (int i = 0; i < NBPORTS; i++)
	{
		sprintf(buf, "in%d", i);
		inputs[i]->setName(buf);
		//if (i==0) inputs[i]->storeSpecification();
	}

	//only one outputmessagequeue for this filter
	initOutputs(1);
	outputs[0]->setName("out");
	outputs[0]->msgtype = Message::FULL;

	return result;
}

void Merge::newMessageNotification(int mqid, int msgnum, const Message& msg,
		Dispatcher* dispatcher)
{
#ifdef DEBUG
	std::cout << name()<<": new input"<<mqid<<" "<<msgnum<<std::endl;
#endif
	sendPendingMessages(dispatcher);
}

void Merge::newStampListSpecification(int mqid, const Message& msg,
		Dispatcher* dispatcher)
{
	if (stamp != NULL)
	{
		stamp[mqid] = inputs[mqid]->getStampList()[stampname];
		if (stamp[mqid] == NULL)
			std::cerr << objectID() << ":in" << mqid << " ERROR stamp "
					<< stampname << " not found." << std::endl;
#ifdef DEBUG
		else
		std::cout << objectID() << ":in"<<mqid<<": stamp "<<stampname<<" @ "<<stamp[mqid]->getOffset()<<std::endl;
#endif
	}
	if (mqid == 0)
	{ // forward specification to out port
#ifdef DEBUG
		std::cout << name()<<": forwarding stamps specification"<<std::endl;
#endif

		//give the Stamplist to the outputmessage queue
		if(stamp == NULL)
			outputs[0]->stamps = inputs[0]->getStampList();
		else {
			flowvr::StampList *stampList = new StampList();
			//outputs[0]->stamps = StampList();

			//The first 6 stamps are default and already generated by the constructor. Go directly to user's stamps
			for(int i = 6; i < inputs[0]->getStampList().nbStamp(); i++){
				//std::cout<<"Traitement du stamp "<<(inputs[0]->getStampList())[i]->getName()<<"...";
				if((inputs[0]->getStampList())[i]->getName() != stampname){
					//std::cout<<"Copie simple...";
					stampList->add((inputs[0]->getStampList())[i]);
					//outputs[0]->stamps.add((inputs[0]->getStampList())[i]);
				}
				else {
					//std::cout<<"Creation d'un nouveau stamp...";
					flowvr::StampInfo* inStampInfo = (inputs[0]->getStampList())[i];

					int nbStamp = inStampInfo->getSize() / sizeof(int);
					flowvr::StampInfo* outStampInfo = new StampInfo( stampname, flowvr::TypeArray::create(nbStamp * NBPORTS, flowvr::TypeInt::create()));
					stampList->add(outStampInfo);
					//outputs[0]->stamps.add(outStampInfo);
				}
				//std::cout<<"Stamp traite."<<std::endl;
					
			}
			outputs[0]->stamps = *stampList;
		}
	
		//outputs[0]->stamps = inputs[0]->getStampList();
		outputs[0]->newStampSpecification(dispatcher);
		//std::cout<<"Fin de l'initialisation des stamps."<<std::endl;

	}
	sendPendingMessages(dispatcher);
}

void Merge::sendPendingMessages(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
	int p;
	if (!inputs[0]->stampsReceived())
	{
		if (flowvr::daemon::verboseLevel >= 1)
			std::cout << objectID() << " waiting for stamps specification"
					<< std::endl;
		return; // still waiting for stamps specification
	}
	if (stamp)
	{
		for (p = 0; p < NBPORTS; p++)
			if (stamp[p] == NULL)
				break;
		if (p < NBPORTS)
		{
			if (flowvr::daemon::verboseLevel >= 1)
				std::cout << objectID() << " waiting for good stamps"
						<< std::endl;
			return; // bad stamps
		}
	}
	for (;;)
	{
		for (p = 0; p < NBPORTS; p++)
			if (!inputs[p]->frontMsg().valid())
				break; // next message not ready on port p

		if (p < NBPORTS)
			break;
		// we have all messages ready

		size_t size = 0;
		int stampval = 0;
		int ndata = 0;
		int first = 0;
		for (p = 0; p < NBPORTS; p++)
		{
			const Message& mread = inputs[p]->frontMsg();

			if (mread.data.getSize() > 0)
			{
				++ndata;
				if (size == 0)
					first = p; // this is the first non-empty message
			}
			size += mread.data.getSize();
			if (stamp)
			{
				int v;
				if (mread.stamps.read(*(stamp[p]), v))
					stampval += v;
			}
		}

		MessagePut m;
		std::vector<int> stampData; 

		if (ndata <= 1)
		{ // no merge required
			m.data = inputs[first]->frontMsg().data;
		}
		else
		{
			BufferWrite data = poolout.alloc(getAllocator(), size);
			size_t pos = 0;
			for (p = 0; p < NBPORTS; p++)
			{
				const Message& mread = inputs[p]->frontMsg();
				memcpy(data.writeAccess() + pos, mread.data.readAccess(), mread.data.getSize());
				pos += mread.data.getSize();

				if(stamp){
					flowvr::StampInfo *   pStampID = (stamp[p]);
					int nbStamps = pStampID->getSize() / sizeof(int);
	
					int d;
					for(int i = 0; i < nbStamps; i++){
						if (! mread.stamps.read((*pStampID)[i],d))
	  						std::cout<<"Erreur de lecture du stamp."<<std::endl;
						else
							stampData.push_back(d);
					}
				}
                                /*std::cout<<"Stamp du message "<<p<<" : [";
                                for(int i = 0; i < nbStamps; i++)
                                        std::cout<<stampData[stampData.size() - nbStamps + i] <<",";
                                std::cout<<"]"<<std::endl;*/
			}
			m.data = data;
		}
		// set stamps and send the message
#ifdef DEBUG
		std::cout<<name()<<": sending message size="<<size;
		if (stamp) std::cout<<" "<<stampname<<"="<<stampval;
		std::cout<<std::endl;
#endif
		//std::cout<<"Verification des stamps."<<std::endl;
		//m.stamps.clone(inputs[0]->frontMsg().stamps, &inputs[0]->getStampList()); // get stamps from first message

		if (stamp){
			
			flowvr::StampInfo *StampID = outputs[0]->stamps[stampname];
			//if(StampID) std::cout<<"Le stamp est non null."<<std::endl;
			//std::cout<<"Le custum stamp est alloue pour "<<StampID->getSize()<<std::endl;
			for(unsigned int i = 0; i < stampData.size(); i++){
				if(! m.stamps.write((*StampID)[i],stampData[i]))
					std::cerr<<"Error while writing custum stamp ("<<i<<")"<<std::endl;
			}
		}
		else {
			m.stamps.clone(inputs[0]->frontMsg().stamps, &inputs[0]->getStampList()); // get stamps from first message
		}
		outputs[0]->put(m, dispatcher);

		for (p = 0; p < NBPORTS; p++)
			inputs[p]->eraseFront();

	}
#ifdef DEBUG
	std::cout<<name()<<": waiting messages"<<std::endl;
#endif
}

flowvr::plugd::GenClass<Merge> MergeClass("flowvr.plugins.Merge", // name
		"", // description
		&flowvr::plugins::FilterClass);

Class* Merge::getClass() const
{
	return &MergeClass;
}

} // namespace plugins

} // namespace flowvr
