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
 * File: src/plugins/flowvr.plugins.FilterIncrement.cpp            *
 *    						                  *
 * Contacts:                                                       *
 *  25/06/2008 khalid Zaida <khalid.zaida@gmail.com>               *
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

        /// \brief Puts an empty message on 'out', then transmits all messages from 'in' to 'out', increasing the num and it fields by 1.
        ///
        /// <b>Init parameters:</b> none.
        ///
        /// <b>Input ports:</b>
        /// -  <b>in</b>:  Messages to be increased by one.
        ///
        /// <b>Output Ports:</b>
        /// - <b>out</b>: increased by one message.

        class FilterIncrement : public Filter
        {
            public:

                FilterIncrement(const std::string &objID);

                virtual ~FilterIncrement();

                virtual Class* getClass() const;

                virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

                virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
                virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

            protected:
                virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);

        };

        using namespace flowvr::xml;

        /// Constructor.
        FilterIncrement::FilterIncrement(const std::string &objID)
            : Filter(objID)
        {
        }

        FilterIncrement::~FilterIncrement()
        {
        }

        flowvr::plugd::Result FilterIncrement::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
        {
            flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
            if (result.error()) return result;

            //initialization of the input message queue
            initInputs(1);
            inputs[0]->setName("in");

            //initialization of the OMQ "out"
            initOutputs(1);
            outputs[0]->setName("out");
            outputs[0]->msgtype = Message::STAMPS;

            outputs[0]->newStampSpecification(dispatcher);
            // Send 0 for initialization
            MessagePut m;
            m.stamps.write(outputs[0]->stamps.it, -1);
            m.stamps.write(outputs[0]->stamps.num, -1 );
            outputs[0]->put(m,dispatcher);



            return result;
        }

        void FilterIncrement::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
        {

#ifdef DEBUG
            if (mqid == 0)
                std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
            else
                std::cout << objectID()<<": new order "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif


            sendPendingMessages(dispatcher);
            inputs[mqid]->eraseFront();

        }
        void FilterIncrement::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
        {


        }

        void FilterIncrement::sendPendingMessages(plugd::Dispatcher* dispatcher)
        { // MAIN FILTER FUNCTION


            int mit,it;
            Message msgfromIn=inputs[0]->frontMsg();
            MessageWrite mresult;

            //read data from IN port
            msgfromIn.stamps.read(inputs[0]->getStampList().it, it);

            // increment num if at least a turn of the loop has been done 
            int num = -1;
            msgfromIn.stamps.read(inputs[0]->getStampList().num, num);

            if(it>=-1)
                mit = it+1;
            else 
                mit = -1;

            // Build data
            mresult.stamps.clone(inputs[0]->frontMsg().stamps, &inputs[0]->getStampList());  
            mresult.stamps.write(outputs[0]->stamps.it,mit);  


            //send  msg
            outputs[0]->put(mresult,dispatcher, num +1);

        }

        flowvr::plugd::GenClass<FilterIncrement> FilterIncrementClass("flowvr.plugins.FilterIncrement", // name
                "", // description
                &flowvr::plugins::FilterClass
                );

        Class* FilterIncrement::getClass() const
        {
            return &FilterIncrementClass;
        }

    } // namespace plugins

} // namespace flowvr
