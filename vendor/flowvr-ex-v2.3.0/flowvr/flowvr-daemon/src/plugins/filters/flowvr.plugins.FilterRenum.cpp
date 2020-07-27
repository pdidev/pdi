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
 * File: src/plugins/flowvr.plugins.FilterFilterRenum.cpp          *
 *                                                                 *
 * Contacts:                                                       *
 *  25/06/2008 khalid Zaida <khalid.zaida@gmail.com>               *
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

        /// \brief Transmits messages from 'in' to 'out', using it numbers from 'it'
        ///
        /// <b>Init parameters:</b> none.
        ///
        /// <b>Input ports:</b>
        /// -  <b>in</b>: Message to be renumbered.
        /// -  <b>it</b>: contains the new renumbering.
        ///
        /// <b>Output Ports:</b>
        /// - <b>out</b>: renumbered message.

        class FilterRenum : public Filter
        {
            public:

                FilterRenum (const std::string &objID);

                virtual ~FilterRenum ();

                virtual Class* getClass() const;

                virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

                virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

                virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

                bool stampsSpecified;

                int lastNum;  ///< number of messages sent

                enum {
                    IDPORT_IN=0,
                    IDPORT_IT=1
                };


            protected:
                virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

        };

        using namespace flowvr::xml;

        /// Constructor.
        FilterRenum::FilterRenum(const std::string &objID)
            : Filter(objID), stampsSpecified(false), lastNum(0)
        {
        }

        FilterRenum::~FilterRenum()
        {
        }

        flowvr::plugd::Result FilterRenum::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
        {
            flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
            if (result.error()) return result;

            //initialization of the input message queue
            initInputs(2);
            inputs[IDPORT_IN]->setName("in");
            inputs[IDPORT_IN]->setCouldBeDisconnected(); // This port can be disconnected (it is the goal of this filter)
            inputs[IDPORT_IT]->setName("it");

            //initialization of the OMQ "out"
            initOutputs(1);
            outputs[0]->setName("out");
            outputs[0]->msgtype = Message::FULL;

            return result;
        }

        void FilterRenum::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
        {
#ifdef DEBUG
            if (mqid == IDPORT_IN)
                std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
            else
                std::cout << objectID()<<": new itOrder "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
            sendPendingOrders(dispatcher);
        }

        void FilterRenum::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
        {
            // forward specification to out port
#ifdef DEBUG
            std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif

            if(mqid == IDPORT_IN && !stampsSpecified)
            {	  
                //forwarding stamplist specification to the OMQ
                outputs[0]->stamps=inputs[mqid]->getStampList();
                outputs[0]->newStampSpecification(dispatcher);
                stampsSpecified = true;
            }
        }

        void FilterRenum::sendPendingOrders(plugd::Dispatcher* dispatcher)
        { // MAIN FILTER FUNCTION

            MessagePut mresult;
            Message msgfromIt,msgfromIn;
            int it, mit = 0;

            if (inputs[IDPORT_IN]->empty())
            {
                return;
            }

            if (inputs[IDPORT_IT]->empty())
            {
                return;
            }

            msgfromIn=inputs[IDPORT_IN]->frontMsg();
            msgfromIt=inputs[IDPORT_IT]->frontMsg(); 
            // Read data and set new it
            msgfromIt.stamps.read(inputs[IDPORT_IT]->getStampList().it, mit); 
            msgfromIt.stamps.read(inputs[IDPORT_IT]->getStampList().it, it); 


            // set the stamps 
            mresult.stamps.clone(inputs[IDPORT_IN]->frontMsg().stamps, &inputs[IDPORT_IN]->getStampList());  
            mresult.stamps.write(outputs[0]->stamps.it, mit);

            mresult.data =msgfromIn.data;

            //sending message from the output port out
            outputs[0]->put(mresult,dispatcher);

            inputs[IDPORT_IT]->eraseFront();
            inputs[IDPORT_IN]->eraseFront();
        }

        flowvr::plugd::GenClass<FilterRenum> FilterRenumClass("flowvr.plugins.FilterRenum", // name
                "", // description
                &flowvr::plugins::FilterClass
                );

        Class* FilterRenum::getClass() const
        {
            return &FilterRenumClass;
        }

    } // namespace plugins

} // namespace flowvr
