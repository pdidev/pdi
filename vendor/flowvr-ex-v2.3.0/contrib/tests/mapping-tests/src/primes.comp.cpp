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
 *  Contact : Jean-Denis Lesage                                    *
 *                                                                 *
 ******************************************************************/

// flowvr-app core includes
#include <flowvr/app/core/genclass.h>


// primes components includes
#include "primes/components/metamodulecompute.comp.h"
#include "primes/components/metamodulevisu.comp.h"
#include "primes/components/metamodulecapture.comp.h"

// basic components includes
// #include <flowvr/app/components/filterit.comp.h>
// #include <flowvr/app/components/bodygreedy.comp.h>
// #include <flowvr/app/components/syncgreedy.comp.h>
// #include <flowvr/app/components/comsync.comp.h>
// #include <flowvr/app/components/patternsync.comp.h>
// #include <flowvr/app/components/com1ton.comp.h>
// #include <flowvr/app/components/comnto1.comp.h>
// #include <flowvr/app/components/filtermerge.comp.h>
// #include <flowvr/app/components/filterroutingnode.comp.h>
// #include <flowvr/app/components/filtersignaland.comp.h>
// #include <flowvr/app/components/filtermergeit.comp.h>
// #include <flowvr/app/components/syncmaxfrequency.comp.h>
// #include <flowvr/app/components/connection.comp.h>
// #include <flowvr/app/components/filterpresignal.comp.h>
#include <flowvr/app/components/flowvr-app.comp.h> // Contain all components header files.

#include "primes/components/modulecomposite.comp.h"
#include "primes/components/mycomposite.comp.h"

#include "primes/components/primes.comp.h"

using namespace flowvr::app;

namespace primes
{


    // Required to enable dynamic component loading
    // Argument given in parameter is the class name (respect case) (also requires a Primes(id) constructor)
    GENCLASS(Primes)


    void Primes::execute()
    {
        // Metamodules

        // metamodule that spawns only one module
        MetaModuleCapture * capture  = addObject<MetaModuleCapture >("capture");
        // metamodule that spawns only one module
        MetaModuleVisu * visu  = addObject<MetaModuleVisu >("visu");

        int example = getParameter<int>("example");

        if (example == 0)
            {
                // Only use a 1 to 1 connection 
                // Push model without control on FIFO buffer sizes (may lead to a memory  overflow)

                // CAPTURE/VISU COMMNICATIONS
                //  link capture and visu
                link(capture->getPort("keysOut"),visu->getPort("keysIn"));

            }
        else if (example == 1)
            {
                //  Only use 1 to 1 connections.
                //  Backward connections to implement a pull model (get data on request)
                //  Need to set  a PreSignal filter on this connection to generate a first message to unlock the cycle.

                // CAPTURE/VISU COMMNICATIONS
                // 1 to 1 Connection  between capture and visu to transfer mouse events
                link(capture->getPort("keysOut"),visu->getPort("keysIn"));

                // backward 1 to 1 Connection  from  visu to compute to request  new data (with presignal to unlock the cycle)
                FilterPreSignal * presignalKeysRequest  = addObject<FilterPreSignal >("PresignalKeysRequest");
                link(visu->getPort("endIt"),presignalKeysRequest->getPort("in"));
                link(presignalKeysRequest->getPort("out"),capture->getPort("beginIt"));

            }

        else if (example == 2)
            {
                // Only use 1 to 1 connections.
                // Use a greedy (re-sampling) pattern on keys input port of  visu to asynchronize visu from capture.
                // Use a maxfrequency  synchronizer on capture to control  its frequency.

                // CAPTURE/VISU COMMNICATIONS


                // Greedy pattern on mouse events: visu runs asynchronously from capture.
                typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy;
                Greedy * gKeys  = addObject<Greedy >("greedyKeys"); // greedy  for prime numbers

                // link capture to greedy
                link(capture->getPort("keysOut"),gKeys->getPort("in"));
                // link greedy to visu
                link(gKeys->getPort("out"),visu->getPort("keysIn"));
                // link  visu (request for new data) to greedy
                link(visu->getPort("endIt"),gKeys->getPort("sync"));

                // CAPTURE

                // Add a  max frequency synchronizor   to limit the frequency of capture
                // Notice that this synchronizor directly  sends a first message to its out port to boot connection cycle with visu
                SyncMaxFrequency * sMaxFrequency  = addObject<SyncMaxFrequency >("MaxFrequency");
                sMaxFrequency->setParameter<float>("freq",25);

                // link  filter to capture  (each new message received on  the beginIt  port triggers a new iteration)
                link(sMaxFrequency->getPort("out"),capture->getPort("beginIt"));
                // link capture  to filter  (request for new data)
                link(capture->getPort("endIt"), sMaxFrequency->getPort("endIt"));

            }

        else if (example == 3)
            {
                // Only use 1 to 1 connections.
                // Set the compute metamodule
                // Use a greedy (re-sampling) pattern on keys input port of  visu to asynchronize visu from capture.
                // Use a maxfrequency  synchronizer on capture to control  its frequency.
                // This example fails if more than one host is assigned to the Compute metamodule. Use in this case the next example.


                // metamodule that is expected to  spawn only one module (example fails otherwise)
                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");


                // COMPUTE/VISU COMMNICATIONS

                // 1 to 1 Connection  between compute and visu to transfer prime numbers.
                link(compute->getPort("primesOut"),visu->getPort("primesIn"));


                // backward 1 to 1 Connection  from  visu to compute to request  new data (with presignal to unlock the cycle)
                FilterPreSignal * presignalPrimesRequest  = addObject<FilterPreSignal >("PresignalPrimesRequest");
                link( visu->getPort("endIt"), presignalPrimesRequest->getPort("in"));
                link(presignalPrimesRequest->getPort("out"),compute->getPort("beginIt"));

                // CAPTURE/VISU COMMNICATIONS

                // Greedy pattern on mouse events: visu runs asynchronously from capture.
                typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy;
                Greedy * gKeys  = addObject<Greedy >("greedyKeys"); // greedy  for prime numbers

                // link capture to greedy
                link(capture->getPort("keysOut"),gKeys->getPort("in"));
                // link greedy to visu
                link(gKeys->getPort("out"), visu->getPort("keysIn"));
                // link  visu (request for new data) to greedy
                link( visu->getPort("endIt"),gKeys->getPort("sync"));

                // CAPTURE

                // Use a  max frequency synchronizor   to limit the frequency of capture
                // Notice that this synchronizor directly  sends a first message to its out port to boot connection cycle with visu
                SyncMaxFrequency * sMaxFrequency  = addObject<SyncMaxFrequency >("MaxFrequency");
                sMaxFrequency->setParameter<float>("freq",25);
                // link  filter to capture with a ConnectionStamps (each new message received on  the beginIt  port triggers a new iteration)
                link(sMaxFrequency->getPort("out"),capture->getPort("beginIt"));
                // link capture  to filter with a ConnectionStamps  (request for new data)
                link(capture->getPort("endIt"), sMaxFrequency->getPort("endIt"));

            }
        else if (example == 4)
            {
                // We now consider that the compute  (meta)module can be parallelized: it can starts several modules.
                // Production of primes numbers is thus distributed. We gather the partial results of each module
                // using a N to 1 communication pattern.


                // metamodule that can spawn several modules
                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");

                // COMPUTE/VISU COMMNICATIONS

                // Comunication N to 1 to merge results  from the different compute modules
                // to visu. By default the arity of the ComNto1 tree is infinity (this is a simple gather),
                // Use a  MergeFilter to merge incomming messages.
                addObjectandLink<ComNto1<FilterMerge> >("Merge",compute->getPort("primesOut"),visu->getPort("primesIn"));


                // backward 1 to N Connection  from  visu to compute to request  new data (with presignal to unlock the cycle)

                //  add presignal filter and link to visu  with a connection
                FilterPreSignal * presignalPrimesRequest  = addObject<FilterPreSignal >("PresignalPrimesRequest");
                link(visu->getPort("endIt"), presignalPrimesRequest ->getPort("in"));

                // link presignal filter with compute using a 1 to N broadcast  connection
                Com1toN<FilterRoutingNode> *  bcast= addObjectandLink<Com1toN<FilterRoutingNode> >("cPrimesRequest2",presignalPrimesRequest ->getPort("out"),compute->getPort("beginIt"));
                // to change 1 to N tree arity (default: infinity)
                //                setParameter<unsigned int>("TREE_ARITY",2,"cPrimesRequest2");

                // CAPTURE/VISU COMMNICATIONS


                // Greedy pattern on mouse events: visu runs asynchronously from capture.
                typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy;
                Greedy * gKeys  = addObject<Greedy >("greedyKeys"); // greedy  for prime numbers

                // link capture to greedy
                link(capture->getPort("keysOut"),gKeys->getPort("in"));
                // link greedy to visu
                link(gKeys->getPort("out"),visu->getPort("keysIn"));
                // link  visu (request for new data) to greedy
                link(visu->getPort("endIt"),gKeys->getPort("sync"));


            }
        else if (example == 5)
            {

                // We now consider that the compute  (meta)module can be parallelized: it can starts several modules.
                // Production of primes numbers is thus distributed. We gather the partial results of each module
                // using a N to 1 communication pattern. We use a binary tree  for this merging. We also append after the
                // merging a greedy pattern to asynchronize compute from visu. But, because we  don't want to lose data,
                // we use a filtermergeit filter, that merges all incoming messages  when visu makes a request for a new message.
                // When merging messages the filter also cumulate the computation time stored in the 'computationTimeIt' stamp.

                // metamodule that can spawn several modules
                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");

                // COMPUTE/VISU COMMNICATIONS

                // Add a synchronizer pattern based on the  MergeIt filter.
                // Because compute is a parallel module (multiple instances running),
                // incoming connections should be:
                //  - Nto1 to send primes to the MergeIt filter. It uses a merge filter to gather partial results from each compute instance
                //  - Nto1 to send stamps to the synchronizer. It uses a and filter that forwards a message each time it receives one on each of its inputs.

                typedef ComSync<ComNto1<FilterMerge>,ComNto1<FilterSignalAnd>,Connection,ConnectionStamps,FilterMergeIt,SyncGreedy> GreedyMergeIt;
                GreedyMergeIt * gPrimes  = addObject<GreedyMergeIt>("greedyPrimes"); // greedy  for prime numbers

                setParameter("stamp","computationTimeIt");// parameter used by the FilterMergeIt filter to accumulate computation times when lergin messages.

                // Set tree arity. Because the component contain several trees, we have to specify more precisely what component is concerned by the setparameter call
                // (otherwise all subtrees will have the same arity)
                setParameter<int>("TREE_ARITY", 2,"greedyPrimes/ComIn");// set tree arity for the ComNto1  tree of the ComIn component (first template in GreedyMereIt)
                setParameter<int>("TREE_ARITY", 4,"greedyPrimes/ComStamps"); // set tree arity for the ComNto1  tree of the ComStamps component (second template in GreedyMereIt)



                // link compute outpute to greedy input
                link(compute->getPort("primesOut"),gPrimes->getPort("in"));
                // link greedyPrimes output to visu input
                link(gPrimes->getPort("out"), visu->getPort("primesIn"));
                // link  visu (request for new data) to greedy
                link( visu->getPort("endIt"),gPrimes->getPort("sync"));


                // CAPTURE/VISU COMMNICATIONS

                // Greedy pattern on mouse events: visu runs asynchronously from capture.
                typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy;
                Greedy * gKeys  = addObject<Greedy >("greedyKeys"); // greedy  for prime numbers

                // link capture to greedy
                link(capture->getPort("keysOut"),gKeys->getPort("in"));
                // link greedy to visu
                link(gKeys->getPort("out"),visu->getPort("keysIn"));
                // link  visu (request for new data) to greedy
                link(visu->getPort("endIt"),gKeys->getPort("sync"));
            }
        else if (example == 6)
        {
               Component* composite_test_1;
               composite_test_1 = addObject(Composite_test_1("composite_test_1"));
        }
        else if (example == 7)
        {
               Component* composite_test_2;
               composite_test_2 = addObject(Composite_test_2("composite_test_2"));
        }
        else if (example == 8)
        {
        	// Test host lit propagation from composite to primitive (adda signaland filter between capture and visu)
        	MyComposite* mycomp  = addObject<MyComposite>("mycomp");
        	link(capture->getPort("endIt"),mycomp->getPort("in"));
        	link(mycomp->getPort("out"),visu->getPort("beginIt"));
        }
        else if (example == 9)
        {
        	// Test metamoduleflowvr-run-ssh-parallelfromport
        	MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");
        	MetaModuleVisuParallel * visuparallel  = addObject<MetaModuleVisuParallel >("metamodvisuparallel");
        	link(compute->getPort("primesOut"),visuparallel->getPort("primesIn"));
        }
        else
            {
                // throw exception if value of example is wrong. Use exception and not message error not to mess-up with the traverse process.
                throw CustomException("The parameter example="+toString<int>(example)+" does not exist.Choose in range [0,5]",__FUNCTION_NAME__);
            }

    };
};
