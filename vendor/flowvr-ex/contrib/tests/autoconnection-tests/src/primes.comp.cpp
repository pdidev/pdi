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



#include "primes/components/primes.comp.h"

using namespace flowvr::app;

namespace primes
{


    // Required to enable dynamic component loading
    // Argument given in parameter is the class name (respect case) (also requires a Primes(id) constructor)
    GENCLASS(Primes)


   // Used on one test where we need an empty component
        class EmptyComp : public Composite
        {
            public :
                EmptyComp(const std::string id_) : Composite(id_)
                {
                    setInfo("Empty Composite  for Test");
                    addPort("out",OUTPUT,FULL);
                    addPort("in",INPUT,STAMPS);
                };
                // Mandatory create method
                virtual Component* create() const	{ return new EmptyComp(this->getId());	};
        };


   


        class EmptyModule : public Module
        {
            public :
        	EmptyModule(const std::string id_) : Module(id_)
                {
                    setInfo("Empty Module  for Test");
                };


            void createNbPorts(unsigned int nbPort)
            {
                for (unsigned int i = 0; i != nbPort; ++i)
                        if (!isPortExist("out-" + toString<unsigned int>(i)))
                                addPort("out-"+toString<unsigned int>(i), OUTPUT);
            }


            virtual Component* create() const { return new EmptyModule(this->getId());  }

        };
  
        class SimplePrim : public Primitive
        {
            public :
                SimplePrim(const std::string id_) : Primitive(id_,"module")
                {
                    setInfo("Simple Primitive for Tests");
                    addPort("out",OUTPUT,FULL);
                    addPort("out2",OUTPUT,FULL);
                    addPort("out3",OUTPUT,FULL);
                    addPort("in",INPUT,FULL);
                    addPort("in2",INPUT,FULL);
                };
                virtual TypeComponent getTypeComponent()const { return OBJECT; };

                // Mandatory create method
                virtual Component* create() const	{ return new SimplePrim(this->getId());	};

                virtual bool isPrimitive() const{
                	return true;
                }
        };
                
  
        // Used on one test where we need a specific component
		 class RestrictedProducerTestComp : public Composite
		 {

			 public :
			 RestrictedProducerTestComp(const std::string id_) : Composite(id_)
				 {
					 setInfo("Composite  for Test");
					 addPort("out",OUTPUT);
					 addPort("out2",OUTPUT);
				 };
			 // Mandatory create method
			 virtual Component* create() const	{ return new RestrictedProducerTestComp(this->getId());	};

			 virtual void execute()
				 {
					std::list<const Port*> pOutPrimList;
					getPort("out")->getPrimitiveSiblingList(pOutPrimList);
					std::list<const Port*>::iterator itPOutPrimSibling = pOutPrimList.begin();

					std::list<const Port*> pOutPrimList2;
					getPort("out2")->getPrimitiveSiblingList(pOutPrimList2);
					std::list<const Port*>::iterator itPOutPrimSibling2 = pOutPrimList2.begin();


					int nbPrim = pOutPrimList.size();
					int nbPrim2 = pOutPrimList2.size();
					EmptyModule* emptyModule = addObject<EmptyModule>("emptyModule");
					emptyModule->createNbPorts(nbPrim);

					for(int i=0;i<nbPrim;i++){
						link(emptyModule->getPort("out-"+toString<int>(i)),getPort("out"));
						emptyModule->getPort("out-"+toString<int>(i))->addRestrictionToPrimitiveSibling(*itPOutPrimSibling);

						if(nbPrim2==nbPrim){
							link(emptyModule->getPort("out-"+toString<int>(i)),getPort("out2"));
                                                    	emptyModule->getPort("out-"+toString<int>(i))->addRestrictionToPrimitiveSibling(*itPOutPrimSibling2);
							itPOutPrimSibling2++;
						}
						itPOutPrimSibling++;

					}
				 }
		 };


		  // Used on one test where we need a specific component
		   class consumerTestComp : public Composite
		   {
			   public :
			   consumerTestComp(const std::string id_) : Composite(id_)
				   {
					   setInfo("Composite  for Test");
					   addParameter<int>("nbPrimitives",3);
					   addPort("in",INPUT);
				   };
				   // Mandatory create method
				   virtual Component* create() const	{ return new consumerTestComp(this->getId());	};

				   virtual void execute(){
					   int nbPrimitives=getParameter<int>("nbPrimitives");
					   for(int i = 0;i<nbPrimitives;i++){
						   ModuleVisu* moduleVisu = addObject<ModuleVisu>("visu"+toString<int>(i));
						   link(moduleVisu->getPort("keysIn"),getPort("in"));
					   }
				   }
		   };


		   class intermediateComp : public Composite
		   {
			   public :
			   intermediateComp(const std::string id_) : Composite(id_)
				   {
					   setInfo("Composite  for Test");
					   addPort("in",INPUT);
					   addPort("out",OUTPUT);
				   };
				   // Mandatory create method
				   virtual Component* create() const	{ return new intermediateComp(this->getId());	};

				   virtual void execute(){
						   FilterRoutingNode* rn = addObjectandLink<FilterRoutingNode>("rn",getPort("in"),getPort("out"));
				   }
		   };

		   class paramComp2 : public Composite
		   {
			   public :
			   paramComp2(const std::string id_) : Composite(id_)
				   {
					   addParameter("testParam");
				   };
				   // Mandatory create method
				   virtual Component* create() const	{ return new paramComp2(this->getId());	};

				   virtual void execute(){
					   std::string param = getParameter<std::string>("testParam");
				   }
		   };
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
                // Only use 1 to 1 connections.
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


                // metamodule that spawns only one module
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
                Com1ToN<FilterRoutingNode> *  bcast= addObjectandLink<Com1toN<FilterRoutingNode> >("cPrimesRequest2",presignalPrimesRequest ->getPort("out"),compute->getPort("beginIt"));
                // to change 1 to N tree arity (default: infinity)
                //setParameter<unsigned int>("TREE_ARITY",2,"cPrimesRequest2");

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
                //  - Nto1 to send stamps to the synchronizer. It uses a and filter that forwards a essage each time it receveis one on each of its inputs.
                typedef ComSync<ComNto1<FilterMerge>,ComNto1<FilterSignalAnd>,Connection,ConnectionStamps,FilterMergeIt,SyncGreedy> GreedyMergeIt;
                GreedyMergeIt * gPrimes  = addObject<GreedyMergeIt>("greedyPrimes"); // greedy  for prime numbers

                setParameter("stamp","computationTimeIt");// parameter used by the FilterMergeIt filter to accumulate computation times when lergin messages.

                // Set tree arity. Because the component contain several trees, we have to specify more precisely what component is concerned by the setparameter call
                // (otherwise all subtrees will have the same arity)
                setParameter<int>("TREE_ARITY", 2,"greedyPrimes/ComIn");// set tree arity for the ComNto1  tree of the ComIn component (first template in GreedyMereIt)
                setParameter<int>("TREE_ARITY", 3,"greedyPrimes/ComStamps"); // set tree arity for the ComNto1  tree of the ComStamps component (second template in GreedyMereIt)



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
                //Just for testing autoconnections
                // Negative test (cycle of presignals with SAMEAS/AUTO ports)

                // CAPTURE/VISU COMMNICATIONS
                //  link capture and visu
                FilterPreSignal * presignal1  = addObject<FilterPreSignal >("Presignal1");                 FilterPreSignal * presignal2  = addObject<FilterPreSignal >("Presignal2");
                FilterPreSignal * presignal3  = addObject<FilterPreSignal >("Presignal3");
                FilterPreSignal * presignal4  = addObject<FilterPreSignal >("Presignal4");

                link(presignal1->getPort("out"),presignal2->getPort("in"));
                link(presignal2->getPort("out"),presignal3->getPort("in"));
                link(presignal3->getPort("out"),presignal4->getPort("in"));
                link(presignal4->getPort("out"),presignal1->getPort("in"));


            }
        else if (example == 7)
            {
                // Just for testing autoconnection
                // Large list of Presignals: positive test

                // CAPTURE/VISU COMMNICATIONS
                //  link capture and visu
                FilterPreSignal * presignal4  = addObject<FilterPreSignal >("Presignal4");
                FilterPreSignal * presignal3  = addObject<FilterPreSignal >("Presignal3");
                FilterPreSignal * presignal2  = addObject<FilterPreSignal >("Presignal2");
        	FilterPreSignal * presignal1  = addObject<FilterPreSignal >("Presignal1");


                // backward 1 to 1 Connection  from  visu to compute to request  new data (with A list of 4 presignals)
                // expect stamp connections
                link(visu->getPort("endIt"),presignal1->getPort("in"));
                link(presignal1->getPort("out"),presignal2->getPort("in"));
                link(presignal2->getPort("out"),presignal3->getPort("in"));
                link(presignal3->getPort("out"),presignal4->getPort("in"));
                link(presignal4->getPort("out"),capture->getPort("beginIt"));


                // 1 to 1 Connection  between capture and visu to transfer mouse events
                // expect full connections
                FilterPreSignal * presignal5  = addObject<FilterPreSignal >("Presignal5");
                FilterPreSignal * presignal6  = addObject<FilterPreSignal >("Presignal6");
                FilterPreSignal * presignal7  = addObject<FilterPreSignal >("Presignal7");
                FilterPreSignal * presignal8  = addObject<FilterPreSignal >("Presignal8");

                link(capture->getPort("keysOut"),presignal5->getPort("in"));
                link(presignal5->getPort("out"),presignal6->getPort("in"));
                link(presignal6->getPort("out"),presignal7->getPort("in"));
                link(presignal7->getPort("out"),presignal8->getPort("in"));
                link(presignal8->getPort("out"),visu->getPort("keysIn"));
            }
        
        else   if (example == 8)
            {
                //Just for testing autoconnections: 
                // Negative test   stamps to full connection
                link(capture->getPort("endIt"),visu->getPort("keysIn"));
            }
        else if (example == 9)
            {
                //Just for testing autoconnections: should raise 1 errors
                // Negative test: input port receiving  several incoming primitive components

                FilterPreSignal * presignal1  = addObject<FilterPreSignal >("Presignal1");

                link(capture->getPort("endIt"),visu->getPort("keysIn"));
                link(presignal1->getPort("out"),visu->getPort("keysIn"));                     

               
            }
        else if (example == 10)
            {
                //Just for testing autoconnections: 
                // Positive  test:  output port connected to several primitive objects expecting different types of data (FULL, STAMPS and SAMEAS)
                
                FilterPreSignal * presignal1  = addObject<FilterPreSignal >("Presignal1");
                FilterPreSignal * presignal2  = addObject<FilterPreSignal >("Presignal2");

                link(presignal1->getPort("out"),visu->getPort("keysIn")); // FULL
                link(presignal1->getPort("out"),capture->getPort("beginIt")); // STAMPS               
                link(presignal1->getPort("out"),presignal2->getPort("in")); // SAMEAS    
                link(presignal2->getPort("out"),visu->getPort("beginIt")); // to make sure  of presignal2 can be resolved

            }
        else if (example == 11)
            {
                //Just for testing autoconnections: 
                // Negative test:  AUTO port not connected to any primitive: cannot be resolved to FULL or STAMPS.
                 
                FilterRoutingNode *  rtn = addObject<FilterRoutingNode>("rtn");
                link(visu->getPort("endIt"),rtn->getPort("in"));
            }
        else if (example == 12)
            {
                //Just for testing autoconnections:
                // Positive test:   empty component -> no connectiong generated
                
                EmptyComp * emptycomp = addObject<EmptyComp >("empty");

                link(emptycomp->getPort("out"),visu->getPort("primesIn")); 
            }
        else if (example == 13)
            {
                //Just for testing autoconnections:
                // Negative test: com1ton with no  primitive source.
                 
                EmptyComp * emptycomp = addObject<EmptyComp >("empty");

                Com1ToN<FilterRoutingNode> *  com = addObject< Com1toN<FilterRoutingNode> >("bcast");

                link(emptycomp->getPort("out"),com->getPort("in"));
                link(com->getPort("out"),visu->getPort("primesIn"));
            }
        else if (example == 14)
            {
                //Just for testing autoconnections:
                // Negative test: com1ton with no  primitive  destination
                 
                EmptyComp * emptycomp = addObject<EmptyComp >("empty");
                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");

                Com1ToN<FilterRoutingNode> *  com = addObject< Com1toN<FilterRoutingNode> >("bcast");

                link(compute->getPort("endIt"),com->getPort("in"));
                link(emptycomp->getPort("in"),com->getPort("out"));
            }
        else if (example == 15)
            {
                //Just for testing autoconnections:
                // Negative test:   self links are forbiden
                
                EmptyComp * emptycomp = addObject<EmptyComp >("empty");

                link(visu->getPort("endIt"),emptycomp->getPort("in"));
                link(emptycomp->getPort("in"),emptycomp->getPort("out"));//This is ilegal.
                link(emptycomp->getPort("out"),visu->getPort("primesIn")); 
            }
        else if (example == 16)
            {
                //Just for testing autoconnections:
                // Positive test: com1ton with unusual arity and n value

                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");
                
                Com1ToN<FilterRoutingNode> * com = addObject< Com1toN<FilterRoutingNode> >("bcast");
                com->setParameter<int>("TREE_ARITY",2);
                
                link(visu->getPort("endIt"),com->getPort("in"));
                link(com->getPort("out"),compute->getPort("beginIt"));
            }
        else if (example == 17)
            {
                //Just for testing autoconnections:
                // Positive test: com1ton with   n % arity != 0

                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");
                
                Com1ToN<FilterRoutingNode> * com = addObject< Com1toN<FilterRoutingNode> >("bcast");
                com->setParameter<int>("TREE_ARITY",3);
                
                link(visu->getPort("endIt"),com->getPort("in"));
                link(com->getPort("out"),compute->getPort("beginIt"));
            }
        else if (example == 18)
            {
                //Just for testing autoconnections:
                // Positive test: comnto1 with   n % arity != 0

                MetaModuleCompute * compute  = addObject<MetaModuleCompute >("compute");
                
                ComNto1<FilterMerge> * com = addObject< ComNto1<FilterMerge> >("bcast");
                com->setParameter<int>("TREE_ARITY",3);
                
                link(compute->getPort("primesOut"),com->getPort("in"));
                link(com->getPort("out"),visu->getPort("beginIt"));
            }
        else if (example == 19)
            {
                // Just for testing autoconnection
                // link with multiplex and presignal : (should be)positive test / now fatal error
                
                // CAPTURE/VISU COMMNICATIONS
                //  link capture and visu
               	FilterPreSignal * presignal1  = addObject<FilterPreSignal >("Presignal1");
               	FilterMultiplex * multiplex1  = addObject<FilterMultiplex >("multiplex1");
                
                // backward 1 to 1 Connection  from  visu to compute to request  new data (with A list of 4 presignals)
                // expect stamp connections
                link(visu->getPort("endIt"),presignal1->getPort("in"));
                link(presignal1->getPort("out"),multiplex1->getPort("in"));
                link(multiplex1->getPort("out"),capture->getPort("beginIt"));

                
                // 1 to 1 Connection  between capture and visu to transfer mouse events
                // expect full connections
                FilterMultiplex * multiplex2  = addObject<FilterMultiplex >("multiplex2");
                FilterPreSignal * presignal2  = addObject<FilterPreSignal >("Presignal2");
                 
                link(capture->getPort("keysOut"),multiplex2->getPort("in"));
                link(multiplex2->getPort("out"),presignal2->getPort("in"));
                link(presignal2->getPort("out"),visu->getPort("keysIn"));
                
            }
        else if (example == 20)
            {
                // Negative test.
                // Just for testing autoconnection
                // Link between com1toN and multiplex : execute deadlock because depency cycle.
                
                // com1toN  followed by a multiplex : Execute deadlock
                FilterMultiplex * multiplex2  = addObject<FilterMultiplex >("multiplex2");
                Com1ToN<FilterRoutingNode> * com1toN   = addObject<Com1toN<FilterRoutingNode> >("com1toN");
                
                link(capture->getPort("keysOut"), com1toN->getPort("in"));
                link(com1toN->getPort("out"),multiplex2->getPort("in"));
                link(multiplex2->getPort("out"),visu->getPort("keysIn"));
                
            }
        else if (example == 21)
            {
                // Positive test
                // Just for testing autoconnection: test a delicate  order constraint when reseting the restrictions 
                // when setConnection add  a new connection on the input of the routing node  - @see  Composite::setConnections()
                //
                // a MetaModuleFlowvrRunSSHSingleton is composed of a patternparallel
                // and here its lookupnbinstances parameter is set to "from port"
                
                //new visu with specific parameters + a routing node
                FilterRoutingNode * RoutingNode1  = addObject<FilterRoutingNode >("RoutingNode1");
                MetaModuleVisu* visu2 = addObject<MetaModuleVisu >("visu2");
                visu2->setParameter<std::string>("LOOKUP_NB_INSTANCES_METHOD","FROM_PORT");
                visu2->setParameter<std::string>("PORT_NAME_INSTANCES","keysIn");
                
                //linking visu2, routing node and capture
                link(capture->getPort("keysOut"),visu2->getPort("keysIn"));
                link(RoutingNode1->getPort("in"),visu2->getPort("endIt"));
                link(RoutingNode1->getPort("out"),capture->getPort("beginIt"));
            }

        else if (example == 22)
            {
                // Positive test 
                // Just for testing autoconnection
                // first composite is composed of a single primitive that has several port linked to the output port (with restriction)
                // second one is composed of several primitive that are linked to the input port
                
                RestrictedProducerTestComp* producerComp = addObject<RestrictedProducerTestComp>("producerComp");
                consumerTestComp* consumerComp = addObject<consumerTestComp>("consumerComp");
                consumerComp->setParameter<int>("nbPrimitives",2);
                link(producerComp->getPort("out"),consumerComp->getPort("in"));
                
            }
        else if (example == 23)
            {
                // Positive test
                // Just for testing autoconnection
                // link between composite and several primitive 
                // first composite is composed of a single primitive that has several port linked to the output port (with restriction)
                
                RestrictedProducerTestComp* producerComp = addObject<RestrictedProducerTestComp>("producerComp");
                ModuleVisu* visu0  = addObject<ModuleVisu >("visuu0");
                ModuleVisu* visu1  = addObject<ModuleVisu >("visuu1");
                ModuleVisu* visu2  = addObject<ModuleVisu >("visuu2");
                link(producerComp->getPort("out"),visu0->getPort("keysIn"));
                link(producerComp->getPort("out"),visu1->getPort("keysIn"));
                link(producerComp->getPort("out"),visu2->getPort("keysIn"));
            }
        else if (example == 24)
            {
                // Positive test
                // Just for testing autoconnection
                // link between 3 composite 
                // first composite is composed of a single primitive that has several port linked to the output port (with restriction)
                // second one is composed of a single filterRoutingNode
                // third one is composed of several primitive that are linked to the input port
                
                
                RestrictedProducerTestComp* producerComp = addObject<RestrictedProducerTestComp>("producerComp");
                PatternParallelFromPorts<intermediateComp>* intermediate = addObject<PatternParallelFromPorts<intermediateComp> >("intermediateComp");
                consumerTestComp* consumerComp = addObject<consumerTestComp>("consumerComp");
                
                consumerComp->setParameter<int>("nbPrimitives",3);
                //intermediate->setFromPortInstances("out");
                intermediate->setParameter("INSTANCES_FROM_PORT", "out");
                
                link(producerComp->getPort("out"),intermediate->getPort("in"));
                link(intermediate->getPort("out"),consumerComp->getPort("in"));
            }

        else if (example == 25)
            {
                // Positive test
                // Just for testing autoconnection

                // Teste the particular case where a  child component  (emptyModule) has each port pointing to each  port (out and out-2) 
                // on the parent component (producerComp). (more than one parentLink).

                // link between 3 composite
                // first composite is composed of a single primitive that has several port linked to 2 output ports (with restriction)
                // second and third one are composed of several primitive that are linked to the input port


                RestrictedProducerTestComp* producerComp = addObject<RestrictedProducerTestComp>("producerComp");
                consumerTestComp* consumerComp = addObject<consumerTestComp>("consumerComp");
                consumerTestComp* consumerComp2 = addObject<consumerTestComp>("consumerComp2");

                consumerComp->setParameter<int>("nbPrimitives",3);
                consumerComp2->setParameter<int>("nbPrimitives",3);

                link(producerComp->getPort("out"),consumerComp->getPort("in"));
                link(producerComp->getPort("out2"),consumerComp2->getPort("in"));
            }
        else if (example == 26)
            {
                // Positive test
                //testing parameters set from a command line (parameter without default value) are correctly set  when cloning composite.
                
                //May lead to a "No value found for parameter" if something wrong.
                
                paramComp2* paramcomp2 = addObject<paramComp2>("paramComp2");
        	}        
        else if (example == 27)
            {
                // Positive test
                // Test  pattern parallel in a non trivial pattern
                
                PatternParallelFromPorts<SimplePrim>* start  = addObject<PatternParallelFromPorts<SimplePrim> >("start");

                PatternParallelFromPorts<SimplePrim>* intermediate = addObject<PatternParallelFromPorts<SimplePrim> >("intermediateComp");
                PatternParallelFromPorts<SimplePrim>* final = addObject<PatternParallelFromPorts<SimplePrim> >("final");
                PatternParallelFromPorts<SimplePrim>* final2 = addObject<PatternParallelFromPorts<SimplePrim> >("final2");

                //start->setStaticInstances(4);
			intermediate->setParameter("INSTANCES_FROM_PORT", "in");
			final->setParameter("INSTANCES_FROM_PORT", "in");
			final2->setParameter("INSTANCES_FROM_PORT", "in");
                //intermediate->setFromPortInstances("in");
                //final->setFromPortInstances("in");
                //final2->setFromPortInstances("in");

                link(start->getPort("out"),intermediate->getPort("in"));
                link(intermediate->getPort("out"),final->getPort("in"));
                link(intermediate->getPort("out"),final2->getPort("in"));
                link(start->getPort("out2"),final->getPort("in2"));
                link(start->getPort("out3"),final2->getPort("in2"));

            }
        else
            {
                // throw exception if value of example is wrong. Use exception and not message error not to mess-up with the traverse process.
                throw CustomException("The parameter example="+toString<int>(example)+" does not exist.Choose in range [0,27]",__FUNCTION_NAME__);
            }
        
    };
};
