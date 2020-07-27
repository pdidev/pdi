// flowvr-app core includes
#include <flowvr/app/core/genclass.h>

#include "primes/components/modulecomposite.comp.h"

#include <flowvr/app/components/flowvr-app.comp.h>

#include <flowvr/app/components/filterroutingnode.comp.h>
#include <flowvr/app/components/syncmaxfrequency.comp.h>



using namespace flowvr;
using namespace flowvr::app;


void Composite_test_1::execute(){

    Component* composite_test_2;
    composite_test_2 = addObject(Composite_test_2("composite_test_2"));
}

void Composite_test_1::setHosts(){
    // Read mapping from CSV
    if (csvHostMap != NULL)
        {
            const  HostMap::iterator it = csvHostMap->find(getFullId());
            if (  it != csvHostMap->end() )
                {
                    hostList.clear();
                    hostList = it->second;
                }
        }
    // Propagate to children
    Composite::setHosts();

}



void Composite_test_2::execute(){

    FilterRoutingNode* r1 = addObject<FilterRoutingNode>("r1");
    SyncMaxFrequency* s2 = addObject<SyncMaxFrequency>("s2");
    s2->setParameter<float>("freq",25);
    link(r1->getPort("out"),s2->getPort("endIt"));

}
