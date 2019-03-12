/******* COPYRIGHT ************************************************
 *                                                                 *
 *  Contact : %EMAIL% 
 *                                                                 *
 ******************************************************************/

// basic components includes
#include <flowvr/app/components/flowvr-app.comp.h>

#ifndef %COMPONENT_NAME% 
#define %COMPONENT_NAME% 

using namespace flowvr::app;

namespace %NAMESPACE%{

    class %Component_Name% : public %PARENT_NAME% 
    {
        public :
            %Component_Name%(const std::string& id_) : %PARENT_NAME%(id_)
        {

            //TODO
            setInfo("Put some info on %component_name% HERE")

                // Interface declaration
                // TODO Declare your ports here 
                // addPort("text", INPUT);

                //TODO Decalare your parameters
                // addParameter("MyParameter");
        };

            // Mandatory create method
            virtual Component* create() const { return new %Component_Name%(getId());};
    };

};
#endif //%COMPONENT_NAME% 
