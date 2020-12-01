/******* COPYRIGHT ************************************************
 *                                                                 *
 *  Contact : %EMAIL%                                              * 
 *                                                                 *
 ******************************************************************/

#include "flowvr/app/component/flowvr-app.comp.h"
#include "flowvr/app/core/flowvr-app.h"

// Your app include

#include "%NAMESPACE%/app/components/%component_name%.comp.h"


using namespace flowvr::app;

namespace %NAMESPACE% 
{

  // Required to enable dynamic component loading
  // Argument given in parameter is the class name (respect case) (also requires a TicTac(id) constructor)
  GENCLASS(%APP_NAME)


  void %Component_Name%::execute()
  {
    
  };


};
