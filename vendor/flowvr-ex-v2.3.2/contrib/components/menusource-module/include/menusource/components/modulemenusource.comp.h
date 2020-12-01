/******************************************************************
*                                                                 *
*       File : modulemenusource.comp.h                                       *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#ifndef MODULEMENUSOURCE_COMP_H_
#define MODULEMENUSOURCE_COMP_H_

#include <flowvr/app/components/module.comp.h>

namespace menurender
{
	class ModuleMenuSource : public flowvr::app::Module
	{
	public:
		ModuleMenuSource(const std::string &id_) : Module(id_){
		  {

			setInfo("Module menusource uses CEGUI library to generate flowvr-render primitives, to display overlay menus");

			  addPort("token_in", flowvr::app::INPUT);
			  addPort("viewport_in", flowvr::app::INPUT);
			  addPort("script_in", flowvr::app::INPUT);
			  addPort("response_in", flowvr::app::INPUT);

			  addPort("token_out", flowvr::app::OUTPUT);
			  addPort("scene", flowvr::app::OUTPUT);
			  addPort("request_out", flowvr::app::OUTPUT);

		  }
		}

	   //Mandatory virtual destructor
	   virtual ~ModuleMenuSource(){};

	   flowvr::app::Component *create() const {return new ModuleMenuSource(getId());}

	};
}//namespace menusource
#endif /* MODULEMENUSOURCE_COMP_H_ */
