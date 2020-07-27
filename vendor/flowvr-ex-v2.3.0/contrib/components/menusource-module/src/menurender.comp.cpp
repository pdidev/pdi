
#include <flowvr/app/components/syncmaxfrequency.comp.h>
#include <flowvr/app/components/connection.comp.h>

#include <flowvr/app/core/genclass.h>

//#include "modulemenusource.comp.h"
#include "metamodulemenusource.comp.h"
#include "menurender.comp.h"

#include <flowvr/app/components/flowvr-app.comp.h>

using namespace flowvr::app;

namespace menurender
{

GENCLASS(MenuRender)


	MenuRender::MenuRender( const std::string &id_ )
	: Composite(id_)
	{
	  setInfo("MenuRender connects  menusource and a synchronizer");

	  addPort("token_in", INPUT);
	  addPort("viewport_in", INPUT);
	  addPort("script_in", INPUT);
	  addPort("response_in", INPUT);

	  addPort("token_out", OUTPUT);
	  addPort("scene", OUTPUT);
	  addPort("request_out", OUTPUT);
	}

	void MenuRender::execute()
	{
		SyncMaxFrequency *mf     = addObject<SyncMaxFrequency>("sync");
		MetaModuleMenuSource *mm = addObject<MetaModuleMenuSource>("menusource");

		// push internal ports to the outside (with the same name)
		link(getPort("token_in"), mm->getPort("token_in") );
		link(getPort("viewport_in"), mm->getPort("viewport_in") );
		link(getPort("script_in"), mm->getPort("script_in") );
		link(getPort("response_in"), mm->getPort("response_in") );

		link(mm->getPort("token_out"), getPort("token_out") );
		link(mm->getPort("scene"), getPort("scene"));
		link(mm->getPort("request_out"), getPort("request_out"));



		// update processor for the menu animations
		flowvr::app::ConnectionStamps *cn1 = addObject<flowvr::app::ConnectionStamps>( "cn1" );
		flowvr::app::ConnectionStamps *cn2 = addObject<flowvr::app::ConnectionStamps>( "cn2" );

		link( mm->getPort("endIt"), cn1->getPort("in") );
		link( cn1->getPort("out"), mf->getPort("endIt") );

		link( mf->getPort("out"), cn2->getPort("in") );
		link( cn2->getPort("out"), mm->getPort("beginIt") );
	}
} // end namespace

