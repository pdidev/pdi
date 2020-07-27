/******************************************************************
*                                                                 *
*       File : metamodulemenusource.comp.h                                       *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#ifndef METAMODULEMENUSOURCE_COMP_H_
#define METAMODULEMENUSOURCE_COMP_H_

#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>
#include "modulemenusource.comp.h"

using namespace flowvr::app;

namespace menurender
{
	class MetaModuleMenuSource : public MetaModuleFlowvrRunSSHSingleton<ModuleMenuSource>
	{
	public:
		MetaModuleMenuSource(const std::string& id_) : MetaModuleFlowvrRunSSHSingleton<ModuleMenuSource>(id_, CmdLine("menusource"))
		{
			setInfo("Metamodule launching menusource modules");

			addParameter<std::string>( "logfile", "cegui.out.log" );
			addParameter<unsigned int>( "loglevel", 0);
			addParameter<std::string>( "menuconfig", "$MENURENDER_DATA_PREFIX/config/menusource-config.xml" );
			addParameter<std::string>( "menuscript", "" );
		}

		flowvr::app::Component* create() const { return new MetaModuleMenuSource(getId());};

		void configure(){
			flowvr::app::FlowvrRunSSH ssh(this);
			//ssh.addCommand(*getRun()); /// the command line given in parameter is used as the argument of flowvr-run-ssh

			ssh.addArg("menusource");
			ssh.setVerbose();
			setRun(ssh);

			setArgs("logfile");
			setArgs("loglevel");
			setArgs("menuconfig");
			setArgs("menuscript");
		}

		  //Mandatory virtual destructor
		  virtual ~MetaModuleMenuSource(){};

	private:
		void setArgs( const std::string &strParam )
		{
			std::string parm = getParameter<std::string>(strParam);
			if(!parm.empty())
			{
				getRun()->addArg( "--"+strParam + " " + parm );
			}
		}
	};
}

#endif /* METAMODULEMENUSOURCE_COMP_H_ */
