#if !defined(_DYNHIERARCHY_COMP_H_)
#define _DYNHIERARCHY_COMP_H_

#include <flowvr/app/core/component.h>
#include <flowvr/app/core/genclass.h>
#include <flowvr/app/core/run.h>
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>
#include <flowvr/app/components/module.comp.h>
#include <iostream>

namespace tests
{
	class ShallowComposite : public flowvr::app::Composite
	{
	public:
		ShallowComposite( const std::string &id_)
		: flowvr::app::Composite(id_)
		{

		}

		flowvr::app::Component *create() const { return new ShallowComposite( getId() ); }

		virtual void execute() {}
	};

	class EmptyModule : public flowvr::app::Module
	{
	public:
		EmptyModule( const std::string &id_)
		: flowvr::app::Module(id_)
		{}

		virtual Component *create() const { return new EmptyModule( getId() ); }
		virtual void execute() {}
	};

	class MetaModuleEmptyModule : public flowvr::app::MetaModuleFlowvrRunSSHSingleton<EmptyModule>
	{
	public:
		MetaModuleEmptyModule( const std::string &id_)
		: flowvr::app::MetaModuleFlowvrRunSSHSingleton<EmptyModule>(id_, flowvr::app::CmdLine("none") )
		{}

		virtual Component *create() const { return new MetaModuleEmptyModule(getId()); }
	};

	class DynHierarchy : public flowvr::app::Composite
	{
	public:
		DynHierarchy( const std::string &id_ )
		: flowvr::app::Composite(id_)
		{
			addParameter<int>("levels", 1 );
		}

		flowvr::app::Composite *dive( flowvr::app::Composite *parent, int nLevel, int nMax )
		{
			if( nLevel == nMax )
			{
				parent->addObject<MetaModuleEmptyModule>("empty-module");
				return NULL;
			}
			else
			{
				flowvr::app::Composite *child = parent->addObject<ShallowComposite>(toString<int>(nLevel));
				dive( child, nLevel+1, nMax );
				return child;
			}
		}

		virtual void execute()
		{
			dive( this, 0, getParameter<int>("levels") );
		}

		virtual Component *create() const
		{
			return new DynHierarchy(getId());
		}
	};

	GENCLASS(DynHierarchy);
}


#endif // _DYNHIERARCHY_COMP_H_

