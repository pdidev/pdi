#ifndef _MENURENDER_H_
#define _MENURENDER_H_


#include <flowvr/app/core/flowvr-app.h> // contain all core header files

namespace menurender
{

    // Primes is a composite component
    class MenuRender : public flowvr::app::Composite
    {
    public :
    	MenuRender(const std::string &id_);
        virtual ~MenuRender() {}

        // Composite components need an execute method.
        virtual void execute();

        // Mandatory create method
        virtual Component* create() const
		{
        	return new MenuRender(this->getId());
		}


    };


} // namespace menurender


#endif //_MENURENDER_H_
