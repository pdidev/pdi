#ifndef COMPUTE_MODIF_COMP_H_
#define COMPUTE_MODIF_COMP_H_

#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

using namespace flowvr::app;

class Composite_test_1 : public Composite {
    public :
	Composite_test_1(const std::string& id_) : Composite(id_) {
        addPort("primesOut", OUTPUT);
    }

    virtual Component* create() const {
        return new Composite_test_1(getId());
    }


    virtual void execute();


    virtual void setHosts();
};


class Composite_test_2 : public Composite {
    public :
	Composite_test_2(const std::string& id_) : Composite(id_) {
        addPort("primesOut", OUTPUT);
    }

    virtual Component* create() const {
        return new Composite_test_2(getId());
    }

    virtual void execute();
};

#endif // COMPUTE_MODIF_COMP_H_
