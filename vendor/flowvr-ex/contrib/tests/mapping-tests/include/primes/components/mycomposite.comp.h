#ifndef MYCOMP_COMP_H_
#define MYCOMP_COMP_H_

using namespace flowvr::app;

class MyComposite : public Composite {
    public :
	MyComposite(const std::string& id_) : Composite(id_) {
        addPort("in", INPUT);
        addPort("out", OUTPUT);
    }

        virtual Component* create() const {
            return new MyComposite(getId());
        }


        virtual void execute()
        {
            FilterSignalAnd * signal = addObject<FilterSignalAnd>("signaland");
            link(getPort("in"),signal->getPort("in"));
            link(signal->getPort("out"),getPort("out"));   
        }

        virtual void setHosts()
        {
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

};



#endif 
