#include <Damaris.h>
#include <damaris/data/VariableManager.hpp>
//using namespace damaris;
//namespace dam = damaris;  
   /*  
    void UpdateGhostZones(std::shared_ptr<dam::Variable> v , size_t* localDim , size_t* offset )
    {
        int varDimension = v->GetLayout()->GetDimensions();
        bool retVal = true;

        for(int i=0; i<varDimension ; i++) {
            int g1 = v->GetLayout()->GetGhostAlong(i).first;
            int g2 = v->GetLayout()->GetGhostAlong(i).second;

            offset[i] = g1 ;
            localDim[i] = localDim[i] - g1 - g2;
        }

        delete [] offset;  // this would be passed back to the processing 
    }
*/

 std::string GetVariableFullName(std::shared_ptr<damaris::Variable> v , std::shared_ptr<damaris::Block> *b){
        std::stringstream varName;
        std::string baseName;
        int numDomains;

        baseName = damaris::Environment::GetSimulationName();
        numDomains = damaris::Environment::NumDomainsPerClient();

        // (b == NULL) means that there is no access to block data, i.e. in file-per-core mode or future modes.
        if (b == NULL)
            return v->GetName();

        if (numDomains == 1){
            varName << v->GetName() << "/P" << (*b)->GetSource(); // e.g. varName/P2
        } else {// more than one domain
            varName << v->GetName() << "/P" << (*b)->GetSource() << "/B" << (*b)->GetID(); // e.g. varName/P2/B3
        }

        return  varName.str();
    }

 extern "C"  void GetDataFromADamarisVariable(const char* name, int source, int iteration,
                const char* /*args*/)
    {
        
        std::cout << "Iteration: " << iteration << "   We were passed the Damaris event named: " << name  << std::endl << std::flush;
        // I assume you know the name of your variables, if not you need to search by numeric id.
        // If you need to iterate over all variables, that requires your plugin to inherit 
        // from: #include "storage/Store.hpp" or possibly have it's own XML tags to allow specification
        // that a variable will be present that can be passed through from the Damaris clients.
        std::shared_ptr<damaris::Variable> v = damaris::VariableManager::Search(name);
        if (!v) {
            std::cerr << "ERORR: iteration: " << iteration << ". No variable found by name: " << name  << std::endl << std::flush ;
            return;
        }

        // non TimeVarying variables only are written in the first iteration.
        if ((not v->IsTimeVarying()) && (iteration > 0))
            return;

        // Getting the dimension of the variable
        int varDimension;
        varDimension = v->GetLayout()->GetDimensions();
        
        size_t *globalDims;
        globalDims = new (std::nothrow) size_t[varDimension];

        if (globalDims == NULL) {
            ERROR("Failed to allocate memory for dim arrays!");
        }

        damaris::BlocksByIteration::iterator begin;
        damaris::BlocksByIteration::iterator end;
        v->GetBlocksByIteration(iteration, begin, end);
        int numBlocks = 0;
        std::string varName;
        
        // There will be a block for each Damaris client that a Damaris server looks after
        // e.g. if we run 4 ranks with one damaris server, there could be 3 blocks, or possibly
        // more if the simulation writes multiple blocks (AKA domains in the Damaris XML file).
        for (damaris::BlocksByIteration::iterator bid = begin; bid != end; bid++) {
            std::shared_ptr<damaris::Block> b = *bid;
            numBlocks++;

            // Create block dimensions
            // possibly multi-dimensional, so we need a value for each dimension
            int blockDimension = b->GetDimensions();
            size_t *blockDim = new (std::nothrow) size_t[blockDimension];
            size_t *offset   = new (std::nothrow) size_t[varDimension];
            if (blockDim == NULL)
                ERROR("HDF5:Failed to allocate memory ");

            for (int i = 0 ; i < blockDimension ; i++)
            {
                blockDim[i] = b->GetEndIndex(i) - b->GetStartIndex(i) + 1;
            }

            // Update ghost zones - returns the correct size of the dataset on file in blockDim
            // UpdateGhostZones(v, blockDim, offset);
            
            // Global dimensions may not be present (they would be set to the same as the blockDim values (i think))
            for (int i = 0; i < varDimension; i++) {
                globalDims[i] = b->GetGlobalExtent(i) ;
            }

            // Create Dataset for each block
            varName = GetVariableFullName(v , &b);

            // Getting the data
            int *ptr = (int *) b->GetDataSpace().GetData();
            std::cout << "Iteration: " << iteration << "   We have the first element from variable (" << varName << "): " << ptr[0] << std::endl ;
            // Do something with ptr - take into account the offset[] values if they are present.
            
            // Free stuff
            delete [] blockDim;
            delete [] offset;

        } // for of block iteration
        
        // } // for of variable iteration - we are not using this ATM

    }