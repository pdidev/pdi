#include <damaris/data/VariableManager.hpp>
using namespace damaris;

extern "C" void my_function(const char* varName, int source, int iteration) {
    std::cout << varName << std::endl;
    std::shared_ptr<Variable> v = VariableManager::Search("my_rank");
    std::cout << v->GetName() << std::endl;
}