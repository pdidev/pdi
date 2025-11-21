#ifndef PDI_ADAPTOR_H
#define PDI_ADAPTOR_H

#include <string>

class Grid;
class Attributes;

namespace PDIAdaptor
{
bool Initialize(const std::string& pdi_yaml_config_file_path, const Grid& grid);
bool Execute(int cycle, double time, Grid& grid, Attributes& attribs);
bool Finalize();
}

#endif
