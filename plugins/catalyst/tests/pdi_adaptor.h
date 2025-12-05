#ifndef PDI_ADAPTOR_H
#define PDI_ADAPTOR_H

#include <string>

class Grid;
class Attributes;

namespace pdi_adaptor {
bool initialize(const std::string& pdi_yaml_config_file_path, const Grid& grid);
bool execute(int cycle, double time, Grid& grid, Attributes& attribs);
bool finalize();
} // namespace pdi_adaptor

#endif
