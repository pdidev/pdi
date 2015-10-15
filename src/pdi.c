#include "paraconf.h"
#include "pdi.h"

int PDI_init(const paraconf_t* conf)
{
  return PDI_OK;
}

int PDI_MPI_init(MPI_Comm* world, const paraconf_t* conf)
{
  return PDI_OK;
}

int PDI_finalize()
{
  return PDI_OK;
}

int PDI_val(const void* data, const char* name)
{
  return PDI_OK;
}

int PDI_Event(const char* event)
{
  return PDI_OK;
}
