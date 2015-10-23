#include "pdi.h"

#ifndef WITHOUT_MPI
PDI_status_t PDI_MPI_init(const yaml_node_t* conf, MPI_Comm* world)
{
	return PDI_OK;
}
#endif // WITHOUT_MPI

PDI_status_t PDI_init(const yaml_node_t* conf)
{
	return PDI_OK;
}

PDI_status_t PDI_finalize()
{
	return PDI_OK;
}


PDI_status_t PDI_Event(const char* event)
{
	return PDI_OK;
}


PDI_status_t PDI_share(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_access(const char* name, void* data)
{
	return PDI_OK;
}


PDI_status_t PDI_reclaim(const char* name)
{
	return PDI_OK;
}

PDI_status_t PDI_release(const char* name)
{
	return PDI_OK;
}


PDI_status_t PDI_expose(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_export(const char* name, const void* data)
{

}

PDI_status_t PDI_import(const char* name, void* data)
{
	return PDI_OK;

}
 