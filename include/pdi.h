#include <yaml.h>
#include <mpi.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum PDI_status_e { 
	PDI_OK=0,
	PDI_UNAVAILABLE ///< on an input call, no such data is available
} PDI_status_t;

/// \{ Initialization / Finalization stuff

/** Initializes PDI
 * \param[in] conf the configuration
 * \param[in,out] world the main MPI communicator
 * \return an error status
 */
PDI_status_t PDI_init(const yaml_node_t *conf, MPI_Comm *world);

/** Finalizes PDI
 * \return an error status
 */
PDI_status_t PDI_finalize();

/// \}

/** Triggers a PDI “event”
 * \param[in] event the event name
 * \return an error status
 */
PDI_status_t PDI_event(const char *event);

/// \{ in/out data access

/** Let the code take a look at some data. Neither PDI nor the user code
 * should modify it before a call to either PDI_release or PDI_reclaim.
 * \param[in] name the data name
 * \param[out] data the accessed data
 * \return an error status
 */
PDI_status_t PDI_access(const char *name, void *data);

//TODO: a version of access where memory is allocated by PDI

/** Shares some data with PDI. Neither PDI nor the user code should modify it
 * before a call to either PDI_release or PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the shared data
 * \return an error status
 */
PDI_status_t PDI_share(const char *name, const void *data);

/// \}

/// \{ memory ownership management

/** Releases ownership of a data shared with PDI. PDI is then responsible to
 * free the associated memory whenever necessary.
 * \param[in] name name of the data to release
 * \return an error status
 */
PDI_status_t PDI_release(const char *name);

/** Exposes a value to PDI
 * \param[in] name name of the data to reclaim
 * \return an error status
 */
PDI_status_t PDI_reclaim(const char *name);

/// \}

/// \{ combined in/out data access & memory ownership

/** Exports some data to PDI. Equivalent to PDI_share + PDI_release.
 * \param[in] name the data name
 * \param[in] data the exported data
 * \return an error status
 */
PDI_status_t PDI_export(const char *name, const void *data);

/** Shortly exposes some data to PDI. Equivalent to PDI_share + PDI_reclaim.
 * \param[in] name the data name
 * \param[in] data the exposed data
 * \return an error status
 */
PDI_status_t PDI_expose(const char *name, const void *data);

/** Imports some data from PDI. Equivalent to PDI_access + PDI_reclaim.
 * \param[in] name the data name
 * \param[out] data the data to initialize
 * \return an error status
 */
PDI_status_t PDI_import(const char *name, void *data);

//TODO: a version of import where memory is allocated by PDI

/// \}

#ifdef __cplusplus
} // extern C
#endif
