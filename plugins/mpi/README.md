# The MPI plugin {#mpi_plugin}

**WARNING** This documentation is a work in progress and does not reflect the
full potential of the MPI plugin.

The MPI plugin defines two types in PDI:
* `MPI_Comm` - the C `MPI_Comm` type,
* `MPI_Comm_f` - the Fortran communicator type, an integer.

MPI plugin exposes several metadata that correspond with mpi library.
* `MPI_COMM_WORLD.rank` - an integer value, represents the rank of the proccess
  in the MPI_COMM_WORLD
* `MPI_COMM_WORLD` - a MPI_Comm, MPI_COMM_WORLD for C language
* `MPI_COMM_SELF` - a MPI_Comm, MPI_COMM_SELF for C language
* `MPI_COMM_NULL` - a MPI_Comm, MPI_COMM_NULL for C language
* `MPI_COMM_WORLD_F` - an integer, MPI_COMM_WORLD for Fortran language
* `MPI_COMM_SELF_F` - an integer, MPI_COMM_SELF for Fortran language
* `MPI_COMM_NULL_F` - an integer, MPI_COMM_NULL for Fortran language

All the metadata above are available from the call to `PDI_init` to the call to `PDI_finalize`.

At the yaml tree root plugin is a map that contains the following keys:
|key|value|
|:--|:----|
|`"logging"` (*optional*)|a \ref logging_node|
|`"transtype"` (*optional*)|a \ref transtype_map_node|

## transtype map {#transtype_map_node}
A *transtype map* can be used to transtype between C and Fortran MPI communicators.
It is a mapping that in key has the name of data with communicator to transtype
and in value the name of data where to write transtyped communicator.

Example:

```yaml
data:
  mpi_comm: MPI_Comm
  mpi_comm_f: MPI_Comm_f
plugins:
  mpi:
    transtype:
      mpi_comm: m_mpi_comm_f
      mpi_comm_f: m_mpi_comm
```
