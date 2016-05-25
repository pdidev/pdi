PROGRAM example 

USE pdi
USE mpi


!#define VAL2D(arr, xx, yy) (arr[(xx)+width*(yy)])

INTEGER :: nb_iter, width, height, pheight, pwidth, mpi_comm, status
TYPE(PC_tree_t_f) :: treetmp, tree1

call MPI_INIT(status)

mpi_comm = MPI_COMM_WORLD

call PC_parse_path("example.yml",tree1)

call PC_get(tree1,'.pdi',treetmp)


call PDI_init(treetmp,mpi_comm,status)

print *, "PDI_init"

call PDI_finalize(status)

call MPI_FINALIZE(status)



END PROGRAM example



!gfortran -g ../vendor/paraconf/src/paraconf.f90 ../vendor/paraconf/build/libparaconf.so ../src/pdi.f90 ../build/libpdi.so ../example/example.f90 -o example34