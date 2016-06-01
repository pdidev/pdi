PROGRAM example 

USE pdi
USE mpi


!#define VAL2D(arr, xx, yy) (arr[(xx)+width*(yy)])

INTEGER :: nb_iter,iter, width, height, pheight, pwidth, mpi_comm, status
TYPE(PC_tree_t_f) :: treetmp, tree1

call MPI_INIT(status)

mpi_comm = MPI_COMM_WORLD

nb_iter = 1
iter = 1

call PDI_expose("iter", iter)

call PC_parse_path("example.yml",tree1)

call PC_get(tree1,'.pdi',treetmp)


call PDI_init(treetmp,mpi_comm,status)

call PDI_share("test", nb_iter,1)
call PDI_reclaim("test")
nb_iter = 12
call PDI_import("iter", iter)
print *, "iter =", iter


call PDI_event("main_loop");

do iter =1,12
	nb_iter = nb_iter + iter
	call PDI_expose("iter", iter)
	call PDI_expose("test",nb_iter)
end do 
call PDI_release("test")
call PDI_event("end");

call PC_tree_destroy(tree1)

call PDI_finalize()

call MPI_FINALIZE(status)



END PROGRAM example



!mpif90 -g ../vendor/paraconf/src/paraconf.f90 ../vendor/paraconf/build/libparaconf.so ../src/pdi.f90 ../build/libpdi.so ../example/example.f90 -o example34