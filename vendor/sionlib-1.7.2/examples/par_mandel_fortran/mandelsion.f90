module mandelsion
    use infostruct

    use sion_f90
    use sion_f90_mpi

    implicit none

    include "mpif.h"

    private

    public :: open_sion, close_sion, write_to_sion_file, collect_sion

contains
    ! open SION file
    subroutine open_sion(sid, info, blocksize, start, rank)
        integer, intent(out) :: sid
        type(t_infostruct), intent(in) :: info
        integer, dimension(2), intent(in) :: blocksize
        integer, dimension(2), intent(in) :: start
        integer, intent(in) :: rank

        integer :: numFiles = 1
        integer(kind=4) :: fsblksize = -1
        character(len=2) :: filemode = "wb"
        character(len=250) :: newfname
        integer :: lComm
        integer(kind=8) :: chunksize
        integer(kind=8) :: bwrote


        type(t_pos_struct) :: dummy_pos_struct
        integer :: dummy_integer
        integer :: storage_size
        character(kind=1), dimension(:), allocatable :: storage

        storage_size = size(transfer(dummy_pos_struct,storage))

        chunksize = blocksize(1) * blocksize(2) * kind(dummy_integer) + storage_size

        if (rank == 0) then
            if (info%type == 2) then
                chunksize = 0
            end if
            if (info%type == 3) then
                chunksize = info%width * info%height * kind(dummy_integer) + storage_size
            end if
            chunksize = chunksize + size(transfer(info,storage))
        else
            if (info%type == 3) then
                chunksize = 0
            end if
        end if


        call fsion_paropen_mpi("simple.sion", filemode, numFiles, MPI_COMM_WORLD, lComm, &
                             & chunksize, fsblksize, rank, newfname, sid)

        ! write global file header
        if (rank == 0) then
            storage_size = size(transfer(info,storage))
            allocate(storage(storage_size))
            storage = transfer(info,storage)
            call fsion_write(storage(1), 1_8, int(storage_size,8), sid, bwrote)
            deallocate(storage)
        end if
    end subroutine open_sion

    ! write block to SION file
    subroutine write_to_sion_file(sid, info, iterations, width, height, xpos, ypos)
        integer, intent(in) :: sid
        type(t_infostruct), intent(in) :: info
        integer, dimension(:), intent(in) :: iterations
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(in) :: xpos
        integer, intent(in) :: ypos

        type(t_pos_struct) pos_struct
        integer(kind=8) :: bwrote
        integer :: storage_size
        character(kind=1), dimension(:), allocatable :: storage

        if (width * height > 0) then
            pos_struct%width = width
            pos_struct%height = height
            pos_struct%xpos = xpos
            pos_struct%ypos = ypos

            storage_size = size(transfer(pos_struct,storage))
            allocate(storage(storage_size))
            storage = transfer(pos_struct,storage)

            call fsion_write(storage(1), 1_8, int(storage_size,8), sid, bwrote)
            call fsion_write(iterations(1), int(kind(iterations(1)),8), int(width * height,8), sid, bwrote)
            deallocate(storage)
        end if
    end subroutine write_to_sion_file

    ! close SION file
    subroutine close_sion(sid, info, rank)
        integer, intent(inout) :: sid
        type(t_infostruct), intent(in) :: info
        integer, intent(in) :: rank
        integer :: ierr


        call fsion_parclose_mpi(sid,ierr)
    end subroutine close_sion


    ! read SION file
    subroutine collect_sion(iterations, proc_distribution, info)
        integer, dimension(:), pointer :: iterations
        integer, dimension(:), pointer :: proc_distribution
        type(t_infostruct), intent(inout) :: info

        character(len=2) :: filemode = "rb"
        integer :: ntasks = 1
        integer :: nfiles = 1
        integer(kind=8) :: chunksizes
        integer(kind=4) :: fsblksize = -1
        integer :: globalranks
        integer :: outsid

        character(kind=1), dimension(:), allocatable :: storage
        integer :: array_size, pos_struct_size
        integer :: ierr
        integer(kind=8) :: rc

        integer :: start_task, task = 0
        integer :: eof
        type(t_pos_struct) :: pos_struct
        integer, dimension(:), allocatable :: buffer
        integer :: i = 0
        integer :: xpos, ypos

        ! open SION file
        call fsion_open("simple.sion", filemode, ntasks, nfiles, chunksizes, fsblksize, globalranks, outsid)

        ! read global header
        array_size = size(transfer(info,storage))
        allocate(storage(array_size))
        call fsion_seek(outsid, 0, SION_CURRENT_BLK, int(SION_CURRENT_POS,8),ierr)
        call fsion_read(storage(1), 1_8, int(array_size,8), outsid, rc)
        info = transfer(storage,info)
        deallocate(storage)

        array_size = info%width * info%height
        allocate(iterations(array_size))
        allocate(proc_distribution(array_size))

        ! ignore Master-task when using master-worker scheme
        if (info%type == 2) then
            start_task = 1
        else
            start_task = 0
        end if

        pos_struct_size = size(transfer(pos_struct,storage))
        allocate(storage(pos_struct_size))

        do task = start_task, ntasks - 1
            call fsion_seek(outsid, task, SION_CURRENT_BLK, int(SION_CURRENT_POS,8),ierr)
            call fsion_feof(outsid, eof)
            do while (eof == 0)
                !read info header
                call fsion_read(storage(1), 1_8, int(pos_struct_size,8), outsid, rc)
                pos_struct = transfer(storage,pos_struct)

                ! read data
                array_size = pos_struct%width * pos_struct%height
                allocate(buffer(array_size))
                call fsion_read(buffer(1), int(kind(buffer(1)),8), int(array_size,8), outsid, rc)

                i = 1
                do ypos = pos_struct%ypos, pos_struct%ypos + pos_struct%height - 1
                    do xpos = pos_struct%xpos, pos_struct%xpos + pos_struct%width - 1
                        iterations(ypos*info%width + xpos + 1) = buffer(i)
                        i = i + 1
                        proc_distribution(ypos*info%width + xpos + 1) = task
                    end do
                end do
                deallocate(buffer)
                call fsion_feof(outsid, eof)
            end do
        end do

        deallocate(storage)

        ! close SION file
        call fsion_close(outsid,ierr)
    end subroutine collect_sion
end module mandelsion
