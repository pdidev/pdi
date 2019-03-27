program mandelmpi
    use infostruct
    use mandelsion

    implicit none

    include "mpif.h"

    ! master-worker communication flags
    integer, parameter :: STATE_MASTER = -1
    integer, parameter :: STATE_IDLE = 0
    integer, parameter :: STATE_WORK = 1
    integer, parameter :: TAG_WORK = 27
    integer, parameter :: TAG_DONE = 28
    integer, parameter :: TAG_DATA = 29
    integer, parameter :: TAG_FINISH = 30

    real :: xmin = -1.5 ! coordinates of rectangle
    real :: xmax = 0.5
    real :: ymin = -1.0
    real :: ymax = 1.0
    integer :: width = 256 ! size of rectangle in pixels
    integer :: height = 256
    integer :: maxiter = 256 ! max. number of iterations
    logical :: verbose = .false. ! per default only print error messages
    integer :: type = 0 ! type of calculation: 0=stride, 1=static, 2=blockmaster
    integer, dimension(2) :: blocksize = (/ 64, 64 /) ! personal procs blocksize
    integer, dimension(2) :: start = (/ 0, 0 /) ! personal start index (type = 1)
    integer, dimension(:,:), allocatable :: blocksizes ! all procs blocksizes (type = 1)
    integer, dimension(:,:), allocatable :: starts ! all procs starts (type = 1)
    integer, dimension(2) :: procs = (/ 0, 0 /) ! procs distribution (type = 1)

    integer, dimension(:), allocatable :: iterations ! data array
    logical :: check_ok = .true. ! size check

    integer :: nargs
    character(len=20) :: arg

    integer :: numprocs, myid ! MPI information
    integer :: ierror
    integer :: i

    ! time measurement
    double precision :: st, sta, runtime, calctime = 0.0, commtime = 0.0, waittime = 0.0, iotime = 0.0

    integer :: starty
    real :: lxmin, lxmax, dx
    real :: lymin, lymax, dy
    integer :: array_size
    integer :: master_worker_commtype
    integer :: io_id

    type(t_infostruct) :: info

    ! parse command line
    nargs = iargc()
    i = 1
    do while (i <= nargs)
        call getarg(i, arg)
        if (arg(1:1) == "-") then
            select case (arg(2:2))
                case ("x")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) xmin
                    call getarg(i+2,arg)
                    read(unit=arg,fmt=*) xmax
                    call getarg(i+3,arg)
                    read(unit=arg,fmt=*) ymin
                    call getarg(i+4,arg)
                    read(unit=arg,fmt=*) ymax
                    i = i + 4
                case ("i")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) maxiter
                    i = i + 1
                case ("w")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) width
                    i = i + 1
                case ("h")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) height
                    i = i + 1
                case ("t")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) type
                    i = i + 1
                case ("b")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) blocksize(1)
                    blocksize(2) = blocksize(1)
                    i = i + 1
                case ("v")
                    verbose = .true.
                case ("p")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) procs(1)
                    i = i + 1
                case ("q")
                    call getarg(i+1,arg)
                    read(unit=arg,fmt=*) procs(2)
                    i = i + 1
                case default
                    write(unit=0, fmt=*) arg
                    call getarg(0,arg)
                    call usage(arg)
                    stop 1
            end select
        else
            write(unit=0, fmt=*) arg
            call getarg(0,arg)
            call usage(arg)
            stop 1
        end if
        i = i + 1
    end do

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierror)

    if (type == 0) then
        blocksize(1) = width
        start(2) = myid * blocksize(2);
    else if (type == 1) then
        call MPI_Dims_create(numprocs,2,procs,ierror)
        allocate(blocksizes(2,numprocs), starts(2,numprocs))
        starts(1,1) = 0
        starts(2,1) = 0
        do i = 1, numprocs
            ! calculate blocksizes
            blocksizes(1,i) = width / procs(1)
            if (modulo(i,procs(1)) < modulo(width,procs(1))) then
                blocksizes(1,i) = blocksizes(1,i) + 1
            end if
            blocksizes(2,i) = height / procs(2)
            if (modulo(i,procs(2)) < modulo(height,procs(2))) then
                blocksizes(2,i) = blocksizes(2,i) + 1
            end if
            ! calculate startpoints
            if (i > 1) then
                starts(1,i) = starts(1,i-1) + blocksizes(1,i-1)
                if (starts(1,i) >= width) then
                    starts(1,i) = 0
                    starts(2,i) = starts(2,i-1) + blocksizes(2,i-1)
                else
                    starts(2,i) = starts(2,i-1)
                end if
            end if
        end do
        blocksize = blocksizes(:,myid+1)
        start = starts(:,myid+1)
        deallocate(blocksizes,starts)
    end if

    ! check sizes
    if (type /= 1) then
        check_ok = modulo(width,blocksize(1)) == 0
        if (check_ok) then
            check_ok = modulo(height,blocksize(2)) == 0
        end if
    end if
    if (.not. check_ok .and. myid == 0) then
        write(unit=0, fmt=*) "Blocksize, width and height are not compatible!"
        stop 1
    end if

    ! fill infostruct
    info%type = type
    info%width = width
    info%height = height
    info%numprocs = numprocs
    info%xmin = xmin
    info%xmax = xmax
    info%ymin = ymin
    info%ymax = ymax
    info%maxiter = maxiter

    ! print info header
    if (verbose .and. myid == 0) then
        write(unit=*, fmt="(a,4(g10.3,a))") "start calculation (x=", xmin, "..", xmax, "y=", ymin, "..", ymax, ")"
    end if

    sta = MPI_Wtime()

    ! IO preparation
    array_size = blocksize(1) * blocksize(2)
    if (type == 3 .and. myid == 0) then
        array_size = width * height ! proc 0 will collect all data
    end if

    ! initialize array
    allocate(iterations(array_size))

    st = MPI_Wtime()
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    waittime = waittime + (MPI_Wtime() - st)

    ! open file
    call open_file(io_id, info, blocksize, start, myid, iotime)

    !**************************** Type 0 Stride *******************************

    if (type == 0) then
        dy = (ymax - ymin) / height
        if (verbose) then
            write(unit=*,fmt="(a,i3.2,a,i5,a,i5)") "calc_stride[", myid, "]: ", blocksize(1), " x ", blocksize(2)
        end if
        do starty = start(2), height - 1, numprocs * blocksize(2)
            lymin = ymin + starty * dy
            lymax = lymin + blocksize(2) * dy
            call calc(iterations, blocksize(1), blocksize(2), xmin, xmax, lymin, lymax, maxiter, calctime)
            call write_to_file(io_id, info, iterations, blocksize(1), blocksize(2), start(1), starty, iotime)
        end do

        ! call write routine using empty dataset to allow collectiv operations
        if ((modulo(height / blocksize(2),numprocs) > 0) .and. (myid >= modulo(height / blocksize(2),numprocs))) then
            call write_to_file(io_id, info, iterations, 0, 0, start(1), starty, iotime);
        end if

    end if

    !**************************** Type 1 Static *******************************

    if (type == 1) then
        ! Calculate blocks
        dx = (xmax - xmin) / width
        dy = (ymax - ymin) / height
        lxmin = xmin + start(1) * dx
        lxmax = lxmin + blocksize(1) * dx
        lymin = ymin + start(2) * dy
        lymax = lymin + blocksize(2) * dy

        if (verbose) then
            write(unit=*,fmt="(a,i3.2,a,i5,a,i5)") "calc_static[", myid, "]: ", blocksize(1), " x ", blocksize(2)
        end if
        call calc(iterations, blocksize(1), blocksize(2), lxmin, lxmax, lymin, lymax, maxiter, calctime)

        ! additional barrier because of collective write
        st = MPI_Wtime()
        call MPI_Barrier(MPI_COMM_WORLD, ierror)
        waittime = waittime + (MPI_Wtime() - st)

        call write_to_file(io_id, info, iterations, blocksize(1), blocksize(2), start(1), start(2), iotime)
    end if

    !**************************** Type 2 Blockmaster **************************

    if (type == 2 .or. type == 3) then
        if (numprocs > 1) then
            call MPI_Type_vector(blocksize(1), blocksize(2), width, MPI_INT, master_worker_commtype, ierror)
            call MPI_Type_commit(master_worker_commtype, ierror)
            if (myid == 0) then
                if (verbose) then
                    write(unit=*, fmt='(a,i3.2,a,i5,a,i5)') "calc_master[", myid, "]: ", width, "x", height
                end if
                    call calc_master(iterations, width, height, numprocs, blocksize, type == 3, calctime, &
                                   & master_worker_commtype, commtime)
                if (type == 3) then
                    call write_to_file(io_id, info, iterations, width, height, 0, 0, iotime)
                end if
            else
                if (verbose) then
                    write(unit=*, fmt='(a,i3.2,a,i5,a,i5)') "calc_worker[", myid, "]: ", blocksize(1), "x", blocksize(2)
                end if
                call calc_worker(iterations, io_id, info, type == 3, iotime, calctime, commtime)
            end if
            call MPI_Type_free(master_worker_commtype, ierror)
        else
            write(unit=0, fmt=*) "ERROR: type 'blockmaster' needs at least two processes"
        end if
    end if

    !**************************************************************************

    st = MPI_Wtime()
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    waittime = waittime + (MPI_Wtime() - st)

    ! close file
    call close_file(io_id, info, myid, iotime)

    runtime = MPI_Wtime() - sta

    if (verbose) then
        write(unit=*,fmt='(a,i4.2,a,i4.2,a,i1,1x,i5,a,i5,5(a,f9.3))') "PE ", myid, " of ", numprocs, &
           & ": t= ", type, width, " x " , height, &
           & " calc=", calctime * 1000, ", wait=", waittime * 1000, ", io=", iotime * 1000, &
           & ", mpi=", commtime * 1000, ", runtime=", runtime * 1000
    end if

    deallocate(iterations)

    call MPI_Finalize(ierror)

contains
    subroutine usage(name)
        character(len=20), intent(in) :: name

        write(unit=0, fmt='(A,A,A,/)') "Usage: ", trim(name), " options"
        write(unit=0, fmt=*) "with the following optional options default values in parenthesis):"
        write(unit=0, fmt=*) "  [-x <x0> <x1> <y0> <y1>]  coordinates of initial area (-1.5 0.5 -1.0 1.0)"
        write(unit=0, fmt=*) "  [-w <width>]              image width in pixels (256)"
        write(unit=0, fmt=*) "  [-h <height>]             image height in pixels (256)"
        write(unit=0, fmt=*) "  [-i <maxiter>]            max. number of iterations per pixel (256)"
        write(unit=0, fmt=*) "  [-b <blocksize>]          blocksize used for strides and blockmaster"
        write(unit=0, fmt=*) "  [-p <xprocs>]             #xprocs for type = blocks"
        write(unit=0, fmt=*) "  [-q <yprocs>]             #yprocs for type = blocks"
        write(unit=0, fmt=*) "  [-t <type>]               0=stride, 1=static, 2=blockmaster, 3=single writer"
        write(unit=0, fmt='(A,/)') "   [-v]                      verbose (off)"
    end subroutine usage

    ! Mandelbrot calculation
    subroutine calc(iterations, width, height, xmin, xmax, ymin, ymax, maxiter, calctime)
        integer, dimension(:), intent(inout) :: iterations
        integer, intent(in) :: width
        integer, intent(in) :: height
        real, intent(in) :: xmin, xmax, ymin, ymax
        integer, intent(in) :: maxiter
        double precision, intent(inout) :: calctime

        real :: x, y
        real :: dx, dy
        integer :: ix, iy
        real :: zx, zy, zxnew
        integer :: count
        double precision :: st

        st = MPI_Wtime()

        dx = (xmax - xmin) / width
        dy = (ymax - ymin) / height

        !* calculate value in the center of the pixel
        y = ymin + 0.5 * dy
        do iy = 0, height - 1
            x = xmin + 0.5 * dx
            do ix = 0, width - 1
                zx = 0.0
                zy = 0.0
                count = 0
                do while (zx * zx + zy * zy < 16 * 16 .and. count < maxiter)
                    zxnew = zx * zx - zy * zy + x
                    zy = 2 * zx * zy + y
                    zx = zxnew
                    count = count + 1
                end do
                iterations(iy * width + ix + 1) = count
                x = x + dx
            end do
            y = y + dy
        end do
        calctime = calctime + (MPI_Wtime() - st)
    end subroutine calc

    ! master task (type=2 || type=3)
    subroutine calc_master(iterations, width, height, numprocs, blocksize, collect, calctime, commtype, commtime)
        integer, dimension(:), intent(inout) :: iterations
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(in) :: numprocs
        integer, dimension(2), intent(in) :: blocksize
        logical, intent(in) :: collect
        double precision, intent(inout) :: calctime
        integer, intent(in) :: commtype
        double precision, intent(inout) :: commtime

        double precision :: st, stc
        integer :: ix, iy, i
        integer :: lwidth, lheight
        integer :: workeratwork = 0, numworkers
        integer, dimension(:), allocatable :: workerstat
        integer, dimension(4) :: work
        integer :: ierror
        integer, dimension(MPI_STATUS_SIZE) :: mpi_status, mpi_status2

        numworkers = numprocs - 1
        commtime = 0.0

        allocate(workerstat(numprocs))

        workerstat(1) = STATE_MASTER
        do i = 2, numprocs
            workerstat(i) = STATE_IDLE
        end do

        stc = MPI_Wtime()
        ix = 0
        iy = 0
        i = 0

        do while ((iy < height) .and. (ix < width))
            ! calculate blocksize
            lwidth = blocksize(1)
            lheight = blocksize(2)
            if (ix + lwidth > width) then
                lwidth = width - ix
            end if
            if (iy + lheight > height) then
                lheight = height - iy
            end if

            if (workeratwork < numworkers) then
                do while (workerstat(i+1) /= STATE_IDLE)
                    i = modulo(i+1,numprocs)
                end do
                !  send work to worker #i
                work(1) = ix
                work(2) = iy
                work(3) = lwidth
                work(4) = lheight
                st = MPI_Wtime()
                call MPI_Send(work, 4, MPI_INT, i, TAG_WORK, MPI_COMM_WORLD, ierror)
                commtime = commtime + (MPI_Wtime() - st)
                workerstat(i+1) = STATE_WORK
                workeratwork = workeratwork + 1

                ! calculate location of next block
                ix = ix + lwidth
                if (ix >= width) then
                    ix = 0
                    iy = iy + lheight
                end if
            else
                !  collect result msg
                st = MPI_Wtime()
                call MPI_Recv(work, 4, MPI_INT, MPI_ANY_SOURCE, TAG_DONE, MPI_COMM_WORLD, mpi_status, ierror)
                if (collect) then
                    call MPI_Recv(iterations(work(2) * width + work(1) + 1), 1, commtype, mpi_status(MPI_SOURCE), &
                                & TAG_DATA, MPI_COMM_WORLD, mpi_status2, ierror)
                end if
                commtime = commtime + (MPI_Wtime() - st)
                workerstat(mpi_status(MPI_SOURCE) + 1) = STATE_IDLE
                workeratwork = workeratwork-1
            end if
        end do

        !  get rest of result msg
        do while (workeratwork > 0)
            st = MPI_Wtime()
            call MPI_Recv(work, 4, MPI_INT, MPI_ANY_SOURCE, TAG_DONE, MPI_COMM_WORLD, mpi_status, ierror)
            if (collect) then
                call MPI_Recv(iterations(work(2) * width + work(1) + 1), 1, commtype, mpi_status(MPI_SOURCE), &
                            & TAG_DATA, MPI_COMM_WORLD, mpi_status2, ierror)
            end if
            commtime = commtime + (MPI_Wtime() - st)
            workerstat(mpi_status(MPI_SOURCE)+1) = STATE_IDLE
            workeratwork = workeratwork - 1
        end do

        !  send finish message
        do i= 1, numprocs - 1
            work(1) = -1
            work(2) = -1
            work(3) = -1
            work(4) = -1
            st = MPI_Wtime()
            call MPI_Send(work, 4, MPI_INT, i, TAG_FINISH, MPI_COMM_WORLD, ierror)
            commtime = commtime + (MPI_Wtime() - st)
        end do
        calctime = calctime + (MPI_Wtime() - stc - commtime)

        deallocate(workerstat)
    end subroutine calc_master


    ! worker task (type=2 || type=3)
    subroutine calc_worker(iterations, io_id, info, collect, iotime, calctime, commtime)
        integer, dimension(:), intent(inout) :: iterations
        integer, intent(inout) :: io_id
        type(t_infostruct), intent(in) :: info
        logical, intent(in) :: collect
        double precision, intent(inout) :: iotime
        double precision, intent(inout) :: calctime
        double precision, intent(inout) :: commtime

        real :: dx, dy
        double precision :: st
        logical :: running = .true.
        integer :: xpos, ypos, lwidth, lheight
        real lxmin, lxmax, lymin, lymax
        integer, dimension(4) :: work
        integer :: ierror
        integer, dimension(MPI_STATUS_SIZE) :: mpi_status

        dx = (info%xmax - info%xmin) / info%width
        dy = (info%ymax - info%ymin) / info%height

        do while (running)
            st = MPI_Wtime()
            ! recv new work from master
            call MPI_Recv(work, 4, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_status, ierror)
            commtime = commtime + (MPI_Wtime() - st)
            if (mpi_status(MPI_TAG) == TAG_FINISH) then
                running = .false.
            end if

            if (running) then
                xpos = work(1)
                ypos = work(2)
                lwidth = work(3)
                lheight = work(4)

                lxmin = info%xmin + xpos * dx
                lxmax = lxmin + lwidth * dx
                lymin = info%ymin + ypos * dy
                lymax = lymin + lheight * dy

                call calc(iterations, lwidth, lheight, lxmin, lxmax, lymin, lymax, info%maxiter, calctime)

                if (.not. collect) then
                    call write_to_file(io_id, info, iterations, lwidth, lheight, xpos, ypos, iotime)
                end if

                st = MPI_Wtime()
                call MPI_Send(work, 4, MPI_INT, 0, TAG_DONE, MPI_COMM_WORLD, ierror)
                if (collect) then
                    call MPI_Send(iterations, lwidth * lheight, MPI_INT, 0, TAG_DATA, MPI_COMM_WORLD, ierror)
                end if
                commtime = commtime + (MPI_Wtime() - st)
            end if
        end do
    end subroutine calc_worker

    ! open file  */
    subroutine open_file(io_id, info, blocksize, start, myid, iotime)
        integer, intent(inout) :: io_id
        type (t_infostruct), intent(in) :: info
        integer, dimension(2), intent(in) :: blocksize
        integer, dimension(2), intent(in) :: start
        integer, intent(in) :: myid
        double precision, intent(inout) :: iotime
        double precision :: st

        st = MPI_Wtime()

        call open_sion(io_id, info, blocksize, start, myid)

        iotime = iotime + (MPI_Wtime() - st)
   end subroutine open_file

   ! write to file
   subroutine write_to_file(io_id, info, iterations, width, height, xpos, ypos, iotime)
        integer, intent(inout) :: io_id
        type (t_infostruct), intent(in) :: info
        integer, dimension(:), intent(inout) :: iterations
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(in) :: xpos
        integer, intent(in) :: ypos
        double precision, intent(inout) :: iotime
        double precision :: st

        st = MPI_Wtime()

        call write_to_sion_file(io_id, info, iterations, width, height, xpos, ypos)

        iotime = iotime + (MPI_Wtime() - st)
   end subroutine write_to_file

   ! close file
   subroutine close_file(io_id, info, myid, iotime)
        integer, intent(inout) :: io_id
        type (t_infostruct), intent(in) :: info
        integer, intent(in) :: myid
        double precision, intent(inout) :: iotime
        double precision :: st

        st = MPI_Wtime()

        call close_sion(io_id, info, myid)

        iotime = iotime + (MPI_Wtime() - st)
   end subroutine close_file

end program mandelmpi
