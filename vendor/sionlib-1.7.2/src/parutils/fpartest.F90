!/****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2018                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!****************************************************************************/
program main

#define MB *1024*1024
#define MAXPE 64*1024
#define MAXCHARLEN 256
#define sion_int64 integer*8

!Enable or disable the checksum
!#define CHECKSUM

!*****************************************************************************80
!
!! The main program that will execute the necessary tests
!
        implicit none
!
        include 'mpif.h'


        integer ierr, np, rank, rankl
        integer fsblksize
        integer*8 totalsize, chunksize,bufsize
        character, dimension(:),pointer :: localbuffer
        character(len=MAXCHARLEN) :: filename = 'test_sionfile.sion'
        character(len=MAXCHARLEN) :: newfname = 'newfile'

        integer   :: narg, nfiles
        character(len=256) argv

        logical collectiveread

        double precision gstarttime,starttime,opentime,gwritetime,writetime,closetime,readtime,greadtime
        double precision barr1time,barr2time,barr3time

        integer*8 bsumwrote,sumsize,bsumread, left

        real*8  checksum_fp, checksum_read_fp
        integer globalrank, sid, i

        integer*8 bwrite,bwrote,btoread,bread
        integer feof

        integer   chunkcnt,size1
        integer*8 rchunksize
        integer   rblocksize
!       character(len=MAXCHARLEN) cbuffer

        integer comm,gComm,lComm

        call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

        bufsize   = 10 MB
        totalsize = 20 MB
        chunksize = bufsize
        fsblksize = 2 MB        ! IBM GPFS
        nfiles = 1

        collectiveread = .TRUE.

        if(rank == 0) then
!       parse command line
            i=1
        narg = COMMAND_ARGUMENT_COUNT()
            do while( i < narg )
              call GET_COMMAND_ARGUMENT(i, argv)

              if( argv(:1) == '-' ) then

                        select case( argv(2:) )
                                case ('f')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  filename = trim(argv)
                                case ('F')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)

                                case ('b')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') bufsize
                                case ('B')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') bufsize
                                  bufsize = bufsize MB
                                case ('s')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') totalsize
                                case ('n')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') nfiles
                                case ('S')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') totalsize
                                  totalsize = totalsize MB
                                case ('r')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') chunksize
                                case ('R')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') chunksize
                                  chunksize = chunksize MB
                                case ('q')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') fsblksize
                                case ('Q')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  read(argv,'(I20)') fsblksize
                                  fsblksize = fsblksize MB
                                case ('T')
                                  i = i+1
                                  call GET_COMMAND_ARGUMENT(i, argv)
                                  if (argv /= '0') then
                                        collectiveread = .TRUE.
                                  else
                                        collectiveread = .FALSE.
                                  end if
                                case default
                                  call GET_COMMAND_ARGUMENT(0, argv)
                                  call usage(argv)
                                  call exit(1)
                        end select
                  else
                        call GET_COMMAND_ARGUMENT(0, argv)
                        call usage(argv)
                        call exit(1)
                  end if
                  i = i+1
                end do
        end if

        call MPI_Bcast(filename,MAXCHARLEN,MPI_CHARACTER,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(bufsize,1,MPI_INTEGER4,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(totalsize,1,MPI_INTEGER8,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(chunksize,1,MPI_INTEGER8,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(fsblksize,1,MPI_INTEGER,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(collectiveread,1,MPI_LOGICAL,0,MPI_COMM_WORLD, ierr)
        call MPI_Bcast(nfiles,1,MPI_INTEGER,0,MPI_COMM_WORLD, ierr)


    if (rank == 0) then
#ifndef CHECKSUM
    write(0, '(A)') "partest parameter:                 Checksum DISABLED!!"," "
#endif
        write(0, 100) "datafile", trim(filename)
        write(0, 103) "number of files",nfiles

        write(0, 101) "local buffer size / task", bufsize, bufsize/(1.0 MB)
        write(0, 101) "total data size / task", totalsize, totalsize/(1.0 MB)
        write(0, 101) "sion chunk size", chunksize, chunksize/(1.0 MB)
        write(0, 101) "fs block size", fsblksize, fsblksize/(1.0 MB)
        write(0, 102) "Collective", collectiveread


100     format("partest parameter:      ",A30,TR1,"             = ",A)
101     format("partest parameter:      ",A30,TR1,"             = ",I10," bytes (",F10.4," MB)")
102     format("partest parameter:      ",A30,TR1,"             = ",L5)
103     format("partest parameter:      ",A30,TR1,"             = ",I10)

    end if

    call barrier_after_start(MPI_COMM_WORLD)

    allocate( localbuffer(bufsize) )
    localbuffer(:) = achar(ichar('a')+rank)

    call barrier_after_malloc(MPI_COMM_WORLD)

    size1 = np
    comm = MPI_COMM_WORLD
!    lcomm = MPI_COMM_WORLD
    gComm = comm
    globalrank = rank

!  ****************************** WRITE *****************************
! TODO: REMOVE TRIM
    starttime = MPI_Wtime()
    call fsion_paropen_mpi(trim(filename),'bw',nfiles, gComm,lComm,chunksize,fsblksize,globalrank,newfname,sid)

    if (rank == 0) then
       write(0, 101) "fs block size (calc)", fsblksize, fsblksize/(1.0 MB)
    end if
    
    opentime = MPI_Wtime() - starttime
    
    call MPI_COMM_RANK(lComm, rankl, ierr)

!    write(0, "(A30,I20)") "WF: after open", rank

    starttime = MPI_Wtime()
    call barrier_after_open(lComm)
    barr1time = MPI_Wtime()-starttime

    checksum_fp = 0
    left = totalsize
    bsumwrote = 0
    chunkcnt = 0

    starttime = MPI_Wtime()
    gstarttime = starttime
    ! Fortran 90 specific!
        do while(left > 0)
                bwrite = bufsize
                if (bwrite > left) bwrite = left

                call fsion_ensure_free_space(sid,bwrite,ierr)
                call fsion_write(localbuffer, 1, bwrite, sid, bwrote)
!                write(0, "(A30,I2,I10,I10)") "WF: after write", rank, bwrite,bwrote

#ifdef CHECKSUM
                do i=1,bwrote
                        checksum_fp = checksum_fp + real(IACHAR(localbuffer(i)))
                end do
#endif

                left = left - bwrote
        bsumwrote = bsumwrote + bwrote
        chunkcnt = chunkcnt + 1

!               if (rank == 0) write(0, *) 'Chunk ',chunkcnt,': Wrote ', bwrote,'bytes data! Left: ',left
        end do

        writetime = MPI_Wtime() - starttime

        starttime = MPI_Wtime()
        call barrier_after_write(lComm)
        barr2time = MPI_Wtime() - starttime
        gwritetime = MPI_Wtime() - gstarttime

        starttime = MPI_Wtime()
        call fsion_parclose_mpi(sid,ierr)
        closetime = MPI_Wtime() - starttime

        starttime = MPI_Wtime()
        call barrier_after_close(lComm)
        barr3time = MPI_Wtime()-starttime

        if (writetime == 0) writetime = -1

!       if(verbose) {
!    sprintf(cbuffer,"timings[%03d] open=%10.6fs write=%10.6fs close=%10.6fs
! barrier(open=%10.6fs, write=%10.6fs, close=%10.6fs) #chunks=%d bw=%10.4f MB/s ionode=%d\n",
!           rank1,opentime,writetime,closetime,
!           barr1time,barr2time,barr3time,chunkcnt,
!           totalsize/1024.0/1024.0/writetime,rank3);
!    collective_print_gather(cbuffer,comm1);
!  }

        if (nfiles > 1) then
        call MPI_REDUCE(bsumwrote, sumsize, 1, MPI_INTEGER8, MPI_SUM, 0, lComm, ierr)
        if(rankl == 0) then
                write (0,111) 1.0*sumsize/1024.0/1024.0, newfname
111             format("Write partest result: wrote ",F10.4," MB to >",A30,"<")
        end if
    end if

        call MPI_REDUCE(bsumwrote, sumsize, 1, MPI_INTEGER8, MPI_SUM, 0, gComm, ierr)
        call MPI_BARRIER(gComm,ierr)

        if (rank == 0) then
                write(0,'(A)') "-----------------------------------------------------------------------"
        write(0,112) 1.0*sumsize/1024.0/1024.0, gwritetime, 1.0*sumsize/1024.0/1024.0/gwritetime

112     format("TOTAL: Write partest result: wrote ",F10.4," MB write+barrier= ",F10.6,"s bw= ",F10.4," MB/s")
                write(0,'(A)') "-----------------------------------------------------------------------"
    end if

        if (nfiles > 1) then
          call MPI_BARRIER(lComm,ierr)
      call MPI_COMM_FREE(lComm,ierr)
    end if

    if (rank == 0) write(0,'(A)') "***********************************************************************"

!  ****************************** READ *****************************
        localbuffer(:) = ' '

        starttime = MPI_Wtime();
        if (collectiveread) then 
             call fsion_paropen_mpi(trim(filename),'br',nfiles, gComm,lComm,chunksize,fsblksize,globalrank,newfname,sid)
        else
             call fsion_open_rank(trim(filename),"br",rchunksize,rblocksize,rank,sid)
        end if

        opentime = MPI_Wtime()-starttime

        call MPI_COMM_RANK(lComm, rankl, ierr)

        starttime = MPI_Wtime()
        call barrier_after_open(lComm)
        barr1time = MPI_Wtime()-starttime

        starttime = MPI_Wtime()
        gstarttime = starttime

        checksum_read_fp = 0
        left = totalsize
        bsumread = 0
        chunkcnt = 0

    call fsion_feof(sid,feof)

        do while( (left > 0) .AND. (feof /= 1 ) )
            btoread=bufsize

        if( btoread>left ) btoread = left

        call fsion_read(localbuffer, 1, btoread, sid, bread)

#ifdef  CHECKSUM
                do i=1,bread
                        checksum_read_fp = checksum_read_fp + real(IACHAR(localbuffer(i)))
                end do
#endif

                left = left - bread
        bsumread = bsumread + bread
        chunkcnt = chunkcnt + 1
!               if (rank == 0) write(0, *) 'Chunk ',chunkcnt,': Read ', bread,'bytes data! Left: ',left

        call fsion_feof(sid,feof)
        end do

        readtime = MPI_Wtime()-starttime

        starttime = MPI_Wtime()
        call barrier_after_read(lComm)
        barr2time = MPI_Wtime()-starttime

        greadtime = MPI_Wtime()-gstarttime

        starttime = MPI_Wtime()
        if(collectiveread) then
        call fsion_parclose_mpi(sid,ierr)
        else
        call fsion_close(sid,ierr)
        end if
        closetime = MPI_Wtime()-starttime

        if(readtime == 0) readtime = -1

#ifdef  CHECKSUM
        if (abs(checksum_fp-checksum_read_fp)>1e-5) then
                    write(0,*)"ERROR in double checksum  ",checksum_fp,"!=",checksum_read_fp," diff=",(checksum_fp-checksum_read_fp)
    end if
#endif

        if (nfiles > 1) then
        call MPI_REDUCE(bsumread, sumsize, 1, MPI_INTEGER8, MPI_SUM, 0, lComm, ierr)
        if(rankl == 0) then
                write (0,113) 1.0*sumsize/1024.0/1024.0, newfname
113             format("Read partest result: read ",F10.4," MB from >",A30,"<")
        end if
    end if



        call MPI_REDUCE(bsumread, sumsize, 1, MPI_INTEGER8, MPI_SUM, 0, gComm, ierr)
        call MPI_BARRIER(gComm,ierr)

        if (rank == 0) then
                write(0,'(A)') "-----------------------------------------------------------------------"
        write(0,114) 1.0*sumsize/1024.0/1024.0, greadtime, 1.0*sumsize/1024.0/1024.0/greadtime

114     format("TOTAL: Read partest result: read ",F10.4," MB read+barrier= ",F10.6,"s bw= ",F10.4," MB/s")
                write(0,'(A)') "-----------------------------------------------------------------------"
    end if

        if (nfiles > 1) then
          call MPI_BARRIER(lComm,ierr)
      call MPI_COMM_FREE(lComm,ierr)
    end if
!  ****************************** END *****************************
        deallocate(localbuffer)

        call MPI_Finalize(ierr)
        stop
end

subroutine barrier_after_start(comm)
  integer, intent(in) :: comm
  integer ierr
  call MPI_BARRIER(comm,ierr)
end

subroutine barrier_after_malloc(comm)
  integer, intent(in) :: comm
  integer ierr

  call MPI_BARRIER(comm,ierr)
end

subroutine barrier_after_open(comm)
        integer, intent(in) :: comm
        integer ierr
        call MPI_BARRIER(comm,ierr)
return
end

subroutine barrier_after_write(comm)
        integer, intent(in) :: comm
        integer ierr
        call MPI_BARRIER(comm,ierr)
end

subroutine barrier_after_read(comm)
        integer, intent(in) :: comm
        integer ierr
        call MPI_BARRIER(comm,ierr)
end

subroutine barrier_after_close(comm)
        integer, intent(in) :: comm
        integer ierr
        call MPI_BARRIER(comm,ierr)
end

subroutine collective_print_gather ( cbuffer, comm)
        character, dimension(:), intent(in) :: cbuffer
        integer, intent(in) :: comm

        integer rank, np, p, ierr
        character, dimension(:), allocatable :: lbuffer

        ! Allocate the memory for this array
        allocate( lbuffer(MAXCHARLEN*MAXPE) )

        call MPI_COMM_SIZE(comm, np, ierr)
        call MPI_COMM_RANK(comm, rank, ierr)

        call MPI_GATHER(cbuffer, MAXCHARLEN, MPI_CHARACTER, lbuffer, MAXCHARLEN, MPI_CHARACTER, 0, comm, ierr)

        if(rank .EQ. 0) then
                do p = 0, np
                        write(0,*) lbuffer( (p*MAXCHARLEN+1):((p+1)*MAXCHARLEN) )
                end do
        end if

        ! Free the memory of this array
        deallocate(lbuffer)
end

subroutine usage(name)
        character(len=64), intent(in) :: name

        write(0, '(A,A,A,/,A)') "Usage: ",TRIM(name)," <options>",&
        "with the following optional options (default values in parathesis)"
        write(0, '(/,A)') " Sion File Settings:"
    write(0, *) "  [-f filename]          filename of direct access file"
    write(0, *) "  [-r <chunksize>]       sion chunk size in bytes"
    write(0, *) "  [-R <chunksize>]       sion chunk size in MBytes"
    write(0, *) "  [-q <fsblksize>]       size of filesystem blocks in bytes"
    write(0, *) "  [-Q <fsblksize>]       size of filesystem blocks in MBytes"

    write(0, *) "  [-b <bufsize>]         size of blocks written in ONE fwrite in bytes"
    write(0, *) "  [-B <bufsize>]         size of blocks written in ONE fwrite in MBytes"
    write(0, *) "  [-s <totalsize>]       total size of data written by each processor in bytes"
    write(0, *) "  [-S <totalsize>]       total size of data written by each processor in MBytes"
    write(0, *) "  [-T <type>]            type of test (0): w/o collective read "

end

