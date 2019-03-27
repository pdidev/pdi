program main

  !*****************************************************************************80
  !
  !! The main program that will execute the necessary tests
  !
  use sion_f90
  implicit none

  character(len=1024) :: filename
  character(len=1024) :: ofilename

  integer   :: narg
  character(len=1024) :: argv

  logical :: do_ascii_to_sion
  logical :: do_sion_to_ascii

  integer :: i

  i = 1;
  do_sion_to_ascii = .FALSE.
  do_ascii_to_sion = .FALSE.

  narg = COMMAND_ARGUMENT_COUNT()
  write (*, *) "number of arguments: ", narg
  do while( i < narg )
     call GET_COMMAND_ARGUMENT(i, argv)
     if( argv(:1) == '-' ) then

        select case( argv(2:) )
        case ('f')
           i = i + 1
           call GET_COMMAND_ARGUMENT(i, argv)
           filename = trim(argv)
        case ('o')
           i = i + 1
           call GET_COMMAND_ARGUMENT(i, argv)
           ofilename = trim(argv)
        case ('S')
           do_ascii_to_sion = .TRUE.
        case ('A')
           do_sion_to_ascii = .TRUE.
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
     i = i + 1
  end do

  if(do_ascii_to_sion) then
     call mp2c_ascii_sion(filename)
  end if

  if(do_sion_to_ascii) then
     call mp2c_sion_ascii(filename, ofilename)
  end if

  stop
end program main


subroutine mp2c_sion_ascii(filename, ofilename)
  use sion_f90
  implicit none
  character(len=1024), intent(in) :: filename
  character(len=1024), intent(in) :: ofilename

  integer,  dimension(:), allocatable:: globalranks
  integer     :: sid, ierr, rank, fsblksize, n_proc, n_files
  integer*8   :: n8, chunksize, bread, posinblk
  integer*4   :: currentblknr
  character*2 :: filemoderb = "rb"

  integer*8   :: intbuffer, n_part, n_parttotal, partpertask, partpertask_remain, fieldheader8(4)
  real*8      :: realbuffer(6)
  integer     :: out_file = 89

  write (*, *) "sionmp2c convert: file       =", trim(filename)
  write (*, *) "sionmp2c convert: from        SION"
  write (*, *) "sionmp2c convert: to          ASCII"

  open(unit=out_file, file=ofilename, status="unknown")

  n_proc = -1
  n_files = 1
  fsblksize = -1
  sid = -2;

  ! get information about sion file from first file
  call fsion_open(trim(filename), filemoderb, n_proc, n_files, chunksize, fsblksize, globalranks(1), sid);
  if(sid < 0) then
     write (*, *) "sionmp2c convert: !!! could not open SION file !!!"
     return
  end if

  write (*, *) "sionmp2c convert: open file      = ", trim(filename)
  write (*, *) "sionmp2c convert: in, n_proc      = ", n_proc
  write (*, *) "sionmp2c convert: in, n_files     = ", n_files

  ! read header for this task
  call fsion_read(fieldheader8(1), sizeof(fieldheader8(1)), 3_8, sid, bread)
  write (*, *) "sionmp2c convert: header, n_slt       = ", fieldheader8(1)
  write (*, *) "sionmp2c convert: header, it          = ", fieldheader8(2)
  write (*, *) "sionmp2c convert: header, n_nodes     = ", fieldheader8(3)
  write(out_file, '(3i12)') fieldheader8(1), fieldheader8(2), fieldheader8(3)

  partpertask = floor((real(fieldheader8(1)) / real(n_proc)))
  partpertask_remain = fieldheader8(1) - partpertask * n_proc

  n_parttotal = 0
  n_part = 0
  currentblknr = 0
  posinblk = 0
  ! loop over rank in file
  do rank = 0, n_proc - 1
     call fsion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS, ierr)

     n_part = partpertask
     if(rank < partpertask_remain) n_part = n_part + 1

     n_parttotal = n_parttotal + n_part
     write (*, *) "sionmp2c convert: rank, total, npart = ", rank, n_parttotal, n_part

     do n8 = 1, n_part
        call fsion_read(intbuffer, sizeof(intbuffer), 1_8, sid, bread)
        call fsion_read(realbuffer(1), sizeof(realbuffer(1)), 6_8, sid, bread)
        write(out_file, '(i16, " ", 6(f19.12, " "))') intbuffer, realbuffer
     end do ! parts
  end do ! ranks

  call fsion_close(sid, ierr)

  close(out_file);

end subroutine mp2c_sion_ascii

!---------------------------------------------------------------------------------
! ASCII to SION
!---------------------------------------------------------------------------------
subroutine mp2c_ascii_sion(filename)
  use sion_f90
  implicit none

  character(len=1024), intent(in) :: filename

  integer*8, dimension(:), allocatable :: chunksizes
  integer, dimension(:), allocatable   :: globalranks
  integer, dimension(:), allocatable   :: parts
  real*8, dimension(3)               ::  r, v
  integer     :: sid, i, fsblksize = -1, ierr, rank, partpertask, nextparts
  integer*8   :: idx, n_part, partpertask_remain, n_proc, nt, bwrite, bwrote
  character*2 :: filemodewb = "wb"

  character(len=100) :: fmt_char = '("partest parameter:	", A30, TR1, "		= ", A)'
  character(len=100) :: fmt_bytes_mb = '("partest parameter:	", A30, TR1, "		= ", I10, " bytes (", F10.4, " MB)")'
  character(len=100) :: fmt_int = '("partest parameter:	", A30, TR1, "		= ", I10)'

  integer     :: in_file = 99

  open(unit=in_file, file="mp2c_slv.res")
  read(in_file, *) n_part, nt, n_proc

  write(0, fmt_char) "datafile", trim(filename)
  write(0, fmt_int) "n_part", n_part
  write(0, fmt_int) "nt", nt
  write(0, fmt_int) "n_proc", n_proc

  allocate( chunksizes(n_proc) );
  allocate( globalranks(n_proc) );
  allocate( parts(n_proc) );

  partpertask = floor((real(n_part) / real(n_proc))) ;
  partpertask_remain = n_part - partpertask * n_proc;

  do i = 1, int(n_proc)
     globalranks(i) = i
     parts(i) = partpertask;
     if(i <= partpertask_remain) parts(i) = parts(i) + 1
     chunksizes(i) = 8 * parts(i) * (3 + 3 + 1)
  enddo
  chunksizes(1) = chunksizes(1) + 100;

  write(0, fmt_int) "partpertask", partpertask
  write(0, fmt_int) "partpertask_remain", partpertask_remain

  write(0, fmt_bytes_mb) "fs block size", fsblksize, fsblksize / (1.0 * 1024 * 1024)

  call fsion_open(trim(filename), filemodewb, int(n_proc), 1, chunksizes(1), fsblksize, globalranks(1), sid);

  write(0, fmt_int) "sid", sid

  rank = 0;
  call fsion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS, ierr);
  ! write header on first PE
  bwrite = 8
  call fsion_write(n_part, 1_8, bwrite, sid, bwrote)
  call fsion_write(nt, 1_8, bwrite, sid, bwrote)
  call fsion_write(n_proc, 1_8, bwrite, sid, bwrote)

  do rank = 0, int(n_proc) - 1
     call fsion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS, ierr);
     nextparts = parts(rank + 1)
     write(*, *) 'rank = ', rank, ' nextparts = ', nextparts
     do i = 1, nextparts
        read(in_file, *) idx, r(1:3), v(1:3)
        bwrite = 8;   call fsion_write(idx, 1_8, bwrite, sid, bwrote)
        bwrite = 8 * 3; call fsion_write(r(1), 1_8, bwrite, sid, bwrote)
        bwrite = 8 * 3; call fsion_write(v(1), 1_8, bwrite, sid, bwrote)
     enddo
  enddo

  close(in_file)

  call fsion_close(sid, ierr)

end subroutine mp2c_ascii_sion

subroutine usage(name)
  character(len=64), intent(in) :: name

  write(0, '(A, A, A, /, A)') "Usage: ", TRIM(name), " <options>", &
       & "with the following optional options (default values in parenthesis)"
  write(0, '(/, A)') " Sion File Settings:"
  write(0, *) "  [-f filename]          input  filename for MP2C file"
  write(0, *) "  [-o filename]          output filename for MP2C file"
  write(0, *) "  [-A ]                  convert SION  file to ASCII"
  write(0, *) "  [-S ]                  convert ASCII file to SION"

end subroutine usage
