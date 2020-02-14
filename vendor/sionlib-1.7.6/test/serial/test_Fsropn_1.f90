!*****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2019                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!*****************************************************************************
program main

  use sion_f90

  implicit none
      
  character*2 :: ppmtype
  integer     :: xmaxpoints
  integer     :: ymaxpoints,myymaxpoints
  integer     :: colorlimit
  character*6, allocatable :: image(:)   
  character*20 :: output_fname="test_Fsropn_1.ppm"
  character*20 :: input_fname="test_Fsropn_1.sion"

  integer   :: ntasks,nfiles,fsblksize,globalranks,sid,ipoint
  integer   :: countp,currentblknr,itask,ierr
  integer*8 :: chunksize,posinblk,bread, size,nelem
  character*2 :: filemode="rb"
  integer   :: current_endianness,file_endianness, doswap, swap_needed, rc

  write(*,'(A)')
  write(*,'(A)') '  ==============================='
  write(*,'(A)') '  Calculations of mandelbrot set.'
  write(*,'(A)') '  ==============================='
  write(*,'(A)') '  Converting mandelbrot.sion to mandelbrot.ppm '
  write(*,'(A)')

  fsblksize = -1
  nfiles    =  1
  sid       = -1 
  call fsion_open(trim(input_fname),filemode,ntasks,nfiles,chunksize,fsblksize,globalranks,sid)
  open(unit=42,file=trim(output_fname),status="unknown",form="formatted")

  ! Readi-in data
  countp       = 0
  currentblknr = 0
  posinblk     = 0

  call fsion_get_endianness(current_endianness)
  call fsion_get_file_endianness(sid,file_endianness)
  call fsion_endianness_swap_needed(sid,swap_needed)
  if(current_endianness == file_endianness) then
     doswap=0
  else
     doswap=1
  end if
  if(doswap /= swap_needed) then
      write(*,'(A)') '  doswap /= swap_needed'
  end if


! Read image
  do itask=0,ntasks-1
    call fsion_seek(sid,itask,currentblknr,posinblk,ierr)
    if (itask == 0) then
! Read ppm header in and write it to ppm file
      write(*,'(A,I6)') '  Converting points from task ',itask
      size=2;nelem=1; call fsion_read(ppmtype,size,nelem,sid,bread)
      size=4;nelem=1; call fsion_read(xmaxpoints,size,nelem,sid,bread)
      call fsion_swap(xmaxpoints,xmaxpoints,4,1,doswap,rc)
      size=4;nelem=1; call fsion_read(ymaxpoints,size,nelem,sid,bread)
      call fsion_swap(ymaxpoints,ymaxpoints,4,1,doswap,rc)
      size=4;nelem=1; call fsion_read(colorlimit,size,nelem,sid,bread)
      call fsion_swap(colorlimit,colorlimit,4,1,doswap,rc)
      write(*,'(A,I6,A)') '  File created with ',ntasks,' tasks'
      write(*,'(A,A2)') '  ppmtype: ',ppmtype
      write(*,'(A,I6)') '  xmaxpoints: ',xmaxpoints
      write(*,'(A,I6)') '  ymaxpoints: ',ymaxpoints
      write(*,'(A,I6)') '  colorlimit: ',colorlimit
      write(42,'(A2,A1,I6,A1,I6,A1,I6)') ppmtype,' ',xmaxpoints,' ',ymaxpoints,' ',colorlimit
    endif
    write(*,'(A,I6)') '  Reading data from task ',itask
    myymaxpoints = ymaxpoints/ntasks
    if (mod(ymaxpoints,ntasks) /= 0) then
      if (itask == ntasks-1) then
        myymaxpoints = myymaxpoints + mod(ymaxpoints,ntasks)
      endif
    endif
    allocate (image(myymaxpoints*xmaxpoints))
    size=6;nelem=myymaxpoints*xmaxpoints; call fsion_read(image(1),size,nelem,sid,bread)
    do ipoint=1,myymaxpoints*xmaxpoints
      write(42,'(A6)') image(ipoint)
    enddo
    deallocate(image)
  enddo

   close(42)
   call fsion_close(sid,ierr)

end program main
