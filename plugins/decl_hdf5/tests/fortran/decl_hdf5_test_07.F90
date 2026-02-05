! SPDX-FileCopyrightText: 2021 Centre national de la recherche scientifique (CNRS)
!
! SPDX-License-Identifier: BSD-3-Clause

program async
  use paraconf
  use pdi
  implicit none
  
  character(len=512)      :: strbuf
  type(pc_tree_t),target  :: conf
  character(len=32)       :: string2write, string2read
  
  call get_command_argument(1, strbuf)
  call pc_parse_path(strbuf, conf)
  call pdi_init(conf)

  string2write="I am the walrus"

  ! write to HDF5 file
  call PDI_expose("size", len_trim(string2write) , PDI_OUT)
  
  call PDI_share("string1", string2write, PDI_OUT)
  call PDI_event("write_into_file")
  call PDI_reclaim("string1")
  
  ! read from HDF5 file
  string2read="" ! this initialization is necessary
  call PDI_share("string1", string2read, PDI_IN)
  call PDI_event("read_from_file")
  call PDI_reclaim("string1")  

  if (string2write .ne. string2read) then
     write(0,*) "Fortran written/read 'character(len' variable differ: ", &
          string2write, "/", string2read
     call EXIT(1)
  endif
  
  call pdi_finalize() 
end program
