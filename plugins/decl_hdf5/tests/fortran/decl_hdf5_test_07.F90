!*******************************************************************************
! Copyright (C) 2021 Centre national de la recherche scientifique (CNRS)
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! * Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer.
! * Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! * Neither the name of CEA nor the names of its contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written permission.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
! ******************************************************************************/

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
