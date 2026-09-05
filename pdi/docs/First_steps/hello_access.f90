!*******************************************************************************
! Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! * Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
! * Neither the names of CEA, nor the names of the contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written  permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!******************************************************************************/

!! [example]
subroutine print_secret_msg
  use pdi
  implicit none

  integer, pointer               :: p_val
  character, pointer, contiguous :: p_msg(:)
  integer                        :: msg_ranks(1)

  call pdi_access("my_value", p_val, pdi_in)
  print *, "PDI value: ", p_val
  call pdi_release("my_value")

  ! In case of accessing arrays, PDI_access takes additional array argument with dimensions sizes
  msg_ranks(1) = 32
  call PDI_access("my_message", p_msg, pdi_in, msg_ranks)
  print *, "PDI message: ", p_msg
  call pdi_release("my_message")
  return
end subroutine print_secret_msg

program access
  use paraconf
  use pdi
  implicit none

  type(pc_tree_t)  :: conf
  integer          :: my_value
  character        :: secret_message(32)

  call pc_parse_path("hello_access.yml", conf)
  call pdi_init(conf)

  my_value = 42
  secret_message = (/'W','a','t','e','r','m','e','l','o','n',' ','i','s',' ','t','h','e',' ',&
       't','a','s','t','i','e','s','t',' ','f','r','u','i','t'/)
  print *, "My value: ", my_value
  print *, "My message: ", secret_message

  call pdi_share("my_value", my_value, pdi_out)
  call pdi_share("my_message", secret_message, pdi_out)

  call print_secret_msg()

  call pdi_reclaim("my_message")
  call pdi_reclaim("my_value")
  call pdi_finalize()
end program access
!! [example]
