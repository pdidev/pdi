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
program hello_multi_expose
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t)   :: conf
    integer           :: my_int
    real              :: my_float
    character         :: my_string(26)

    call PC_parse_path("hello_multi_expose.yml", conf)
    call PDI_init(conf)

    my_int = 0
    my_float = 0
    my_string = (/'R','G','B',' ','=',' ','R','e','a','l','l','y',' ',&
         'G','a','w','k','y',' ','B','i','s','c','u','i','t'/)

    call PDI_transaction_begin("event_between")

    call PDI_expose("my_int", my_int, PDI_OUT)
    call PDI_expose("my_float", my_float, PDI_OUT)
    call PDI_expose("my_string", my_string, PDI_OUT)

    call PDI_transaction_end()

    call PDI_finalize()

end program hello_multi_expose
!! [example]
