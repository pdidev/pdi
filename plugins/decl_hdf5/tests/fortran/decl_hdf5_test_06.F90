!*******************************************************************************
! Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

program hdf5_transaction
    use paraconf
    use PDI
    implicit none

    character(len=512)     :: strbuf
    type(PC_tree_t),target :: conf
    integer                :: i
    integer,target         :: x
    integer,pointer        :: px
    real(4), target        :: y(8)
    real(4), pointer       :: py(:)

    call get_command_argument(1, strbuf)
    call PC_parse_path(strbuf, conf)

    call PDI_init(conf)

    x = 42
    do i = 1, 8
        y(i) = i * 1.23
    end do
    py => y

    px => x
    py => y

    call PDI_transaction_begin("write")
    call PDI_expose("x", px, PDI_OUT)
    call PDI_expose("y", py, PDI_OUT)
    call PDI_transaction_end()

    x = 0
    do i = 1, 8
        y(i) = 0
    end do


    call PDI_transaction_begin("read")
    call PDI_expose("x", px, PDI_IN)
    call PDI_expose("y", py, PDI_IN)
    call PDI_transaction_end()

    if (x /= 42) then
        print *, "x = ", x
        call EXIT(1)
    endif

    do i = 1, 8
        if (y(i) /= i * 1.23) then
            print *, "y(", i, ") = ", y(i), " (not ", i * 1.23, ")"
        call EXIT(1)
        end if
    end do
    
    call PDI_finalize()

end program hdf5_transaction
