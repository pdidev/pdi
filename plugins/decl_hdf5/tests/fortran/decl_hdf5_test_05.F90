! SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
! SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
!
! SPDX-License-Identifier: BSD-3-Clause

program hdf5_event
    use paraconf
    use PDI
    implicit none

    character(len=512)           :: strbuf
    type(PC_tree_t),target       :: conf
    integer                      :: i
    integer,target               :: x
    integer,pointer              :: px
    real(4), target              :: y(8)
    real(4), pointer, contiguous :: py(:)

    call get_command_argument(1, strbuf)
    call PC_parse_path(strbuf, conf)

    call PDI_init(conf)

    x = 42
    px => x

    do i = 1, 8
        y(i) = i * 1.23
    end do
    py => y

    call PDI_share("x", px, PDI_OUT)
    call PDI_share("y", py, PDI_OUT)
    call PDI_event("write")
    call PDI_reclaim("y")
    call PDI_reclaim("x")

    x = 0
    do i = 1, 8
        y(i) = 0
    end do
    py => y

    call PDI_share("x", px, PDI_IN)
    call PDI_share("y", py, PDI_IN)
    call PDI_event("read")
    call PDI_reclaim("y")
    call PDI_reclaim("x")

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

end program hdf5_event
