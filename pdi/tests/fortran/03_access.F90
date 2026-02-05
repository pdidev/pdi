! SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
! SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
!
! SPDX-License-Identifier: BSD-3-Clause

program access
    use paraconf
    use PDI
    use iso_c_binding, only: c_associated, c_loc, c_ptr
    implicit none

    character(len=512)             :: strbuf
    type(PC_tree_t),target         :: conf
    integer                        :: i, yranks(1)
    integer, target                :: x
    integer, pointer               :: px, pxaccess
    type(c_ptr)                    :: cpx, cpxaccess
    real(4), target                :: y(8)
    real(4), pointer, contiguous   :: py(:), pyaccess(:)
    type(c_ptr)                    :: cpy, cpyaccess

    call get_command_argument(1, strbuf)
    call PC_parse_path(strbuf, conf)

    call PDI_init(conf)

    x = 42
    px => x

    call PDI_share("x", px, PDI_OUT)
    call PDI_access("x", pxaccess, PDI_IN)

    cpx = c_loc(px)
    cpxaccess = c_loc(pxaccess)

    if (.NOT. c_associated(cpx, cpxaccess)) then
        print *, "Share and accessed pointers are different"
        call PDI_release("x")
        call EXIT(1)
    endif

    call PDI_release("x")
    call PDI_reclaim("x")


    do i = 1, 8
        y(i) = i * 1.23
    end do
    py => y

    call PDI_share("y", py, PDI_OUT)

    yranks(1) = 8
    call PDI_access("y", pyaccess, PDI_IN, yranks)

    cpy = c_loc(py)
    cpyaccess = c_loc(pyaccess)

    if (.NOT. c_associated(cpy, cpyaccess)) then
        print *, "Share and accessed pointers are different"
        call PDI_release("y")
        call EXIT(1)
    endif

    call PDI_release("y")
    call PDI_reclaim("y")
    
    call PDI_finalize()

end program access
