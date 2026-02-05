! SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
! SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
!
! SPDX-License-Identifier: BSD-3-Clause

program share
    use paraconf
    use PDI
    implicit none

    character(len=512)             :: strbuf
    type(PC_tree_t), target        :: conf
    integer                        :: i
    integer, target                :: x
    real(4), target, dimension(8)  :: y
    integer, pointer               :: px
    real(4), pointer, contiguous   :: py(:)

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
    call PDI_reclaim("x")

    call PDI_share("y", py, PDI_OUT)
    call PDI_reclaim("y")
    
    call PDI_finalize()

end program share
