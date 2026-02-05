! SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
!
! SPDX-License-Identifier: BSD-3-Clause

program init_finalize
    use paraconf
    use PDI
    implicit none

    character(len=512)     :: strbuf
    type(PC_tree_t),target :: conf

    call get_command_argument(1, strbuf)
    call PC_parse_path(strbuf, conf)

    call PDI_init(conf)

    call PDI_finalize()

end program init_finalize
