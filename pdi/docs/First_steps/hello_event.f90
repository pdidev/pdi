! SPDX-FileCopyrightText: 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
!
! SPDX-License-Identifier: BSD-3-Clause

program hello_event
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t), target :: conf

    call PC_parse_path("hello_event.yml", conf)
    call PDI_init(conf)

    call PDI_event("Hello World Event")

    call PDI_finalize()

end program hello_event
