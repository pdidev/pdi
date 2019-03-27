module infostruct
    implicit none

    private

    type, public :: t_infostruct
        sequence
        integer :: type
        integer :: width
        integer :: height
        integer :: numprocs
        real :: xmin
        real :: xmax
        real :: ymin
        real :: ymax
        integer :: maxiter
    end type t_infostruct

    type, public :: t_pos_struct
        sequence
        integer :: width
        integer :: height
        integer :: xpos
        integer :: ypos
    end type t_pos_struct

end module infostruct
