module mandelsion
    use infostruct

    use sion_f90
    use sion_f90_mpi

    implicit none

    include "mpif.h"

    private

    public :: open_sion, close_sion, write_to_sion_file, collect_sion

contains
    ! open SION file
    subroutine open_sion(sid, info, blocksize, start, rank)
        integer, intent(out) :: sid
        type(t_infostruct), intent(in) :: info
        integer, dimension(2), intent(in) :: blocksize
        integer, dimension(2), intent(in) :: start
        integer, intent(in) :: rank

    end subroutine open_sion

    ! write block to SION file
    subroutine write_to_sion_file(sid, info, iterations, width, height, xpos, ypos)
        integer, intent(in) :: sid
        type(t_infostruct), intent(in) :: info
        integer, dimension(:), intent(in) :: iterations
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(in) :: xpos
        integer, intent(in) :: ypos

    end subroutine write_to_sion_file

    ! close SION file
    subroutine close_sion(sid, info, rank)
        integer, intent(inout) :: sid
        type(t_infostruct), intent(in) :: info
        integer, intent(in) :: rank

    end subroutine close_sion


    ! read SION file
    subroutine collect_sion(iterations, proc_distribution, info)
        integer, dimension(:), pointer :: iterations
        integer, dimension(:), pointer :: proc_distribution
        type(t_infostruct), intent(inout) :: info

    end subroutine collect_sion
end module mandelsion
