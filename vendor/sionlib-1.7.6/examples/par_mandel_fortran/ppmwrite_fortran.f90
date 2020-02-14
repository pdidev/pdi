module ppmwrite
    implicit none

    private

    public :: fppminit, fppminitsmooth, fppmwrite
contains
    subroutine fppminit(d)
        integer, intent(in) :: d
        call ppminit(d)
    end subroutine fppminit
    subroutine fppminitsmooth(d)
        integer, intent(in) :: d
        call ppminitsmooth(d)
    end subroutine fppminitsmooth

    subroutine fppmwrite(a, nx, ny, minval, maxval, filename)
        integer, dimension(:), intent(in) :: a
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        integer, intent(in) :: minval
        integer, intent(in) :: maxval
        character(len=*), intent(in) :: filename

        call ppmwriter(a,nx,ny,minval,maxval,filename // char(0))
    end subroutine fppmwrite
end module ppmwrite
