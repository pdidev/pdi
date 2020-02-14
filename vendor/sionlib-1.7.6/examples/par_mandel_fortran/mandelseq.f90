program mandelseq
    use infostruct
    use mandelsion
    use ppmwrite

    implicit none

    integer, dimension(:), pointer :: iterations
    integer, dimension(:), pointer :: proc_distribution

    type(t_infostruct) :: info

    nullify(proc_distribution)

    ! init ppm
    call fppminitsmooth(1)

    ! read SION file
    call collect_sion(iterations, proc_distribution, info)

    ! print information
    call print_infostruct(info)

    if (associated(iterations)) then
        ! create ppm files
        call fppmwrite(iterations, info%width, info%height, 0, info%maxiter, "mandelcol.ppm")
        deallocate(iterations)
    end if

    if (associated(proc_distribution)) then
        call fppmwrite(proc_distribution, info%width, info%height, 0, info%numprocs, "mandelcol_procs.ppm")
        deallocate(proc_distribution)
    end if

contains
    ! Print infostruct data
    subroutine print_infostruct(info)
        type(t_infostruct), intent(in) :: info

        write(unit=*,fmt='(1x,a,i10)') "type      = ", info%type
        write(unit=*,fmt='(1x,a,i10)') "width     = ", info%width
        write(unit=*,fmt='(1x,a,i10)') "height    = ", info%height
        write(unit=*,fmt='(1x,a,i10)') "numprocs  = ", info%numprocs
        write(unit=*,fmt='(1x,a,f10.2)') "xmin      = ", info%xmin
        write(unit=*,fmt='(1x,a,f10.2)') "xmax      = ", info%xmax
        write(unit=*,fmt='(1x,a,f10.2)') "ymin      = ", info%ymin
        write(unit=*,fmt='(1x,a,f10.2)') "ymax      = ", info%ymax
        write(unit=*,fmt='(1x,a,i10)') "maxiter   = ", info%maxiter
    end subroutine print_infostruct
end program mandelseq
