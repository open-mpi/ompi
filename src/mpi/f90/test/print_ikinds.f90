!
! $HEADER$
!
! print_ikinds.f90
!   - prints out values of selected_int_kind to determine Fortran compiler
!     dependent values
!
program main
  integer :: i

  do i = 1, 24
    print*, "i = ", i, "selected_int_kind(i) = ", selected_int_kind(i)
  end do

end program main
