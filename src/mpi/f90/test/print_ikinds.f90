!
! Copyright (c) 2004-2005 The Trustees of Indiana University.
!                         All rights reserved.
! Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
!                         All rights reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
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
