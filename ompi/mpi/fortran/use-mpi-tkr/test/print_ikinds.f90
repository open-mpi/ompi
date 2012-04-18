!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
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
  !
  !  integer(kind(1_LONG)) :: l = 1_LONG
  !

  do i = 1, 24
    print*, "i = ", i, "selected_int_kind(i) = ", selected_int_kind(i)
  end do

  print*, "kind(1) = ", kind(1)
  !
  !  print*, "kind(1_LONG) = ", kind(l)
  !
end program main
