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
! print_rkinds.f90
!   - prints out values of selected_real_kind to determine Fortran compiler
!     dependent values
!
program main
  integer :: i
  !
  !  real :: x = 2.2_QUAD
  !
  complex(kind(1.D0)) :: z

  do i = 1, 32
    print*, "i = ", i, "selected_real_kind(i) = ", selected_real_kind(i)
  end do

  print*, "kind(1.E0) = ", kind(1.E0)
  print*, "kind(1.D0) = ", kind(1.D0)
  !
  !  print*, "kind( real :: x = 2.2_QUAD ) = ", kind(x)
  !
  print*, "kind( complex(kind(1.D0) :: z ) = ", kind(z)

end program main
