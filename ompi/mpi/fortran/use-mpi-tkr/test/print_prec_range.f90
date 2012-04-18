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
! print_prec_range.f90
!   - prints out values of precision and range for real types
!
program main
  real :: xr
  real(kind(1.0D0)) :: xd
  real(selected_real_kind(5)) :: x5
  real(selected_real_kind(6)) :: x6
  real(selected_real_kind(7)) :: x7
  real(selected_real_kind(14)) :: x14
  real(selected_real_kind(15)) :: x15
  real(selected_real_kind(16)) :: x16
  real(selected_real_kind(30)) :: x30
  real(selected_real_kind(31)) :: x31
  real(selected_real_kind(32)) :: x32

  print *, "precision(p=5) = ", precision(x5), " range(p=5) = ", range(x5)
  print *, "precision(p=6) = ", precision(x6), " range(p=6) = ", range(x6)
  print *, "precision(p=7) = ", precision(x7), " range(p=7) = ", range(x7)
  print *, "precision(p=14) = ", precision(x14), " range(p=14) = ", range(x14)
  print *, "precision(p=15) = ", precision(x15), " range(p=15) = ", range(x15)
  print *, "precision(p=16) = ", precision(x16), " range(p=16) = ", range(x16)
  print *, "precision(p=30) = ", precision(x30), " range(p=30) = ", range(x30)
  print *, "precision(p=31) = ", precision(x31), " range(p=31) = ", range(x31)
  print *, "precision(p=32) = ", precision(x32), " range(p=32) = ", range(x32)
  print *
  print *, "precision(r) = ", precision(xr), " range(r) = ", range(r)
  print *, "precision(d) = ", precision(xd), " range(d) = ", range(d)

end program main
