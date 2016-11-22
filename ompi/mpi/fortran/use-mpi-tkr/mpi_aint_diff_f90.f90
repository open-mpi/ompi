! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2015      Los Alamos National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

function MPI_Aint_diff(addr1, addr2)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr1
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr2
  integer(kind=MPI_ADDRESS_KIND) :: MPI_Aint_diff,foo
  call MPI_Aint_diff_f90(addr1,addr2,foo)
  MPI_Aint_diff = foo
end function MPI_Aint_diff

