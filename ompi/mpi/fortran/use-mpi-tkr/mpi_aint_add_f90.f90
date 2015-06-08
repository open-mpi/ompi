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

function MPI_Aint_add(base, diff)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: diff
  integer(kind=MPI_ADDRESS_KIND) :: MPI_Aint_add,foo
  call MPI_Aint_add_f90(base,diff,foo)
  MPI_Aint_add = foo
end function MPI_Aint_add

