module mpi_kinds

  include "mpif.h"

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

!
! kind for 4 byte integer (selected_int_kind(18) for 8 byte integer)
! (WARNING, Compiler dependent)
!
  integer :: MPI_ADDRESS_KIND
  parameter(MPI_ADDRESS_KIND = selected_int_kind(9))

!
! kind for int64_t equivalent, used for offsets
!
  integer :: MPI_OFFSET_KIND
  parameter(MPI_OFFSET_KIND = selected_int_kind(18))

!
! integer kinds
!

  integer :: MPI_INTEGER1_KIND, MPI_INTEGER2_KIND, MPI_INTEGER4_KIND
  integer :: MPI_INTEGER8_KIND, MPI_INTEGER16_KIND
  parameter(MPI_INTEGER1_KIND  = selected_int_kind(2))
  parameter(MPI_INTEGER2_KIND  = selected_int_kind(4))
  parameter(MPI_INTEGER4_KIND  = selected_int_kind(9))
  parameter(MPI_INTEGER8_KIND  = selected_int_kind(18))
  parameter(MPI_INTEGER16_KIND = selected_int_kind(19))

!
! real kinds
!

  integer :: MPI_REAL4_KIND, MPI_REAL8_KIND
  integer :: MPI_REAL16_KIND, MPI_REAL32_KIND
  parameter(MPI_REAL4_KIND  = selected_real_kind(6))
  parameter(MPI_REAL8_KIND  = selected_real_kind(15))
  parameter(MPI_REAL16_KIND = selected_real_kind(31))
  parameter(MPI_REAL32_KIND = selected_real_kind(32))

end module mpi_kinds


module mpi

  use mpi_kinds

  include "mpi.i.h"

end module mpi
