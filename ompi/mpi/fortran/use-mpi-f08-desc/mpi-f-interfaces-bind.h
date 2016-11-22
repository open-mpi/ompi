! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the back-end implementation subroutine name (e.g., "ompi_init_f").

interface

subroutine ompi_comm_rank_f(comm,rank,ierror) &
   BIND(C, name="ompi_comm_rank_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: rank
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_rank_f

subroutine ompi_comm_size_f(comm,size,ierror) &
   BIND(C, name="ompi_comm_size_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_size_f

subroutine ompi_finalize_f(ierror) &
   BIND(C, name="ompi_finalize_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_finalize_f

subroutine ompi_init_f(ierror) &
   BIND(C, name="ompi_init_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_init_f

! ompi_send_f/ompi_recv_f interfaces not needed as they are called from C
!

subroutine ompi_type_commit_f(datatype,ierror) &
   BIND(C, name="ompi_type_commit_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(INOUT) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_commit_f

subroutine ompi_type_contiguous_f(count,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_contiguous_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_contiguous_f

subroutine ompi_type_vector_f(count,blocklength,stride,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_vector_f")
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength, stride
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_vector_f

end interface
