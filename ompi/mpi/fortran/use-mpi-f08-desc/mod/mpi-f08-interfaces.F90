! -*- f90 -*-
!
! Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the name for tools ("MPI_Init_f08") and the back-end implementation
! name (e.g., "MPI_Init_f08").

module mpi_f08_interfaces

interface  MPI_Comm_rank
subroutine MPI_Comm_rank_f08(comm,rank,ierror)
   use :: mpi_f08_types
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_rank_f08
end interface  MPI_Comm_rank

interface  MPI_Comm_size
subroutine MPI_Comm_size_f08(comm,size,ierror)
   use :: mpi_f08_types
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_size_f08
end interface  MPI_Comm_size

interface  MPI_Finalize
subroutine MPI_Finalize_f08(ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Finalize_f08
end interface  MPI_Finalize

interface  MPI_Init
subroutine MPI_Init_f08(ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Init_f08
end interface  MPI_Init

! Note that send/recv only works with specific types for buffers (2D integers)
! Double precision is not implemented at this time but allows compiler to do
! type checking so that MPI_SUBARRAYS_SUPPORTED flag can be utilized in
! test_send_recv.f90.
!

interface  MPI_Recv
subroutine MPI_Recv_f08_desc_int_2d(buf,count,datatype,source,tag,comm,status,ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN), target :: buf(:,:)
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Recv_f08_desc_int_2d
subroutine MPI_Recv_f08_desc_dbl_1d(buf,count,datatype,source,tag,comm,status,ierror)
   use :: mpi_f08_types
   implicit none
   DOUBLE PRECISION, INTENT(IN), target :: buf(:)
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Recv_f08_desc_dbl_1d
subroutine MPI_Recv_f08_desc_dbl_0d(buf,count,datatype,source,tag,comm,status,ierror)
   use :: mpi_f08_types
   implicit none
   DOUBLE PRECISION, INTENT(IN), target :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Recv_f08_desc_dbl_0d
end interface  MPI_Recv

interface  MPI_Send
subroutine MPI_Send_f08_desc_int_2d(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN), target :: buf(:,:)
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Send_f08_desc_int_2d
subroutine MPI_Send_f08_desc_dbl_1d(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types
   implicit none
   DOUBLE PRECISION, INTENT(IN), target :: buf(:)
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Send_f08_desc_dbl_1d
subroutine MPI_Send_f08_desc_dbl_0d(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types
   implicit none
   DOUBLE PRECISION, INTENT(IN), target :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Send_f08_desc_dbl_0d
end interface  MPI_Send

interface  MPI_Type_commit
subroutine MPI_Type_commit_f08(datatype,ierror)
   use :: mpi_f08_types
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_commit_f08
end interface  MPI_Type_commit

interface  MPI_Type_contiguous
subroutine MPI_Type_contiguous_f08(count,oldtype,newtype,ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_contiguous_f08
end interface  MPI_Type_contiguous

interface  MPI_Type_vector
subroutine MPI_Type_vector_f08(count,blocklength,stride,oldtype,newtype,ierror)
   use :: mpi_f08_types
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength, stride
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_vector_f08
end interface  MPI_Type_vector

end module mpi_f08_interfaces
