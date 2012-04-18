! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Type_commit_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: mpi_f08, only : ompi_type_commit_f
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_commit_f(datatype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_commit_f08
