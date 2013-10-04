! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_File_get_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_File_get_atomicity
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_File_get_atomicity(fh%MPI_VAL,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_File_get_atomicity_f08
