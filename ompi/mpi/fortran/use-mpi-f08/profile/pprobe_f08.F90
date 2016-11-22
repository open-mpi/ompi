! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Probe_f08(source,tag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   use :: mpi_f08, only : ompi_probe_f
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_probe_f(source,tag,comm%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Probe_f08
