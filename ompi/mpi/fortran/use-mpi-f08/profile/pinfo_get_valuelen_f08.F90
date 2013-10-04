! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Info_get_valuelen_f08(info,key,valuelen,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Info_get_valuelen
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(OUT) :: valuelen
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Info_get_valuelen(info%MPI_VAL,key,valuelen,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Info_get_valuelen_f08
