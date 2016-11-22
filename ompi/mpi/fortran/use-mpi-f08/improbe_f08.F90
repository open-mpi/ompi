! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2010-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Improbe_f08(source,tag,comm,flag,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Message, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Message), INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Improbe(source, tag, comm, flag, message, status, ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(in) :: source
        integer, intent(in) :: tag
        integer, intent(in) :: comm
        logical, intent(out) :: flag
        integer, intent(out) :: message
        type(MPI_STATUS), intent(out) :: status
        integer, intent(out) :: ierror
      end subroutine PMPI_Improbe
   end interface

   call PMPI_Improbe(source,tag,comm%MPI_VAL,flag,message%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Improbe_f08
