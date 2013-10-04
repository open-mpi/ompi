! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_get_attr_f08(type,type_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Type_get_attr
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: type
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Type_get_attr(type%MPI_VAL,type_keyval,attribute_val,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Type_get_attr_f08
