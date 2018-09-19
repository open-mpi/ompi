! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Status_set_elements_x_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype, MPI_COUNT_KIND
   use :: ompi_mpifh_bindings, only : ompi_status_set_elements_x_f
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(IN) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_status_set_elements_x_f(status,datatype%MPI_VAL,count,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Status_set_elements_x_f08
