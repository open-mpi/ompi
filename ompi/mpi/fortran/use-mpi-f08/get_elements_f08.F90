! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Get_elements_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   use :: mpi_f08, only : ompi_get_elements_f
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_get_elements_f(status,datatype%MPI_VAL,count,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Get_elements_f08
