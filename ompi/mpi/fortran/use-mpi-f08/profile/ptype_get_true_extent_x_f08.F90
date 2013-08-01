! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_get_true_extent_x_f08(datatype,true_lb,true_extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   use :: mpi_f08, only : ompi_type_get_true_extent_x_f
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_get_true_extent_x_f(datatype%MPI_VAL,true_lb,true_extent,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_get_true_extent_x_f08
