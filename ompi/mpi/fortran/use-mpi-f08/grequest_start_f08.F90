! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Grequest_start_f08(query_fn,free_fn,cancel_fn,&
                                  extra_state,request,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_query_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_free_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_cancel_function
   use :: mpi_f08, only : ompi_grequest_start_f
   implicit none
   OMPI_PROCEDURE(MPI_Grequest_query_function) :: query_fn
   OMPI_PROCEDURE(MPI_Grequest_free_function) :: free_fn
   OMPI_PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_grequest_start_f(query_fn,free_fn,cancel_fn,&
                              extra_state,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Grequest_start_f08
