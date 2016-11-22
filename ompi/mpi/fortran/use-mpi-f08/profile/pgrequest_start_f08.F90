! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Grequest_start_f08(query_fn,free_fn,cancel_fn,&
                                  extra_state,request,ierror)
   use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
   use :: mpi_f08_types, only : MPI_Request, MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_query_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_free_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_cancel_function
   use :: mpi_f08, only : ompi_grequest_start_f
   implicit none
   PROCEDURE(MPI_Grequest_query_function) :: query_fn
   PROCEDURE(MPI_Grequest_free_function) :: free_fn
   PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror
   type(c_funptr) :: fquery_fn, ffree_fn, fcancel_fn

   fquery_fn = c_funloc(query_fn)
   ffree_fn = c_funloc(free_fn)
   fcancel_fn = c_funloc(cancel_fn)
   call ompi_grequest_start_f(fquery_fn,ffree_fn,fcancel_fn,&
                              extra_state,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Grequest_start_f08
