! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Comm_create_keyval_f08(comm_copy_attr_fn,comm_delete_attr_fn,&
                                      comm_keyval,extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_delete_attr_function
   use :: mpi_f08, only : ompi_comm_create_keyval_f
   implicit none
   OMPI_PROCEDURE(MPI_Comm_copy_attr_function) :: comm_copy_attr_fn
   OMPI_PROCEDURE(MPI_Comm_delete_attr_function) :: comm_delete_attr_fn
   INTEGER, INTENT(OUT) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_create_keyval_f(comm_copy_attr_fn,comm_delete_attr_fn,&
                                  comm_keyval,extra_state,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_create_keyval_f08
