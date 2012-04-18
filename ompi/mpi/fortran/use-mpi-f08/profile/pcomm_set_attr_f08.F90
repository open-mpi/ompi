! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_set_attr_f08(comm,comm_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_comm_set_attr_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_set_attr_f(comm%MPI_VAL,comm_keyval,attribute_val,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_set_attr_f08
