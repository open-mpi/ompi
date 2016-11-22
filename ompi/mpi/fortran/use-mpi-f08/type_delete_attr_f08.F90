! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Type_delete_attr_f08(type,type_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: mpi_f08, only : ompi_type_delete_attr_f
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: type
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_delete_attr_f(type%MPI_VAL,type_keyval,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_delete_attr_f08
