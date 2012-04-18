! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Type_create_indexed_block_f08(count,blocklength, &
                           array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: mpi_f08, only : ompi_type_create_indexed_block_f
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER, INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_create_indexed_block_f(count,blocklength,array_of_displacements, &
                                         oldtype%MPI_VAL,newtype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_create_indexed_block_f08
