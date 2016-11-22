! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_create_darray_f08(size,rank,ndims,&
                     array_of_gsizes,array_of_distribs,array_of_dargs,array_of_psizes,&
                     order,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: mpi_f08, only : ompi_type_create_darray_f
   implicit none
   INTEGER, INTENT(IN) :: size, rank, ndims, order
   INTEGER, INTENT(IN) :: array_of_gsizes(*), array_of_distribs(*), array_of_dargs(*), array_of_psizes(*)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_create_darray_f(size,rank,ndims, &
                    array_of_gsizes,array_of_distribs,array_of_dargs,array_of_psizes, &
                    order,oldtype%MPI_VAL,newtype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_create_darray_f08
