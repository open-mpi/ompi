! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_get_contents_f08(datatype,max_integers,max_addresses, &
                     max_datatypes,array_of_integers,array_of_addresses, &
                     array_of_datatypes,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_type_get_contents_f
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
   INTEGER, INTENT(OUT) :: array_of_integers(max_integers)
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: array_of_addresses(max_addresses)
   TYPE(MPI_Datatype), INTENT(OUT) :: array_of_datatypes(max_datatypes)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_get_contents_f(datatype%MPI_VAL,max_integers,max_addresses, &
                    max_datatypes,array_of_integers,array_of_addresses, &
                    array_of_datatypes(:)%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_get_contents_f08
