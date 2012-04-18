! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Type_create_hindexed_f08(count,array_of_blocklengths, &
                                        array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_type_create_hindexed_f
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_create_hindexed_f(count,array_of_blocklengths, &
                                    array_of_displacements,oldtype%MPI_VAL, &
                                    newtype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_create_hindexed_f08
