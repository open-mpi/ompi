! -*- f90 -*-
!
! Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_set_name_f08(datatype,type_name,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: ompi_mpifh_bindings, only : ompi_type_set_name_f
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   CHARACTER(LEN=*), INTENT(IN) :: type_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_set_name_f(datatype%MPI_VAL,type_name,c_ierror,len(type_name))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_set_name_f08
