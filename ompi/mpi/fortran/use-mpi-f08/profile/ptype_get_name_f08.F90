! -*- f90 -*-
!
! Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Type_get_name_f08(type,type_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_MAX_OBJECT_NAME
   use :: mpi_f08, only : ompi_type_get_name_f
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: type
   CHARACTER(LEN=*), INTENT(OUT) :: type_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_get_name_f(type%MPI_VAL,type_name,resultlen,c_ierror,len(type_name))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_get_name_f08
