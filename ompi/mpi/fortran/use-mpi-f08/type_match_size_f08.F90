! -*- f90 -*-
!
! Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Type_match_size_f08 PMPI_Type_match_size_f08
#endif

subroutine MPI_Type_match_size_f08(typeclass,size,datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: ompi_mpifh_bindings, only : ompi_type_match_size_f
   implicit none
   INTEGER, INTENT(IN) :: typeclass, size
   TYPE(MPI_Datatype), INTENT(OUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_match_size_f(typeclass,size,datatype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_match_size_f08
