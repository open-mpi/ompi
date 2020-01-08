! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Type_free_f08 PMPI_Type_free_f08
#endif

subroutine MPI_Type_free_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   use :: ompi_mpifh_bindings, only : ompi_type_free_f
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_free_f(datatype%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_free_f08
