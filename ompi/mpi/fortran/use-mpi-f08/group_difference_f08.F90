! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Group_difference_f08 PMPI_Group_difference_f08
#endif

subroutine MPI_Group_difference_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   use :: ompi_mpifh_bindings, only : ompi_group_difference_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1
   TYPE(MPI_Group), INTENT(IN) :: group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_group_difference_f(group1%MPI_VAL,group2%MPI_VAL,newgroup%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Group_difference_f08
