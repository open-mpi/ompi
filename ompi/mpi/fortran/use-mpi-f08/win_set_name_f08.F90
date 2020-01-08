! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Win_set_name_f08 PMPI_Win_set_name_f08
#endif

subroutine MPI_Win_set_name_f08(win,win_name,ierror)
   use :: mpi_f08_types, only : MPI_Win
   use :: ompi_mpifh_bindings, only : ompi_win_set_name_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   CHARACTER(LEN=*), INTENT(IN) :: win_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_set_name_f(win%MPI_VAL,win_name,c_ierror,len(win_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_set_name_f08
