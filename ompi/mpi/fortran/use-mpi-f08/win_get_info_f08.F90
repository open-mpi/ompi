! -*- f90 -*-
!
! Copyright (c) 2015-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Win_get_info_f08 PMPI_Win_get_info_f08
#endif

subroutine MPI_Win_get_info_f08(win,info,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Info
   use :: ompi_mpifh_bindings, only : ompi_win_get_info_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Info), INTENT(OUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_get_info_f(win%MPI_VAL,info%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_get_info_f08
