! -*- f90 -*-
!
! Copyright (c) 2015-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Win_get_info_f08(win,info_used,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Info
   use :: ompi_mpifh_bindings, only : ompi_win_get_info_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Info), INTENT(OUT) :: info_used
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_get_info_f(win%MPI_VAL,info_used%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_get_info_f08
