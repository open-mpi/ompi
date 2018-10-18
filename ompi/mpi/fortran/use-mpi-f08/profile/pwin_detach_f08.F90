! -*- f90 -*-
!
! Copyright (c) 2015-2018 Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Win_detach_f08(win,base,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   use :: ompi_mpifh_bindings, only : ompi_win_detach_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: base
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_detach_f(win%MPI_VAL,base,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Win_detach_f08
