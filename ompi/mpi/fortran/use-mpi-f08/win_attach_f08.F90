! -*- f90 -*-
!
! Copyright (c) 2015-2018 Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Win_attach_f08(win,base,size,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   use :: ompi_mpifh_bindings, only : ompi_win_attach_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_attach_f(win%MPI_VAL,base,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_attach_f08
