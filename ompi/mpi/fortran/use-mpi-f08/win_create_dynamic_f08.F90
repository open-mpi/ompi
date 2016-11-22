! -*- f90 -*-
!
! Copyright (c) 2015      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Win_create_dynamic_f08(info,comm,win,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win
   use :: mpi_f08, only : ompi_win_create_dynamic_f
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Win), INTENT(OUT) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_create_dynamic_f(info%MPI_VAL,&
                          comm%MPI_VAL,win%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_create_dynamic_f08
