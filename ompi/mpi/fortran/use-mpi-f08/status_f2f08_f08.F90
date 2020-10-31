! -*- f90 -*-
!
! Copyright (c) 2020      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Status_f2f08_f08(f_status,f08_status,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_STATUS_SIZE
   use :: ompi_mpifh_bindings, only : ompi_status_f2f08_f
   implicit none
   INTEGER, INTENT(IN) :: f_status(MPI_STATUS_SIZE)
   TYPE(MPI_Status), INTENT(OUT) :: f08_status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_status_f2f08_f(f_status, f08_status, c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Status_f2f08_f08
