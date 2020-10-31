! -*- f90 -*-
!
! Copyright (c) 2020      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Status_f082f_f08(f08_status,f_status,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_STATUS_SIZE
   use :: ompi_mpifh_bindings, only : ompi_status_f082f_f
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: f08_status
   INTEGER, INTENT(OUT) :: f_status(MPI_STATUS_SIZE)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_status_f082f_f(f08_status, f_status, c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Status_f082f_f08
