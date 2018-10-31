! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2019-2021 Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

#include "mpi-f08-rename.h"

subroutine MPI_Session_init_f08(info,errhandler,session,ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Info, MPI_Errhandler
   use :: ompi_mpifh_bindings, only : ompi_session_init_f
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   TYPE(MPI_Session), INTENT(OUT) :: session
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_session_init_f(info%MPI_VAL,errhandler%MPI_VAL,session%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Session_init_f08

