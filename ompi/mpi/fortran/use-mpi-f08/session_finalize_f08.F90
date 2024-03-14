! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2019      Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Session_finalize_f08(session,ierror)
   use :: mpi_f08_types, only : MPI_Session
   use :: ompi_mpifh_bindings, only : ompi_session_finalize_f
   implicit none
   TYPE(MPI_Session), INTENT(OUT) :: session
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_session_finalize_f(session%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Session_finalize_f08

