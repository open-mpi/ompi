! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2019-2022 Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Session_set_errhandler_f08(session,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Errhandler
   use :: ompi_mpifh_bindings, only : ompi_session_set_errhandler_f
   implicit none
   TYPE(MPI_Session), INTENT(IN) :: session
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_session_set_errhandler_f(session%MPI_VAL,errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Session_set_errhandler_f08
