! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2019-2022 Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Session_get_nth_pset_f08(session, info, n, pset_len, pset_name, ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Info, MPI_INFO_NULL
   use :: ompi_mpifh_bindings, only : ompi_session_get_nth_pset_f
   implicit none
   TYPE(MPI_Session), INTENT(IN) :: session
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(IN) :: n
   INTEGER, OPTIONAL, INTENT(INOUT) :: pset_len
   CHARACTER(LEN=*), INTENT(OUT) :: pset_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_session_get_nth_pset_f(session%MPI_VAL, MPI_INFO_NULL%MPI_VAL, n,     &
                                    pset_len, pset_name, c_ierror, len(pset_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Session_get_nth_pset_f08
