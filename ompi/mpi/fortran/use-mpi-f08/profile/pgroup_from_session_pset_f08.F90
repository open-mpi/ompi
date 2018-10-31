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

subroutine PMPI_Group_from_session_pset_f08(session, pset_name, newgroup, ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Group
   use :: ompi_mpifh_bindings, only : ompi_group_from_session_pset_f
   implicit none
   TYPE(MPI_Session), INTENT(IN) :: session
   CHARACTER(LEN=*), INTENT(IN) :: pset_name
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_group_from_session_pset_f(session%MPI_VAL, pset_name, newgroup%MPI_VAL, c_ierror, len(pset_name))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Group_from_session_pset_f08

