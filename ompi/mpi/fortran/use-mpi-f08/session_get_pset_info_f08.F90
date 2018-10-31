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

subroutine MPI_Session_get_pset_info_f08(session, pset_name, info, ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Info
   use :: ompi_mpifh_bindings, only : ompi_session_get_pset_info_f
   implicit none
   TYPE(MPI_Session), INTENT(IN) :: session
   CHARACTER(LEN=*), INTENT(IN) :: pset_name
   TYPE(MPI_Info), INTENT(OUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_session_get_pset_info_f(session%MPI_VAL, pset_name, info%MPI_VAL, c_ierror, len(pset_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Session_get_pset_info_f08

