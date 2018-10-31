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

subroutine MPI_Intercomm_create_from_groups_f08(local_group, local_leader, remote_group, &
                                                remote_leader, stringtag, info, errhandler, &
                                                newintercomm, ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group, MPI_Errhandler, MPI_Info
   use :: ompi_mpifh_bindings, only : ompi_intercomm_create_from_groups_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: local_group, remote_group
   INTEGER, INTENT(IN):: local_leader, remote_leader
   CHARACTER(LEN=*), INTENT(IN) :: stringtag
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_intercomm_create_from_groups_f(local_group%MPI_VAL, local_leader, &
                                            remote_group%MPI_VAL,  &
                                            remote_leader, stringtag, info%MPI_VAL, &
                                            errhandler%MPI_VAL, &
                                            newintercomm%MPI_VAL, c_ierror, len(stringtag))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Intercomm_create_from_groups_f08

