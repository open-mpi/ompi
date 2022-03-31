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

subroutine PMPI_Comm_create_from_group_f08(group, stringtag, info, errhandler, newcomm, ierror)
   use :: mpi_f08_types, only : MPI_Session, MPI_Group, MPI_Errhandler, MPI_Info, MPI_Comm
   use :: ompi_mpifh_bindings, only : ompi_comm_create_from_group_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   CHARACTER(LEN=*), INTENT(IN) :: stringtag
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_create_from_group_f(group%MPI_VAL, stringtag, info%MPI_VAL, errhandler%MPI_VAL, &
                                      newcomm%MPI_VAL, c_ierror, len(stringtag))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_create_from_group_f08

