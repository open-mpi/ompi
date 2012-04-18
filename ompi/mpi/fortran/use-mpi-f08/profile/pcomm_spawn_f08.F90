! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_spawn_f08(command,argv,maxprocs,info,root,comm,intercomm, &
                               array_of_errcodes,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   use :: mpi_f08, only : ompi_comm_spawn_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: command, argv
   INTEGER, INTENT(IN) :: maxprocs, root
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   INTEGER :: array_of_errcodes(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_spawn_f(command,argv,maxprocs,                            &
                          info%MPI_VAL,root,comm%MPI_VAL,intercomm%MPI_VAL, &
                          array_of_errcodes,c_ierror,                       &
                          len(command), len(argv))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_spawn_f08
