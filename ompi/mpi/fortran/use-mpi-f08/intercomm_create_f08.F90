! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Intercomm_create_f08(local_comm,local_leader,peer_comm,&
                                    remote_leader,tag,newintercomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_intercomm_create_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: local_comm, peer_comm
   INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
   TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_intercomm_create_f(local_comm%MPI_VAL,local_leader,peer_comm%MPI_VAL,&
                                remote_leader,tag,newintercomm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Intercomm_create_f08
