! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_connect_f08(port_name,info,root,comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   use :: mpi_f08, only : ompi_comm_connect_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_connect_f(port_name,info%MPI_VAL,root,comm%MPI_VAL,newcomm%MPI_VAL, &
                            c_ierror,len(port_name))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_connect_f08
