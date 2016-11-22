! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Dist_graph_create_f08(comm_old,n,sources,degrees,destinations,&
                                     weights,info,reorder,comm_dist_graph,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   ! See note in ompi/mpi/fortran/use-mpi-f08/iprobe_f08.F90 about why
   ! we "use mpi" here.
   use :: mpi, only : PMPI_Dist_graph_create
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: sources(*), degrees(*), destinations(*), weights(*)
   TYPE(MPI_Info), INTENT(IN) :: info
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in ompi/mpi/fortran/use-mpi-f08/iprobe_f08.F90 about why
   ! we call a PMPI_* subroutine here
   call PMPI_Dist_graph_create(comm_old%MPI_VAL,n,sources,degrees,destinations,&
                                 weights,info%MPI_VAL,reorder,comm_dist_graph%MPI_VAL,&
                                 c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Dist_graph_create_f08
