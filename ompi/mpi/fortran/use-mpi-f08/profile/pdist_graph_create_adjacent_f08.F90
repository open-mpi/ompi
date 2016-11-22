! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Dist_graph_create_adjacent_f08(comm_old,indegree,sources,sourceweights,&
                                               outdegree,destinations,destweights,info,&
                                               reorder,comm_dist_graph,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Dist_graph_create_adjacent
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: indegree, outdegree
   INTEGER, INTENT(IN) :: sources(*), sourceweights(*), destinations(*), destweights(*)
   TYPE(MPI_Info), INTENT(IN) :: info
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Dist_graph_create_adjacent(comm_old%MPI_VAL,indegree,sources,&
                                          sourceweights,outdegree,destinations,&
                                          destweights,info%MPI_VAL,&
                                          reorder,comm_dist_graph%MPI_VAL,ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Dist_graph_create_adjacent_f08
