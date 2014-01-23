! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Dist_graph_neighbors_count_f08(comm,indegree,outdegree,weighted,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Dist_graph_neighbors_count
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: indegree, outdegree
   LOGICAL, INTENT(OUT) :: weighted
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Dist_graph_neighbors_count(comm%MPI_VAL,indegree,outdegree,weighted,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Dist_graph_neighbors_count_f08
