! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Dist_graph_neighbors_f08(comm,maxindegree,sources,sourceweights,&
                                         maxoutdegree,destinations,destweights,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_dist_graph_neighbors_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
   INTEGER, INTENT(OUT) :: sources(*), destinations(*)
   INTEGER :: sourceweights(*), destweights(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_dist_graph_neighbors_f(comm%MPI_VAL,maxindegree,sources,sourceweights,&
                                    maxoutdegree,destinations,destweights,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Dist_graph_neighbors_f08
