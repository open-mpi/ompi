! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Graph_map_f08(comm,nnodes,index,edges,newrank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_graph_map_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: nnodes
   INTEGER, INTENT(IN) :: index(*), edges(*)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_graph_map_f(comm%MPI_VAL,nnodes,index,edges,newrank,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Graph_map_f08
