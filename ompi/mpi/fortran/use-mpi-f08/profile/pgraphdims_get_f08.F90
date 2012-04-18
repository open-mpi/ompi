! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Graphdims_get_f08(comm,nnodes,nedges,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_graphdims_get_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: nnodes, nedges
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_graphdims_get_f(comm%MPI_VAL,nnodes,nedges,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Graphdims_get_f08
