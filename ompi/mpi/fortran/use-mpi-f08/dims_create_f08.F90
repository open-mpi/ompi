! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Dims_create_f08(nnodes,ndims,dims,ierror)
   use :: mpi_f08, only : ompi_dims_create_f
   implicit none
   INTEGER, INTENT(IN) :: nnodes, ndims
   INTEGER, INTENT(INOUT) :: dims(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_dims_create_f(nnodes,ndims,dims,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Dims_create_f08
