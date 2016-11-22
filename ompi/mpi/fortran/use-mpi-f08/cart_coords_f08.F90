! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Cart_coords_f08(comm,rank,maxdims,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_cart_coords_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxdims
   INTEGER, INTENT(OUT) :: coords(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_cart_coords_f(comm%MPI_VAL,rank,maxdims,coords,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Cart_coords_f08
