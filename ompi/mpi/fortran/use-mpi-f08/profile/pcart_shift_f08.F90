! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Cart_shift_f08(comm,direction,disp,rank_source,rank_dest,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_cart_shift_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: direction, disp
   INTEGER, INTENT(OUT) :: rank_source, rank_dest
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_cart_shift_f(comm%MPI_VAL,direction,disp,rank_source,rank_dest,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Cart_shift_f08
