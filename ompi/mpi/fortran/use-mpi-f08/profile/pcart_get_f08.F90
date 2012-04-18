! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Cart_get_f08(comm,maxdims,dims,periods,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_cart_get_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxdims
   INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
   LOGICAL, INTENT(OUT) :: periods(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_cart_get_f(comm%MPI_VAL,maxdims,dims,periods,coords,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Cart_get_f08
