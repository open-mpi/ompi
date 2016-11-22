! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Cart_get_f08(comm,maxdims,dims,periods,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Cart_get
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxdims
   INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
   LOGICAL, INTENT(OUT) :: periods(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Cart_get(comm%MPI_VAL,maxdims,dims,periods,coords,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Cart_get_f08
