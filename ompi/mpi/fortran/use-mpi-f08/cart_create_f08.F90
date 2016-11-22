! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Cart_create_f08(comm_old,ndims,dims,periods,reorder,comm_cart,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Cart_create
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: ndims
   INTEGER, INTENT(IN) :: dims(ndims)
   LOGICAL, INTENT(IN) :: periods(ndims), reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Cart_create(comm_old%MPI_VAL,ndims,dims,periods,&
                           reorder,comm_cart%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Cart_create_f08
