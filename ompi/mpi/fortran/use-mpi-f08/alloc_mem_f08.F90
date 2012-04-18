! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Alloc_mem_f08(size,info,baseptr,ierror)
   use, intrinsic :: ISO_C_BINDING, only : C_PTR
   use :: mpi_f08_types, only : MPI_Info, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_alloc_mem_f
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(C_PTR), INTENT(OUT) :: baseptr
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_alloc_mem_f(size,info%MPI_VAL,baseptr,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Alloc_mem_f08
