! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Win_shared_query_f08(win, rank, size, disp_unit, baseptr,&
      ierror)
   USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_win_shared_query_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) ::  win
   INTEGER, INTENT(IN) ::  rank
   INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::  size
   INTEGER, INTENT(OUT) ::  disp_unit
   TYPE(C_PTR), INTENT(OUT) ::  baseptr
   INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
   integer :: c_ierror

   call ompi_win_shared_query_f(win%MPI_VAL, rank, size, disp_unit, baseptr, c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_shared_query_f08
