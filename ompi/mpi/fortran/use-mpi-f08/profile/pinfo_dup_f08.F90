! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Info_dup_f08(info,newinfo,ierror)
   use :: mpi_f08_types, only : MPI_Info
   use :: mpi_f08, only : ompi_info_dup_f
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Info), INTENT(OUT) :: newinfo
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_info_dup_f(info%MPI_VAL,newinfo%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Info_dup_f08
