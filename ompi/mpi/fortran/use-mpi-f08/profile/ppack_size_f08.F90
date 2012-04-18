! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Pack_size_f08(incount,datatype,comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   use :: mpi_f08, only : ompi_pack_size_f
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_pack_size_f(incount,datatype%MPI_VAL,comm%MPI_VAL,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Pack_size_f08
