! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Neighbor_alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,&
                                      recvbuf,recvcounts,rdispls,recvtypes,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_neighbor_alltoallw_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
   TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_neighbor_alltoallw_f(sendbuf,sendcounts,sdispls,sendtypes(1)%MPI_VAL,&
                                  recvbuf,recvcounts,rdispls,recvtypes(1)%MPI_VAL,&
                                  comm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Neighbor_alltoallw_f08
