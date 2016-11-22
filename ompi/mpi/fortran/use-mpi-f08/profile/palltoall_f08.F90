! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,&
                             recvcount,recvtype,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   use :: mpi_f08, only : ompi_alltoall_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype
   TYPE(MPI_Datatype), INTENT(IN) :: recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_alltoall_f(sendbuf,sendcount,sendtype%MPI_VAL,recvbuf,&
                        recvcount,recvtype%MPI_VAL,comm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Alltoall_f08
