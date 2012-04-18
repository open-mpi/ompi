! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Sendrecv_f08(sendbuf,sendcount,sendtype,dest,sendtag,recvbuf, &
                        recvcount,recvtype,source,recvtag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Status
   use :: mpi_f08, only : ompi_sendrecv_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype
   TYPE(MPI_Datatype), INTENT(IN) :: recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_sendrecv_f(sendbuf,sendcount,sendtype%MPI_VAL,dest,sendtag,recvbuf, &
                        recvcount,recvtype%MPI_VAL,source,recvtag,comm%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Sendrecv_f08
