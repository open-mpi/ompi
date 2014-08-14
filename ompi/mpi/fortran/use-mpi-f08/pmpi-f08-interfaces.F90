! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2012      The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2012      Inria.  All rights reserved.
! $COPYRIGHT$
!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the name for tools ("MPI_Init_f08") and the back-end implementation
! name (e.g., "MPI_Init_f08").

#include "ompi/mpi/fortran/configure-fortran-output.h"

module pmpi_f08_interfaces

interface  PMPI_Bsend
subroutine PMPI_Bsend_f08(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Bsend_f08
end interface  PMPI_Bsend

interface  PMPI_Bsend_init
subroutine PMPI_Bsend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Bsend_init_f08
end interface  PMPI_Bsend_init

interface  PMPI_Buffer_attach
subroutine PMPI_Buffer_attach_f08(buffer,size,ierror)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !$PRAGMA IGNORE_TKR buffer
   !DIR$ IGNORE_TKR buffer
   !IBM* IGNORE_TKR buffer
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buffer
   INTEGER, INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Buffer_attach_f08
end interface  PMPI_Buffer_attach

interface  PMPI_Buffer_detach
subroutine PMPI_Buffer_detach_f08(buffer_addr,size,ierror)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer_addr
   !$PRAGMA IGNORE_TKR buffer_addr
   !DIR$ IGNORE_TKR buffer_addr
   !IBM* IGNORE_TKR buffer_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buffer_addr
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Buffer_detach_f08
end interface  PMPI_Buffer_detach

interface  PMPI_Cancel
subroutine PMPI_Cancel_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cancel_f08
end interface  PMPI_Cancel

interface  PMPI_Get_count
subroutine PMPI_Get_count_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_count_f08
end interface  PMPI_Get_count

interface  PMPI_Ibsend
subroutine PMPI_Ibsend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ibsend_f08
end interface  PMPI_Ibsend

interface  PMPI_Iprobe
subroutine PMPI_Iprobe_f08(source,tag,comm,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iprobe_f08
end interface  PMPI_Iprobe

interface  PMPI_Irecv
subroutine PMPI_Irecv_f08(buf,count,datatype,source,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Irecv_f08
end interface  PMPI_Irecv

interface  PMPI_Irsend
subroutine PMPI_Irsend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Irsend_f08
end interface  PMPI_Irsend

interface  PMPI_Isend
subroutine PMPI_Isend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Isend_f08
end interface  PMPI_Isend

interface  PMPI_Issend
subroutine PMPI_Issend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Issend_f08
end interface  PMPI_Issend

interface  PMPI_Probe
subroutine PMPI_Probe_f08(source,tag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Probe_f08
end interface  PMPI_Probe

interface  PMPI_Recv
subroutine PMPI_Recv_f08(buf,count,datatype,source,tag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Status
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Recv_f08
end interface  PMPI_Recv

interface  PMPI_Recv_init
subroutine PMPI_Recv_init_f08(buf,count,datatype,source,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Recv_init_f08
end interface  PMPI_Recv_init

interface  PMPI_Request_free
subroutine PMPI_Request_free_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Request_free_f08
end interface  PMPI_Request_free

interface  PMPI_Request_get_status
subroutine PMPI_Request_get_status_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Request_get_status_f08
end interface  PMPI_Request_get_status

interface  PMPI_Rsend
subroutine PMPI_Rsend_f08(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Rsend_f08
end interface  PMPI_Rsend

interface  PMPI_Rsend_init
subroutine PMPI_Rsend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Rsend_init_f08
end interface  PMPI_Rsend_init

interface  PMPI_Send
subroutine PMPI_Send_f08(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Send_f08
end interface  PMPI_Send

interface  PMPI_Sendrecv
subroutine PMPI_Sendrecv_f08(sendbuf,sendcount,sendtype,dest,sendtag,recvbuf, &
                            recvcount,recvtype,source,recvtag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Status
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Sendrecv_f08
end interface  PMPI_Sendrecv

interface  PMPI_Sendrecv_replace
subroutine PMPI_Sendrecv_replace_f08(buf,count,datatype,dest,sendtag,source,recvtag, &
                                    comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Status
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Sendrecv_replace_f08
end interface  PMPI_Sendrecv_replace

interface  PMPI_Send_init
subroutine PMPI_Send_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Send_init_f08
end interface  PMPI_Send_init

interface  PMPI_Ssend
subroutine PMPI_Ssend_f08(buf,count,datatype,dest,tag,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ssend_f08
end interface  PMPI_Ssend

interface  PMPI_Ssend_init
subroutine PMPI_Ssend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ssend_init_f08
end interface  PMPI_Ssend_init

interface  PMPI_Start
subroutine PMPI_Start_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Start_f08
end interface  PMPI_Start

interface  PMPI_Startall
subroutine PMPI_Startall_f08(count,array_of_requests,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Startall_f08
end interface  PMPI_Startall

interface  PMPI_Test
subroutine PMPI_Test_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Test_f08
end interface  PMPI_Test

interface  PMPI_Testall
subroutine PMPI_Testall_f08(count,array_of_requests,flag,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Testall_f08
end interface  PMPI_Testall

interface  PMPI_Testany
subroutine PMPI_Testany_f08(count,array_of_requests,index,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Testany_f08
end interface  PMPI_Testany

interface  PMPI_Testsome
subroutine PMPI_Testsome_f08(incount,array_of_requests,outcount, &
                        array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Testsome_f08
end interface  PMPI_Testsome

interface  PMPI_Test_cancelled
subroutine PMPI_Test_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Test_cancelled_f08
end interface  PMPI_Test_cancelled

interface  PMPI_Wait
subroutine PMPI_Wait_f08(request,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Wait_f08
end interface  PMPI_Wait

interface  PMPI_Waitall
subroutine PMPI_Waitall_f08(count,array_of_requests,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Waitall_f08
end interface  PMPI_Waitall

interface  PMPI_Waitany
subroutine PMPI_Waitany_f08(count,array_of_requests,index,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Waitany_f08
end interface  PMPI_Waitany

interface  PMPI_Waitsome
subroutine PMPI_Waitsome_f08(incount,array_of_requests,outcount, &
                        array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Waitsome_f08
end interface  PMPI_Waitsome

interface  PMPI_Get_address
subroutine PMPI_Get_address_f08(location,address,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: location
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: location
   !$PRAGMA IGNORE_TKR location
   !DIR$ IGNORE_TKR location
   !IBM* IGNORE_TKR location
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: location
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: address
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_address_f08
end interface  PMPI_Get_address

interface  PMPI_Get_elements
subroutine PMPI_Get_elements_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_elements_f08
end interface  PMPI_Get_elements

interface  PMPI_Get_elements_x
subroutine PMPI_Get_elements_x_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_elements_x_f08
end interface  PMPI_Get_elements_x

interface  PMPI_Pack
subroutine PMPI_Pack_f08(inbuf,incount,datatype,outbuf,outsize,position,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !$PRAGMA IGNORE_TKR inbuf, outbuf
   !DIR$ IGNORE_TKR inbuf, outbuf
   !IBM* IGNORE_TKR inbuf, outbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: outbuf
   INTEGER, INTENT(IN) :: incount, outsize
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(INOUT) :: position
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Pack_f08
end interface  PMPI_Pack

interface  PMPI_Pack_external
subroutine PMPI_Pack_external_f08(datarep,inbuf,incount,datatype,outbuf,outsize, &
                                 position,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !$PRAGMA IGNORE_TKR inbuf, outbuf
   !DIR$ IGNORE_TKR inbuf, outbuf
   !IBM* IGNORE_TKR inbuf, outbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: outbuf
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: outsize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Pack_external_f08
end interface  PMPI_Pack_external

interface  PMPI_Pack_external_size
subroutine PMPI_Pack_external_size_f08(datarep,incount,datatype,size,ierror &
           )
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: incount
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Pack_external_size_f08
end interface  PMPI_Pack_external_size

interface  PMPI_Pack_size
subroutine PMPI_Pack_size_f08(incount,datatype,comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Pack_size_f08
end interface  PMPI_Pack_size

interface  PMPI_Type_commit
subroutine PMPI_Type_commit_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_commit_f08
end interface  PMPI_Type_commit

interface  PMPI_Type_contiguous
subroutine PMPI_Type_contiguous_f08(count,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_contiguous_f08
end interface  PMPI_Type_contiguous

interface  PMPI_Type_create_darray
subroutine PMPI_Type_create_darray_f08(size,rank,ndims,array_of_gsizes, &
                    array_of_distribs,array_of_dargs,array_of_psizes,order, &
                    oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: size, rank, ndims, order
   INTEGER, INTENT(IN) :: array_of_gsizes(ndims), array_of_distribs(ndims)
   INTEGER, INTENT(IN) :: array_of_dargs(ndims), array_of_psizes(ndims)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_darray_f08
end interface  PMPI_Type_create_darray

interface  PMPI_Type_create_hindexed
subroutine PMPI_Type_create_hindexed_f08(count,array_of_blocklengths, &
                                        array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_hindexed_f08
end interface  PMPI_Type_create_hindexed

interface  PMPI_Type_create_hvector
subroutine PMPI_Type_create_hvector_f08(count,blocklength,stride,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: stride
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_hvector_f08
end interface  PMPI_Type_create_hvector

interface  PMPI_Type_create_indexed_block
subroutine PMPI_Type_create_indexed_block_f08(count,blocklength, &
                           array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER, INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_indexed_block_f08
end interface  PMPI_Type_create_indexed_block

interface  PMPI_Type_create_hindexed_block
subroutine PMPI_Type_create_hindexed_block_f08(count,blocklength, &
                           array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_hindexed_block_f08
end interface  PMPI_Type_create_hindexed_block

interface  PMPI_Type_create_resized
subroutine PMPI_Type_create_resized_f08(oldtype,lb,extent,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_resized_f08
end interface  PMPI_Type_create_resized

interface  PMPI_Type_create_struct
subroutine PMPI_Type_create_struct_f08(count,array_of_blocklengths, &
                           array_of_displacements,array_of_types,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(count)
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_struct_f08
end interface  PMPI_Type_create_struct

interface  PMPI_Type_create_subarray
subroutine PMPI_Type_create_subarray_f08(ndims,array_of_sizes,array_of_subsizes, &
                    array_of_starts,order,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: ndims, order
   INTEGER, INTENT(IN) :: array_of_sizes(ndims), array_of_subsizes(ndims)
   INTEGER, INTENT(IN) :: array_of_starts(ndims)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_subarray_f08
end interface  PMPI_Type_create_subarray

interface  PMPI_Type_dup
subroutine PMPI_Type_dup_f08(oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_dup_f08
end interface  PMPI_Type_dup

interface  PMPI_Type_free
subroutine PMPI_Type_free_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_free_f08
end interface  PMPI_Type_free

interface  PMPI_Type_get_contents
subroutine PMPI_Type_get_contents_f08(datatype,max_integers,max_addresses,max_datatypes, &
                                     array_of_integers,array_of_addresses,array_of_datatypes, &
                                     ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
   INTEGER, INTENT(OUT) :: array_of_integers(max_integers)
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: array_of_addresses(max_addresses)
   TYPE(MPI_Datatype), INTENT(OUT) :: array_of_datatypes(max_datatypes)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_contents_f08
end interface  PMPI_Type_get_contents

interface  PMPI_Type_get_envelope
subroutine PMPI_Type_get_envelope_f08(datatype,num_integers,num_addresses,num_datatypes, &
                                     combiner,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes, combiner
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_envelope_f08
end interface  PMPI_Type_get_envelope

interface  PMPI_Type_get_extent
subroutine PMPI_Type_get_extent_f08(datatype,lb,extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_extent_f08
end interface  PMPI_Type_get_extent

interface  PMPI_Type_get_extent_x
subroutine PMPI_Type_get_extent_x_f08(datatype,lb,extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: lb, extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_extent_x_f08
end interface  PMPI_Type_get_extent_x

interface  PMPI_Type_get_true_extent
subroutine PMPI_Type_get_true_extent_f08(datatype,true_lb,true_extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_true_extent_f08
end interface  PMPI_Type_get_true_extent

interface  PMPI_Type_get_true_extent_x
subroutine PMPI_Type_get_true_extent_x_f08(datatype,true_lb,true_extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_true_extent_x_f08
end interface  PMPI_Type_get_true_extent_x

interface  PMPI_Type_indexed
subroutine PMPI_Type_indexed_f08(count,array_of_blocklengths, &
                                array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count), array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_indexed_f08
end interface  PMPI_Type_indexed

interface  PMPI_Type_size
subroutine PMPI_Type_size_f08(datatype,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_size_f08
end interface  PMPI_Type_size

interface  PMPI_Type_size_x
subroutine PMPI_Type_size_x_f08(datatype,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_size_x_f08
end interface  PMPI_Type_size_x

interface  PMPI_Type_vector
subroutine PMPI_Type_vector_f08(count,blocklength,stride,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength, stride
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_vector_f08
end interface  PMPI_Type_vector

interface  PMPI_Unpack
subroutine PMPI_Unpack_f08(inbuf,insize,position,outbuf,outcount,datatype,comm, &
                          ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !$PRAGMA IGNORE_TKR inbuf, outbuf
   !DIR$ IGNORE_TKR inbuf, outbuf
   !IBM* IGNORE_TKR inbuf, outbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: outbuf
   INTEGER, INTENT(IN) :: insize, outcount
   INTEGER, INTENT(INOUT) :: position
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Unpack_f08
end interface  PMPI_Unpack

interface  PMPI_Unpack_external
subroutine PMPI_Unpack_external_f08(datarep,inbuf,insize,position,outbuf,outcount, &
                                   datatype,ierror &
           )
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
   !$PRAGMA IGNORE_TKR inbuf, outbuf
   !DIR$ IGNORE_TKR inbuf, outbuf
   !IBM* IGNORE_TKR inbuf, outbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: outbuf
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: insize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, INTENT(IN) :: outcount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Unpack_external_f08
end interface  PMPI_Unpack_external

interface  PMPI_Allgather
subroutine PMPI_Allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Allgather_f08
end interface  PMPI_Allgather

interface  PMPI_Iallgather
subroutine PMPI_Iallgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iallgather_f08
end interface  PMPI_Iallgather

interface  PMPI_Allgatherv
subroutine PMPI_Allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Allgatherv_f08
end interface  PMPI_Allgatherv

interface  PMPI_Iallgatherv
subroutine PMPI_Iallgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iallgatherv_f08
end interface  PMPI_Iallgatherv

interface  PMPI_Allreduce
subroutine PMPI_Allreduce_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Allreduce_f08
end interface  PMPI_Allreduce

interface  PMPI_Iallreduce
subroutine PMPI_Iallreduce_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iallreduce_f08
end interface  PMPI_Iallreduce

interface  PMPI_Alltoall
subroutine PMPI_Alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Alltoall_f08
end interface  PMPI_Alltoall

interface  PMPI_Ialltoall
subroutine PMPI_Ialltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ialltoall_f08
end interface  PMPI_Ialltoall

interface  PMPI_Alltoallv
subroutine PMPI_Alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Alltoallv_f08
end interface  PMPI_Alltoallv

interface  PMPI_Ialltoallv
subroutine PMPI_Ialltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ialltoallv_f08
end interface  PMPI_Ialltoallv

interface  PMPI_Alltoallw
subroutine PMPI_Alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*), recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Alltoallw_f08
end interface  PMPI_Alltoallw

interface  PMPI_Ialltoallw
subroutine PMPI_Ialltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*), recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ialltoallw_f08
end interface  PMPI_Ialltoallw

interface  PMPI_Barrier
subroutine PMPI_Barrier_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Barrier_f08
end interface  PMPI_Barrier

interface  PMPI_Ibarrier
subroutine PMPI_Ibarrier_f08(comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ibarrier_f08
end interface  PMPI_Ibarrier

interface  PMPI_Bcast
subroutine PMPI_Bcast_f08(buffer,count,datatype,root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !$PRAGMA IGNORE_TKR buffer
   !DIR$ IGNORE_TKR buffer
   !IBM* IGNORE_TKR buffer
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
   INTEGER, INTENT(IN) :: count, root
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Bcast_f08
end interface  PMPI_Bcast

interface  PMPI_Ibcast
subroutine PMPI_Ibcast_f08(buffer,count,datatype,root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !$PRAGMA IGNORE_TKR buffer
   !DIR$ IGNORE_TKR buffer
   !IBM* IGNORE_TKR buffer
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
   INTEGER, INTENT(IN) :: count, root
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ibcast_f08
end interface  PMPI_Ibcast

interface  PMPI_Exscan
subroutine PMPI_Exscan_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Exscan_f08
end interface  PMPI_Exscan

interface  PMPI_Iexscan
subroutine PMPI_Iexscan_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iexscan_f08
end interface  PMPI_Iexscan

interface  PMPI_Gather
subroutine PMPI_Gather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                          root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Gather_f08
end interface  PMPI_Gather

interface  PMPI_Igather
subroutine PMPI_Igather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                          root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Igather_f08
end interface  PMPI_Igather

interface  PMPI_Gatherv
subroutine PMPI_Gatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                           recvtype,root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, root
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Gatherv_f08
end interface  PMPI_Gatherv

interface  PMPI_Igatherv
subroutine PMPI_Igatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                           recvtype,root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, root
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Igatherv_f08
end interface  PMPI_Igatherv

interface  PMPI_Op_commutative
subroutine PMPI_Op_commutative_f08(op,commute,ierror)
   use :: mpi_f08_types, only : MPI_Op
   implicit none
   TYPE(MPI_Op), INTENT(IN) :: op
   LOGICAL, INTENT(OUT) :: commute
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Op_commutative_f08
end interface  PMPI_Op_commutative

interface  PMPI_Op_create
subroutine PMPI_Op_create_f08(user_fn,commute,op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   use :: mpi_f08_interfaces_callbacks, only : MPI_User_function
   implicit none
   PROCEDURE(MPI_User_function) :: user_fn
   LOGICAL, INTENT(IN) :: commute
   TYPE(MPI_Op), INTENT(OUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Op_create_f08
end interface  PMPI_Op_create

interface  PMPI_Op_free
subroutine PMPI_Op_free_f08(op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   implicit none
   TYPE(MPI_Op), INTENT(INOUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Op_free_f08
end interface  PMPI_Op_free

interface  PMPI_Reduce
subroutine PMPI_Reduce_f08(sendbuf,recvbuf,count,datatype,op,root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count, root
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Reduce_f08
end interface  PMPI_Reduce

interface  PMPI_Ireduce
subroutine PMPI_Ireduce_f08(sendbuf,recvbuf,count,datatype,op,root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count, root
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ireduce_f08
end interface  PMPI_Ireduce

interface  PMPI_Reduce_local
subroutine PMPI_Reduce_local_f08(inbuf,inoutbuf,count,datatype,op,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, inoutbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, inoutbuf
   !$PRAGMA IGNORE_TKR inbuf, inoutbuf
   !DIR$ IGNORE_TKR inbuf, inoutbuf
   !IBM* IGNORE_TKR inbuf, inoutbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: inoutbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Reduce_local_f08
end interface  PMPI_Reduce_local

interface  PMPI_Reduce_scatter
subroutine PMPI_Reduce_scatter_f08(sendbuf,recvbuf,recvcounts,datatype,op,comm, &
                                  ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcounts(*)
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Reduce_scatter_f08
end interface  PMPI_Reduce_scatter

interface  PMPI_Ireduce_scatter
subroutine PMPI_Ireduce_scatter_f08(sendbuf,recvbuf,recvcounts,datatype,op,comm, &
                                  request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcounts(*)
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ireduce_scatter_f08
end interface  PMPI_Ireduce_scatter

interface  PMPI_Reduce_scatter_block
subroutine PMPI_Reduce_scatter_block_f08(sendbuf,recvbuf,recvcount,datatype,op,comm, &
                                        ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Reduce_scatter_block_f08
end interface  PMPI_Reduce_scatter_block

interface  PMPI_Ireduce_scatter_block
subroutine PMPI_Ireduce_scatter_block_f08(sendbuf,recvbuf,recvcount,datatype,op,comm, &
                                        request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ireduce_scatter_block_f08
end interface  PMPI_Ireduce_scatter_block

interface  PMPI_Scan
subroutine PMPI_Scan_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Scan_f08
end interface  PMPI_Scan

interface  PMPI_Iscan
subroutine PMPI_Iscan_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iscan_f08
end interface  PMPI_Iscan

interface  PMPI_Scatter
subroutine PMPI_Scatter_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                           root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Scatter_f08
end interface  PMPI_Scatter

interface  PMPI_Iscatter
subroutine PMPI_Iscatter_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                           root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iscatter_f08
end interface  PMPI_Iscatter

interface  PMPI_Scatterv
subroutine PMPI_Scatterv_f08(sendbuf,sendcounts,displs,sendtype,recvbuf,recvcount, &
                            recvtype,root,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcount, root
   INTEGER, INTENT(IN) :: sendcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Scatterv_f08
end interface  PMPI_Scatterv

interface  PMPI_Iscatterv
subroutine PMPI_Iscatterv_f08(sendbuf,sendcounts,displs,sendtype,recvbuf,recvcount, &
                            recvtype,root,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: recvcount, root
   INTEGER, INTENT(IN) :: sendcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Iscatterv_f08
end interface  PMPI_Iscatterv

interface  PMPI_Comm_compare
subroutine PMPI_Comm_compare_f08(comm1,comm2,result,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm1
   TYPE(MPI_Comm), INTENT(IN) :: comm2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_compare_f08
end interface  PMPI_Comm_compare

interface  PMPI_Comm_create
subroutine PMPI_Comm_create_f08(comm,group,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(IN) :: group
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_create_f08
end interface  PMPI_Comm_create

interface  PMPI_Comm_create_group
subroutine PMPI_Comm_create_group_f08(comm,group,tag,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: tag
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_create_group_f08
end interface  PMPI_Comm_create_group

interface  PMPI_Comm_create_keyval
subroutine PMPI_Comm_create_keyval_f08(comm_copy_attr_fn,comm_delete_attr_fn,comm_keyval, &
                                      extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_delete_attr_function
   implicit none
   PROCEDURE(MPI_Comm_copy_attr_function) :: comm_copy_attr_fn
   PROCEDURE(MPI_Comm_delete_attr_function) :: comm_delete_attr_fn
   INTEGER, INTENT(OUT) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_create_keyval_f08
end interface  PMPI_Comm_create_keyval

interface  PMPI_Comm_delete_attr
subroutine PMPI_Comm_delete_attr_f08(comm,comm_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_delete_attr_f08
end interface  PMPI_Comm_delete_attr

interface  PMPI_Comm_dup
subroutine PMPI_Comm_dup_f08(comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_dup_f08
end interface  PMPI_Comm_dup

interface  PMPI_Comm_dup_with_info
subroutine PMPI_Comm_dup_with_info_f08(comm,info,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_dup_with_info_f08
end interface  PMPI_Comm_dup_with_info

interface  PMPI_Comm_idup
subroutine PMPI_Comm_idup_f08(comm,newcomm,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_idup_f08
end interface  PMPI_Comm_idup

interface  PMPI_Comm_free
subroutine PMPI_Comm_free_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(INOUT) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_free_f08
end interface  PMPI_Comm_free

interface  PMPI_Comm_free_keyval
subroutine PMPI_Comm_free_keyval_f08(comm_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: comm_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_free_keyval_f08
end interface  PMPI_Comm_free_keyval

interface  PMPI_Comm_get_attr
subroutine PMPI_Comm_get_attr_f08(comm,comm_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_get_attr_f08
end interface  PMPI_Comm_get_attr

interface  PMPI_Comm_get_info
subroutine PMPI_Comm_get_info_f08(comm,info_used,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: info_used
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_get_info_f08
end interface  PMPI_Comm_get_info

interface  PMPI_Comm_get_name
subroutine PMPI_Comm_get_name_f08(comm,comm_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: comm_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_get_name_f08
end interface  PMPI_Comm_get_name

interface  PMPI_Comm_group
subroutine PMPI_Comm_group_f08(comm,group,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_group_f08
end interface  PMPI_Comm_group

interface  PMPI_Comm_rank
subroutine PMPI_Comm_rank_f08(comm,rank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_rank_f08
end interface  PMPI_Comm_rank

interface  PMPI_Comm_remote_group
subroutine PMPI_Comm_remote_group_f08(comm,group,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_remote_group_f08
end interface  PMPI_Comm_remote_group

interface  PMPI_Comm_remote_size
subroutine PMPI_Comm_remote_size_f08(comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_remote_size_f08
end interface  PMPI_Comm_remote_size

interface  PMPI_Comm_set_attr
subroutine PMPI_Comm_set_attr_f08(comm,comm_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_set_attr_f08
end interface  PMPI_Comm_set_attr

interface  PMPI_Comm_set_info
subroutine PMPI_Comm_set_info_f08(comm,info,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_set_info_f08
end interface  PMPI_Comm_set_info

interface  PMPI_Comm_set_name
subroutine PMPI_Comm_set_name_f08(comm,comm_name,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: comm_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_set_name_f08
end interface  PMPI_Comm_set_name

interface  PMPI_Comm_size
subroutine PMPI_Comm_size_f08(comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_size_f08
end interface  PMPI_Comm_size

interface  PMPI_Comm_split
subroutine PMPI_Comm_split_f08(comm,color,key,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: color, key
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_split_f08
end interface  PMPI_Comm_split

interface  PMPI_Comm_test_inter
subroutine PMPI_Comm_test_inter_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_test_inter_f08
end interface  PMPI_Comm_test_inter

interface  PMPI_Group_compare
subroutine PMPI_Group_compare_f08(group1,group2,result,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_compare_f08
end interface  PMPI_Group_compare

interface  PMPI_Group_difference
subroutine PMPI_Group_difference_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_difference_f08
end interface  PMPI_Group_difference

interface  PMPI_Group_excl
subroutine PMPI_Group_excl_f08(group,n,ranks,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranks(n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_excl_f08
end interface  PMPI_Group_excl

interface  PMPI_Group_free
subroutine PMPI_Group_free_f08(group,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(INOUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_free_f08
end interface  PMPI_Group_free

interface  PMPI_Group_incl
subroutine PMPI_Group_incl_f08(group,n,ranks,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   INTEGER, INTENT(IN) :: n, ranks(n)
   TYPE(MPI_Group), INTENT(IN) :: group
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_incl_f08
end interface  PMPI_Group_incl

interface  PMPI_Group_intersection
subroutine PMPI_Group_intersection_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_intersection_f08
end interface  PMPI_Group_intersection

interface  PMPI_Group_range_excl
subroutine PMPI_Group_range_excl_f08(group,n,ranges,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranges(3,n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_range_excl_f08
end interface  PMPI_Group_range_excl

interface  PMPI_Group_range_incl
subroutine PMPI_Group_range_incl_f08(group,n,ranges,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranges(3,n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_range_incl_f08
end interface  PMPI_Group_range_incl

interface  PMPI_Group_rank
subroutine PMPI_Group_rank_f08(group,rank,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_rank_f08
end interface  PMPI_Group_rank

interface  PMPI_Group_size
subroutine PMPI_Group_size_f08(group,size,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_size_f08
end interface  PMPI_Group_size

interface  PMPI_Group_translate_ranks
subroutine PMPI_Group_translate_ranks_f08(group1,n,ranks1,group2,ranks2,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks1(n)
   INTEGER, INTENT(OUT) :: ranks2(n)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_translate_ranks_f08
end interface  PMPI_Group_translate_ranks

interface  PMPI_Group_union
subroutine PMPI_Group_union_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Group_union_f08
end interface  PMPI_Group_union

interface  PMPI_Intercomm_create
subroutine PMPI_Intercomm_create_f08(local_comm,local_leader,peer_comm,remote_leader, &
                                    tag,newintercomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: local_comm, peer_comm
   INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
   TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Intercomm_create_f08
end interface  PMPI_Intercomm_create

interface  PMPI_Intercomm_merge
subroutine PMPI_Intercomm_merge_f08(intercomm,high,newintracomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: intercomm
   LOGICAL, INTENT(IN) :: high
   TYPE(MPI_Comm), INTENT(OUT) :: newintracomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Intercomm_merge_f08
end interface  PMPI_Intercomm_merge

interface  PMPI_Type_create_keyval
subroutine PMPI_Type_create_keyval_f08(type_copy_attr_fn,type_delete_attr_fn,type_keyval, &
                                      extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Type_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Type_delete_attr_function
   implicit none
   PROCEDURE(MPI_Type_copy_attr_function) :: type_copy_attr_fn
   PROCEDURE(MPI_Type_delete_attr_function) :: type_delete_attr_fn
   INTEGER, INTENT(OUT) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_keyval_f08
end interface  PMPI_Type_create_keyval

interface  PMPI_Type_delete_attr
subroutine PMPI_Type_delete_attr_f08(datatype,type_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_delete_attr_f08
end interface  PMPI_Type_delete_attr

interface  PMPI_Type_free_keyval
subroutine PMPI_Type_free_keyval_f08(type_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: type_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_free_keyval_f08
end interface  PMPI_Type_free_keyval

interface  PMPI_Type_get_attr
subroutine PMPI_Type_get_attr_f08(datatype,type_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_attr_f08
end interface  PMPI_Type_get_attr

interface  PMPI_Type_get_name
subroutine PMPI_Type_get_name_f08(datatype,type_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: type_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_get_name_f08
end interface  PMPI_Type_get_name

interface  PMPI_Type_set_attr
subroutine PMPI_Type_set_attr_f08(datatype,type_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_set_attr_f08
end interface  PMPI_Type_set_attr

interface  PMPI_Type_set_name
subroutine PMPI_Type_set_name_f08(datatype,type_name,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   CHARACTER(LEN=*), INTENT(IN) :: type_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_set_name_f08
end interface  PMPI_Type_set_name

interface  PMPI_Win_create_keyval
subroutine PMPI_Win_create_keyval_f08(win_copy_attr_fn,win_delete_attr_fn,win_keyval, &
                                     extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_delete_attr_function
   implicit none
   PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
   PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
   INTEGER, INTENT(OUT) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_create_keyval_f08
end interface  PMPI_Win_create_keyval

interface  PMPI_Win_delete_attr
subroutine PMPI_Win_delete_attr_f08(win,win_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_delete_attr_f08
end interface  PMPI_Win_delete_attr

interface  PMPI_Win_free_keyval
subroutine PMPI_Win_free_keyval_f08(win_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_free_keyval_f08
end interface  PMPI_Win_free_keyval

interface  PMPI_Win_get_attr
subroutine PMPI_Win_get_attr_f08(win,win_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_get_attr_f08
end interface  PMPI_Win_get_attr

interface  PMPI_Win_get_name
subroutine PMPI_Win_get_name_f08(win,win_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: win_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_get_name_f08
end interface  PMPI_Win_get_name

interface  PMPI_Win_set_attr
subroutine PMPI_Win_set_attr_f08(win,win_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_set_attr_f08
end interface  PMPI_Win_set_attr

interface  PMPI_Win_set_name
subroutine PMPI_Win_set_name_f08(win,win_name,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   CHARACTER(LEN=*), INTENT(IN) :: win_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_set_name_f08
end interface  PMPI_Win_set_name

interface  PMPI_Cartdim_get
subroutine PMPI_Cartdim_get_f08(comm,ndims,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ndims
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cartdim_get_f08
end interface  PMPI_Cartdim_get

interface  PMPI_Cart_coords
subroutine PMPI_Cart_coords_f08(comm,rank,maxdims,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxdims
   INTEGER, INTENT(OUT) :: coords(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_coords_f08
end interface  PMPI_Cart_coords

interface  PMPI_Cart_create
subroutine PMPI_Cart_create_f08(comm_old,ndims,dims,periods,reorder,comm_cart,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: ndims, dims(ndims)
   LOGICAL, INTENT(IN) :: periods(ndims), reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_create_f08
end interface  PMPI_Cart_create

interface  PMPI_Cart_get
subroutine PMPI_Cart_get_f08(comm,maxdims,dims,periods,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxdims
   INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
   LOGICAL, INTENT(OUT) :: periods(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_get_f08
end interface  PMPI_Cart_get

interface  PMPI_Cart_map
subroutine PMPI_Cart_map_f08(comm,ndims,dims,periods,newrank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: ndims, dims(ndims)
   LOGICAL, INTENT(IN) :: periods(ndims)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_map_f08
end interface  PMPI_Cart_map

interface  PMPI_Cart_rank
subroutine PMPI_Cart_rank_f08(comm,coords,rank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: coords(*)
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_rank_f08
end interface  PMPI_Cart_rank

interface  PMPI_Cart_shift
subroutine PMPI_Cart_shift_f08(comm,direction,disp,rank_source,rank_dest,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: direction, disp
   INTEGER, INTENT(OUT) :: rank_source, rank_dest
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_shift_f08
end interface  PMPI_Cart_shift

interface  PMPI_Cart_sub
subroutine PMPI_Cart_sub_f08(comm,remain_dims,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(IN) :: remain_dims(*)
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Cart_sub_f08
end interface  PMPI_Cart_sub

interface  PMPI_Dims_create
subroutine PMPI_Dims_create_f08(nnodes,ndims,dims,ierror)
   implicit none
   INTEGER, INTENT(IN) :: nnodes, ndims
   INTEGER, INTENT(INOUT) :: dims(ndims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Dims_create_f08
end interface  PMPI_Dims_create

interface  PMPI_Dist_graph_create
subroutine PMPI_Dist_graph_create_f08(comm_old,n,sources,degrees,destinations,weights, &
                                     info,reorder,comm_dist_graph,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: n, sources(n), degrees(n), destinations(*), weights(*)
   TYPE(MPI_Info), INTENT(IN) :: info
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Dist_graph_create_f08
end interface  PMPI_Dist_graph_create

interface  PMPI_Dist_graph_create_adjacent
subroutine PMPI_Dist_graph_create_adjacent_f08(comm_old,indegree,sources,sourceweights, &
                                              outdegree,destinations,destweights,info,reorder, &
                                              comm_dist_graph,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: indegree, sources(indegree), outdegree, destinations(outdegree)
   INTEGER, INTENT(IN) :: sourceweights(indegree), destweights(outdegree)
   TYPE(MPI_Info), INTENT(IN) :: info
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Dist_graph_create_adjacent_f08
end interface  PMPI_Dist_graph_create_adjacent

interface  PMPI_Dist_graph_neighbors
subroutine PMPI_Dist_graph_neighbors_f08(comm,maxindegree,sources,sourceweights, &
                                        maxoutdegree,destinations,destweights,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
   INTEGER, INTENT(OUT) :: sources(maxindegree), destinations(maxoutdegree)
   INTEGER, INTENT(OUT) :: sourceweights(maxindegree), destweights(maxoutdegree)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Dist_graph_neighbors_f08
end interface  PMPI_Dist_graph_neighbors

interface  PMPI_Dist_graph_neighbors_count
subroutine PMPI_Dist_graph_neighbors_count_f08(comm,indegree,outdegree,weighted,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: indegree, outdegree
   LOGICAL, INTENT(OUT) :: weighted
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Dist_graph_neighbors_count_f08
end interface  PMPI_Dist_graph_neighbors_count

interface  PMPI_Graphdims_get
subroutine PMPI_Graphdims_get_f08(comm,nnodes,nedges,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: nnodes, nedges
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graphdims_get_f08
end interface  PMPI_Graphdims_get

interface  PMPI_Graph_create
subroutine PMPI_Graph_create_f08(comm_old,nnodes,index,edges,reorder,comm_graph, &
                                ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graph_create_f08
end interface  PMPI_Graph_create

interface  PMPI_Graph_get
subroutine PMPI_Graph_get_f08(comm,maxindex,maxedges,index,edges,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindex, maxedges
   INTEGER, INTENT(OUT) :: index(maxindex), edges(maxedges)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graph_get_f08
end interface  PMPI_Graph_get

interface  PMPI_Graph_map
subroutine PMPI_Graph_map_f08(comm,nnodes,index,edges,newrank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graph_map_f08
end interface  PMPI_Graph_map

interface  PMPI_Graph_neighbors
subroutine PMPI_Graph_neighbors_f08(comm,rank,maxneighbors,neighbors,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxneighbors
   INTEGER, INTENT(OUT) :: neighbors(maxneighbors)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graph_neighbors_f08
end interface  PMPI_Graph_neighbors

interface  PMPI_Graph_neighbors_count
subroutine PMPI_Graph_neighbors_count_f08(comm,rank,nneighbors,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(OUT) :: nneighbors
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Graph_neighbors_count_f08
end interface  PMPI_Graph_neighbors_count

interface  PMPI_Topo_test
subroutine PMPI_Topo_test_f08(comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Topo_test_f08
end interface  PMPI_Topo_test

! MPI_Wtick is not a wrapper function
!
interface PMPI_Wtick
function  PMPI_Wtick_f08( ) BIND(C,name="MPI_Wtick")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: PMPI_Wtick_f08
end function  PMPI_Wtick_f08
end interface PMPI_Wtick

! MPI_Wtime is not a wrapper function
!
interface PMPI_Wtime
function  PMPI_Wtime_f08( ) BIND(C,name="MPI_Wtime")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: PMPI_Wtime_f08
end function  PMPI_Wtime_f08
end interface PMPI_Wtime

interface  PMPI_Abort
subroutine PMPI_Abort_f08(comm,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Abort_f08
end interface  PMPI_Abort

interface  PMPI_Add_error_class
subroutine PMPI_Add_error_class_f08(errorclass,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Add_error_class_f08
end interface  PMPI_Add_error_class

interface  PMPI_Add_error_code
subroutine PMPI_Add_error_code_f08(errorclass,errorcode,ierror)
   implicit none
   INTEGER, INTENT(IN) :: errorclass
   INTEGER, INTENT(OUT) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Add_error_code_f08
end interface  PMPI_Add_error_code

interface  PMPI_Add_error_string
subroutine PMPI_Add_error_string_f08(errorcode,string,ierror)
   implicit none
   integer, intent(in) :: errorcode
   character(len=*), intent(in) :: string
   integer, optional, intent(out) :: ierror
end subroutine PMPI_Add_error_string_f08
end interface  PMPI_Add_error_string

interface  PMPI_Alloc_mem
subroutine PMPI_Alloc_mem_f08(size,info,baseptr,ierror)
   use, intrinsic :: ISO_C_BINDING, only : C_PTR
   use :: mpi_f08_types, only : MPI_Info, MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(C_PTR), INTENT(OUT) :: baseptr
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Alloc_mem_f08
end interface  PMPI_Alloc_mem

interface  PMPI_Comm_call_errhandler
subroutine PMPI_Comm_call_errhandler_f08(comm,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_call_errhandler_f08
end interface  PMPI_Comm_call_errhandler

interface  PMPI_Comm_create_errhandler
subroutine PMPI_Comm_create_errhandler_f08(comm_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_errhandler_function
   implicit none
   PROCEDURE(MPI_Comm_errhandler_function) :: comm_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_create_errhandler_f08
end interface  PMPI_Comm_create_errhandler

interface  PMPI_Comm_get_errhandler
subroutine PMPI_Comm_get_errhandler_f08(comm,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Errhandler
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_get_errhandler_f08
end interface  PMPI_Comm_get_errhandler

interface  PMPI_Comm_set_errhandler
subroutine PMPI_Comm_set_errhandler_f08(comm,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Errhandler
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_set_errhandler_f08
end interface  PMPI_Comm_set_errhandler

interface  PMPI_Errhandler_free
subroutine PMPI_Errhandler_free_f08(errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   implicit none
   TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Errhandler_free_f08
end interface  PMPI_Errhandler_free

interface  PMPI_Error_class
subroutine PMPI_Error_class_f08(errorcode,errorclass,ierror)
   implicit none
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Error_class_f08
end interface  PMPI_Error_class

interface  PMPI_Error_string
subroutine PMPI_Error_string_f08(errorcode,string,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_ERROR_STRING
   implicit none
   integer, intent(in) :: errorcode
   character(len=MPI_MAX_ERROR_STRING), intent(out) :: string
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine PMPI_Error_string_f08
end interface  PMPI_Error_string

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

interface  PMPI_File_call_errhandler
subroutine PMPI_File_call_errhandler_f08(fh,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_call_errhandler_f08
end interface  PMPI_File_call_errhandler

interface  PMPI_File_create_errhandler
subroutine PMPI_File_create_errhandler_f08(file_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_File_errhandler_function
   implicit none
   PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_create_errhandler_f08
end interface  PMPI_File_create_errhandler

interface  PMPI_File_get_errhandler
subroutine PMPI_File_get_errhandler_f08(file,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Errhandler
   implicit none
   TYPE(MPI_File), INTENT(IN) :: file
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_errhandler_f08
end interface  PMPI_File_get_errhandler

interface  PMPI_File_set_errhandler
subroutine PMPI_File_set_errhandler_f08(file,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Errhandler
   implicit none
   TYPE(MPI_File), INTENT(IN) :: file
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_set_errhandler_f08
end interface  PMPI_File_set_errhandler

! endif for OMPI_PROVIDE_MPI_FILE_INTERFACE
#endif

interface  PMPI_Finalize
subroutine PMPI_Finalize_f08(ierror)
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Finalize_f08
end interface  PMPI_Finalize

interface  PMPI_Finalized
subroutine PMPI_Finalized_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Finalized_f08
end interface  PMPI_Finalized

! ASYNCHRONOUS had to removed from the base argument because
! the dummy argument is not an assumed-shape array.  This will
! be okay once the Interop TR is implemented.
!
interface  PMPI_Free_mem
subroutine PMPI_Free_mem_f08(base,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: base
   !$PRAGMA IGNORE_TKR base
   !DIR$ IGNORE_TKR base
   !IBM* IGNORE_TKR base
!   INTEGER(MPI_ADDRESS_KIND), DIMENSION(*) OMPI_ASYNCHRONOUS :: base
   INTEGER(MPI_ADDRESS_KIND), DIMENSION(*) :: base
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Free_mem_f08
end interface  PMPI_Free_mem

interface  PMPI_Get_processor_name
subroutine PMPI_Get_processor_name_f08(name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_PROCESSOR_NAME
   implicit none
   character(len=MPI_MAX_PROCESSOR_NAME), intent(out) :: name
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine PMPI_Get_processor_name_f08
end interface  PMPI_Get_processor_name

interface  PMPI_Get_version
subroutine PMPI_Get_version_f08(version,subversion,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: version, subversion
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_version_f08
end interface  PMPI_Get_version

interface  PMPI_Init
subroutine PMPI_Init_f08(ierror)
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Init_f08
end interface  PMPI_Init

interface  PMPI_Initialized
subroutine PMPI_Initialized_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Initialized_f08
end interface  PMPI_Initialized

interface  PMPI_Win_call_errhandler
subroutine PMPI_Win_call_errhandler_f08(win,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_call_errhandler_f08
end interface  PMPI_Win_call_errhandler

interface  PMPI_Win_create_errhandler
subroutine PMPI_Win_create_errhandler_f08(win_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_errhandler_function
   implicit none
   PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_create_errhandler_f08
end interface  PMPI_Win_create_errhandler

interface  PMPI_Win_get_errhandler
subroutine PMPI_Win_get_errhandler_f08(win,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Errhandler
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_get_errhandler_f08
end interface  PMPI_Win_get_errhandler

interface  PMPI_Win_set_errhandler
subroutine PMPI_Win_set_errhandler_f08(win,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Errhandler
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_set_errhandler_f08
end interface  PMPI_Win_set_errhandler

interface  PMPI_Info_create
subroutine PMPI_Info_create_f08(info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(OUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_create_f08
end interface  PMPI_Info_create

interface  PMPI_Info_delete
subroutine PMPI_Info_delete_f08(info,key,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_delete_f08
end interface  PMPI_Info_delete

interface  PMPI_Info_dup
subroutine PMPI_Info_dup_f08(info,newinfo,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Info), INTENT(OUT) :: newinfo
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_dup_f08
end interface  PMPI_Info_dup

interface  PMPI_Info_free
subroutine PMPI_Info_free_f08(info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(INOUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_free_f08
end interface  PMPI_Info_free

interface  PMPI_Info_get
subroutine PMPI_Info_get_f08(info,key,valuelen,value,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(IN) :: valuelen
   CHARACTER(LEN=valuelen), INTENT(OUT) :: value
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_get_f08
end interface  PMPI_Info_get

interface  PMPI_Info_get_nkeys
subroutine PMPI_Info_get_nkeys_f08(info,nkeys,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: nkeys
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_get_nkeys_f08
end interface  PMPI_Info_get_nkeys

interface  PMPI_Info_get_nthkey
subroutine PMPI_Info_get_nthkey_f08(info,n,key,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: n
   CHARACTER(lEN=*), INTENT(OUT) :: key
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_get_nthkey_f08
end interface  PMPI_Info_get_nthkey

interface  PMPI_Info_get_valuelen
subroutine PMPI_Info_get_valuelen_f08(info,key,valuelen,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(OUT) :: valuelen
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_get_valuelen_f08
end interface  PMPI_Info_get_valuelen

interface  PMPI_Info_set
subroutine PMPI_Info_set_f08(info,key,value,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key, value
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Info_set_f08
end interface  PMPI_Info_set

interface  PMPI_Close_port
subroutine PMPI_Close_port_f08(port_name,ierror)
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Close_port_f08
end interface  PMPI_Close_port

interface  PMPI_Comm_accept
subroutine PMPI_Comm_accept_f08(port_name,info,root,comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_accept_f08
end interface  PMPI_Comm_accept

interface  PMPI_Comm_connect
subroutine PMPI_Comm_connect_f08(port_name,info,root,comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_connect_f08
end interface  PMPI_Comm_connect

interface  PMPI_Comm_disconnect
subroutine PMPI_Comm_disconnect_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(INOUT) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_disconnect_f08
end interface  PMPI_Comm_disconnect

interface  PMPI_Comm_get_parent
subroutine PMPI_Comm_get_parent_f08(parent,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(OUT) :: parent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_get_parent_f08
end interface  PMPI_Comm_get_parent

interface  PMPI_Comm_join
subroutine PMPI_Comm_join_f08(fd,intercomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   INTEGER, INTENT(IN) :: fd
   TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_join_f08
end interface  PMPI_Comm_join

interface  PMPI_Comm_spawn
subroutine PMPI_Comm_spawn_f08(command,argv,maxprocs,info,root,comm,intercomm, &
                              array_of_errcodes,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: command, argv(*)
   INTEGER, INTENT(IN) :: maxprocs, root
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   INTEGER :: array_of_errcodes(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_spawn_f08
end interface  PMPI_Comm_spawn

interface  PMPI_Comm_spawn_multiple
subroutine PMPI_Comm_spawn_multiple_f08(count,array_of_commands,array_of_argv,array_of_maxprocs, &
                                       array_of_info,root,comm,intercomm, &
                                       array_of_errcodes,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   INTEGER, INTENT(IN) :: count, array_of_maxprocs(*), root
   CHARACTER(LEN=*), INTENT(IN) :: array_of_commands(*), array_of_argv(count,*)
   TYPE(MPI_Info), INTENT(IN) :: array_of_info(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   INTEGER :: array_of_errcodes(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_spawn_multiple_f08
end interface  PMPI_Comm_spawn_multiple

interface  PMPI_Lookup_name
subroutine PMPI_Lookup_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_MAX_PORT_NAME
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: service_name
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Lookup_name_f08
end interface  PMPI_Lookup_name

interface  PMPI_Open_port
subroutine PMPI_Open_port_f08(info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_MAX_PORT_NAME
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Open_port_f08
end interface  PMPI_Open_port

interface  PMPI_Publish_name
subroutine PMPI_Publish_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Publish_name_f08
end interface  PMPI_Publish_name

interface  PMPI_Unpublish_name
subroutine PMPI_Unpublish_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Unpublish_name_f08
end interface  PMPI_Unpublish_name

interface  PMPI_Accumulate
subroutine PMPI_Accumulate_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                              target_disp,target_count,target_datatype,op,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Accumulate_f08
end interface  PMPI_Accumulate

interface  PMPI_Raccumulate
subroutine PMPI_Raccumulate_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                               target_disp,target_count,target_datatype,op,win,request, &
                               ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_Request, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Raccumulate_f08
end interface  PMPI_Raccumulate

interface  PMPI_Get
subroutine PMPI_Get_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                               target_disp,target_count,target_datatype,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_f08
end interface  PMPI_Get

interface  PMPI_Rget
subroutine PMPI_Rget_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                        target_disp,target_count,target_datatype,win,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Request, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Rget_f08
end interface  PMPI_Rget

interface  PMPI_Get_accumulate
subroutine PMPI_Get_accumulate_f08(origin_addr,origin_count,origin_datatype,result_addr, &
                                  result_count,result_datatype,target_rank,target_disp, &
                                  target_count,target_datatype,op,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !$PRAGMA IGNORE_TKR origin_addr,result_addr
   !DIR$ IGNORE_TKR origin_addr,result_addr
   !IBM* IGNORE_TKR origin_addr,result_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: result_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Get_accumulate_f08
end interface  PMPI_Get_accumulate

interface  PMPI_Rget_accumulate
subroutine PMPI_Rget_accumulate_f08(origin_addr,origin_count,origin_datatype,result_addr, &
                                   result_count,result_datatype,target_rank,target_disp, &
                                   target_count,target_datatype,op,win,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Request, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !$PRAGMA IGNORE_TKR origin_addr,result_addr
   !DIR$ IGNORE_TKR origin_addr,result_addr
   !IBM* IGNORE_TKR origin_addr,result_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: result_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Rget_accumulate_f08
end interface  PMPI_Rget_accumulate

interface  PMPI_Put
subroutine PMPI_Put_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                               target_disp,target_count,target_datatype,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Put_f08
end interface  PMPI_Put

interface  PMPI_Rput
subroutine PMPI_Rput_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                        target_disp,target_count,target_datatype,win,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_Request, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
   !$PRAGMA IGNORE_TKR origin_addr
   !DIR$ IGNORE_TKR origin_addr
   !IBM* IGNORE_TKR origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Rput_f08
end interface  PMPI_Rput

interface  PMPI_Fetch_and_op
subroutine PMPI_Fetch_and_op_f08(origin_addr,result_addr,datatype,target_rank, &
                                target_disp,op,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,result_addr
   !$PRAGMA IGNORE_TKR origin_addr,result_addr
   !DIR$ IGNORE_TKR origin_addr,result_addr
   !IBM* IGNORE_TKR origin_addr,result_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: target_rank
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Fetch_and_op_f08
end interface  PMPI_Fetch_and_op

interface  PMPI_Compare_and_swap
subroutine PMPI_Compare_and_swap_f08(origin_addr,compare_addr,result_addr,datatype, &
                                    target_rank,target_disp,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,compare_addr,result_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr,compare_addr,result_addr
   !$PRAGMA IGNORE_TKR origin_addr,compare_addr,result_addr
   !DIR$ IGNORE_TKR origin_addr,compare_addr,result_addr
   !IBM* IGNORE_TKR origin_addr,compare_addr,result_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr,compare_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: target_rank
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Compare_and_swap_f08
end interface  PMPI_Compare_and_swap

interface  PMPI_Win_complete
subroutine PMPI_Win_complete_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_complete_f08
end interface  PMPI_Win_complete

interface  PMPI_Win_create
subroutine PMPI_Win_create_f08(base,size,disp_unit,info,comm,win,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: base
   !$PRAGMA IGNORE_TKR base
   !DIR$ IGNORE_TKR base
   !IBM* IGNORE_TKR base
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   INTEGER, INTENT(IN) :: disp_unit
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Win), INTENT(OUT) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_create_f08
end interface  PMPI_Win_create

interface  PMPI_Win_fence
subroutine PMPI_Win_fence_f08(assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_fence_f08
end interface  PMPI_Win_fence

interface  PMPI_Win_free
subroutine PMPI_Win_free_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(INOUT) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_free_f08
end interface  PMPI_Win_free

interface  PMPI_Win_get_group
subroutine PMPI_Win_get_group_f08(win,group,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Group
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_get_group_f08
end interface  PMPI_Win_get_group

interface  PMPI_Win_lock
subroutine PMPI_Win_lock_f08(lock_type,rank,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: lock_type, rank, assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_lock_f08
end interface  PMPI_Win_lock

interface  PMPI_Win_lock_all
subroutine PMPI_Win_lock_all_f08(assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_lock_all_f08
end interface  PMPI_Win_lock_all

interface  PMPI_Win_post
subroutine PMPI_Win_post_f08(group,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_post_f08
end interface  PMPI_Win_post

interface  PMPI_Win_start
subroutine PMPI_Win_start_f08(group,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_start_f08
end interface  PMPI_Win_start

interface  PMPI_Win_sync
subroutine PMPI_Win_sync_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_sync_f08
end interface  PMPI_Win_sync

interface  PMPI_Win_test
subroutine PMPI_Win_test_f08(win,flag,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_test_f08
end interface  PMPI_Win_test

interface  PMPI_Win_unlock
subroutine PMPI_Win_unlock_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_unlock_f08
end interface  PMPI_Win_unlock

interface  PMPI_Win_unlock_all
subroutine PMPI_Win_unlock_all_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_unlock_all_f08
end interface  PMPI_Win_unlock_all

interface  PMPI_Win_wait
subroutine PMPI_Win_wait_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_wait_f08
end interface  PMPI_Win_wait

interface  PMPI_Win_flush
subroutine PMPI_Win_flush_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_flush_f08
end interface  PMPI_Win_flush

interface  PMPI_Win_flush_local
subroutine PMPI_Win_flush_local_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_flush_local_f08
end interface  PMPI_Win_flush_local

interface  PMPI_Win_flush_all_local
subroutine PMPI_Win_flush_all_local_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_flush_all_local_f08
end interface  PMPI_Win_flush_all_local

interface  PMPI_Win_flush_all
subroutine PMPI_Win_flush_all_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Win_flush_all_f08
end interface  PMPI_Win_flush_all

interface  PMPI_Grequest_complete
subroutine PMPI_Grequest_complete_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Grequest_complete_f08
end interface  PMPI_Grequest_complete

interface  PMPI_Grequest_start
subroutine PMPI_Grequest_start_f08(query_fn,free_fn,cancel_fn,extra_state,request, &
                                  ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_query_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_free_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Grequest_cancel_function
   implicit none
   PROCEDURE(MPI_Grequest_query_function) :: query_fn
   PROCEDURE(MPI_Grequest_free_function) :: free_fn
   PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Grequest_start_f08
end interface  PMPI_Grequest_start

interface  PMPI_Init_thread
subroutine PMPI_Init_thread_f08(required,provided,ierror)
   implicit none
   INTEGER, INTENT(IN) :: required
   INTEGER, INTENT(OUT) :: provided
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Init_thread_f08
end interface  PMPI_Init_thread

interface  PMPI_Is_thread_main
subroutine PMPI_Is_thread_main_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Is_thread_main_f08
end interface  PMPI_Is_thread_main

interface  PMPI_Query_thread
subroutine PMPI_Query_thread_f08(provided,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: provided
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Query_thread_f08
end interface  PMPI_Query_thread

interface  PMPI_Status_set_cancelled
subroutine PMPI_Status_set_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Status_set_cancelled_f08
end interface  PMPI_Status_set_cancelled

interface  PMPI_Status_set_elements
subroutine PMPI_Status_set_elements_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Status_set_elements_f08
end interface  PMPI_Status_set_elements

interface  PMPI_Status_set_elements_x
subroutine PMPI_Status_set_elements_x_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(IN) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Status_set_elements_x_f08
end interface  PMPI_Status_set_elements_x

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

interface  PMPI_File_close
subroutine PMPI_File_close_f08(fh,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(INOUT) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_close_f08
end interface  PMPI_File_close

interface  PMPI_File_delete
subroutine PMPI_File_delete_f08(filename,info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: filename
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_delete_f08
end interface  PMPI_File_delete

interface  PMPI_File_get_amode
subroutine PMPI_File_get_amode_f08(fh,amode,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: amode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_amode_f08
end interface  PMPI_File_get_amode

interface  PMPI_File_get_atomicity
subroutine PMPI_File_get_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_atomicity_f08
end interface  PMPI_File_get_atomicity

interface  PMPI_File_get_byte_offset
subroutine PMPI_File_get_byte_offset_f08(fh,offset,disp,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_byte_offset_f08
end interface  PMPI_File_get_byte_offset

interface  PMPI_File_get_group
subroutine PMPI_File_get_group_f08(fh,group,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Group
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_group_f08
end interface  PMPI_File_get_group

interface  PMPI_File_get_info
subroutine PMPI_File_get_info_f08(fh,info_used,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Info
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Info), INTENT(OUT) :: info_used
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_info_f08
end interface  PMPI_File_get_info

interface  PMPI_File_get_position
subroutine PMPI_File_get_position_f08(fh,offset,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_position_f08
end interface  PMPI_File_get_position

interface  PMPI_File_get_position_shared
subroutine PMPI_File_get_position_shared_f08(fh,offset,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_position_shared_f08
end interface  PMPI_File_get_position_shared

interface  PMPI_File_get_size
subroutine PMPI_File_get_size_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_size_f08
end interface  PMPI_File_get_size

interface  PMPI_File_get_type_extent
subroutine PMPI_File_get_type_extent_f08(fh,datatype,extent,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_type_extent_f08
end interface  PMPI_File_get_type_extent

interface  PMPI_File_get_view
subroutine PMPI_File_get_view_f08(fh,disp,etype,filetype,datarep,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   TYPE(MPI_Datatype), INTENT(OUT) :: etype
   TYPE(MPI_Datatype), INTENT(OUT) :: filetype
   CHARACTER(LEN=*), INTENT(OUT) :: datarep
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_get_view_f08
end interface  PMPI_File_get_view

interface  PMPI_File_iread
subroutine PMPI_File_iread_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iread_f08
end interface  PMPI_File_iread

interface  PMPI_File_iread_at
subroutine PMPI_File_iread_at_f08(fh,offset,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iread_at_f08
end interface  PMPI_File_iread_at

interface  PMPI_File_iread_shared
subroutine PMPI_File_iread_shared_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iread_shared_f08
end interface  PMPI_File_iread_shared

interface  PMPI_File_iwrite
subroutine PMPI_File_iwrite_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iwrite_f08
end interface  PMPI_File_iwrite

interface  PMPI_File_iwrite_at
subroutine PMPI_File_iwrite_at_f08(fh,offset,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iwrite_at_f08
end interface  PMPI_File_iwrite_at

interface  PMPI_File_iwrite_shared
subroutine PMPI_File_iwrite_shared_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_iwrite_shared_f08
end interface  PMPI_File_iwrite_shared

interface  PMPI_File_open
subroutine PMPI_File_open_f08(comm,filename,amode,info,fh,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info, MPI_File
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: filename
   INTEGER, INTENT(IN) :: amode
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_File), INTENT(OUT) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_open_f08
end interface  PMPI_File_open

interface  PMPI_File_preallocate
subroutine PMPI_File_preallocate_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_preallocate_f08
end interface  PMPI_File_preallocate

interface  PMPI_File_read
subroutine PMPI_File_read_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_f08
end interface  PMPI_File_read

interface  PMPI_File_read_all
subroutine PMPI_File_read_all_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_all_f08
end interface  PMPI_File_read_all

interface  PMPI_File_read_all_begin
subroutine PMPI_File_read_all_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_all_begin_f08
end interface  PMPI_File_read_all_begin

interface  PMPI_File_read_all_end
subroutine PMPI_File_read_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_all_end_f08
end interface  PMPI_File_read_all_end

interface  PMPI_File_read_at
subroutine PMPI_File_read_at_f08(fh,offset,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_at_f08
end interface  PMPI_File_read_at

interface  PMPI_File_read_at_all
subroutine PMPI_File_read_at_all_f08(fh,offset,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_at_all_f08
end interface  PMPI_File_read_at_all

interface  PMPI_File_read_at_all_begin
subroutine PMPI_File_read_at_all_begin_f08(fh,offset,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_at_all_begin_f08
end interface  PMPI_File_read_at_all_begin

interface  PMPI_File_read_at_all_end
subroutine PMPI_File_read_at_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_at_all_end_f08
end interface  PMPI_File_read_at_all_end

interface  PMPI_File_read_ordered
subroutine PMPI_File_read_ordered_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_ordered_f08
end interface  PMPI_File_read_ordered

interface  PMPI_File_read_ordered_begin
subroutine PMPI_File_read_ordered_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_ordered_begin_f08
end interface  PMPI_File_read_ordered_begin

interface  PMPI_File_read_ordered_end
subroutine PMPI_File_read_ordered_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_ordered_end_f08
end interface  PMPI_File_read_ordered_end

interface  PMPI_File_read_shared
subroutine PMPI_File_read_shared_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_read_shared_f08
end interface  PMPI_File_read_shared

interface  PMPI_File_seek
subroutine PMPI_File_seek_f08(fh,offset,whence,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_seek_f08
end interface  PMPI_File_seek

interface  PMPI_File_seek_shared
subroutine PMPI_File_seek_shared_f08(fh,offset,whence,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_seek_shared_f08
end interface  PMPI_File_seek_shared

interface  PMPI_File_set_atomicity
subroutine PMPI_File_set_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(IN) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_set_atomicity_f08
end interface  PMPI_File_set_atomicity

interface  PMPI_File_set_info
subroutine PMPI_File_set_info_f08(fh,info,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Info
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_set_info_f08
end interface  PMPI_File_set_info

interface  PMPI_File_set_size
subroutine PMPI_File_set_size_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_set_size_f08
end interface  PMPI_File_set_size

interface  PMPI_File_set_view
subroutine PMPI_File_set_view_f08(fh,disp,etype,filetype,datarep,info,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Info, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
   TYPE(MPI_Datatype), INTENT(IN) :: etype
   TYPE(MPI_Datatype), INTENT(IN) :: filetype
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_set_view_f08
end interface  PMPI_File_set_view

interface  PMPI_File_sync
subroutine PMPI_File_sync_f08(fh,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_sync_f08
end interface  PMPI_File_sync

interface  PMPI_File_write
subroutine PMPI_File_write_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_f08
end interface  PMPI_File_write

interface  PMPI_File_write_all
subroutine PMPI_File_write_all_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_all_f08
end interface  PMPI_File_write_all

interface  PMPI_File_write_all_begin
subroutine PMPI_File_write_all_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_all_begin_f08
end interface  PMPI_File_write_all_begin

interface  PMPI_File_write_all_end
subroutine PMPI_File_write_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_all_end_f08
end interface  PMPI_File_write_all_end

interface  PMPI_File_write_at
subroutine PMPI_File_write_at_f08(fh,offset,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_at_f08
end interface  PMPI_File_write_at

interface  PMPI_File_write_at_all
subroutine PMPI_File_write_at_all_f08(fh,offset,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_at_all_f08
end interface  PMPI_File_write_at_all

interface  PMPI_File_write_at_all_begin
subroutine PMPI_File_write_at_all_begin_f08(fh,offset,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_at_all_begin_f08
end interface  PMPI_File_write_at_all_begin

interface  PMPI_File_write_at_all_end
subroutine PMPI_File_write_at_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_at_all_end_f08
end interface  PMPI_File_write_at_all_end

interface  PMPI_File_write_ordered
subroutine PMPI_File_write_ordered_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_ordered_f08
end interface  PMPI_File_write_ordered

interface  PMPI_File_write_ordered_begin
subroutine PMPI_File_write_ordered_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_ordered_begin_f08
end interface  PMPI_File_write_ordered_begin

interface  PMPI_File_write_ordered_end
subroutine PMPI_File_write_ordered_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_ordered_end_f08
end interface  PMPI_File_write_ordered_end

interface  PMPI_File_write_shared
subroutine PMPI_File_write_shared_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_File_write_shared_f08
end interface  PMPI_File_write_shared

! endif for OMPI_PROVIDE_MPI_FILE_INTERFACE
#endif

interface  PMPI_Register_datarep
subroutine PMPI_Register_datarep_f08(datarep,read_conversion_fn,write_conversion_fn, &
                                            dtype_file_extent_fn,extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_conversion_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_extent_function
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
   PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
   PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Register_datarep_f08
end interface  PMPI_Register_datarep

!
! MPI_Sizeof is generic for numeric types.  This ignore TKR interface
! is replaced by the specific generics.  Implemented in mpi_sizeof_mod.F90.
!
!subroutine PMPI_Sizeof(x,size,ierror)
!   use :: mpi_f08_types
!   implicit none
!   !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
!   !GCC$ ATTRIBUTES NO_ARG_CHECK :: x
!   !$PRAGMA IGNORE_TKR x
!   !DIR$ IGNORE_TKR x
!   !IBM* IGNORE_TKR x
!   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: x
!   INTEGER, INTENT(OUT) :: size
!   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
!end subroutine PMPI_Sizeof

interface  PMPI_Type_create_f90_complex
subroutine PMPI_Type_create_f90_complex_f08(p,r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: p, r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_f90_complex_f08
end interface  PMPI_Type_create_f90_complex

interface  PMPI_Type_create_f90_integer
subroutine PMPI_Type_create_f90_integer_f08(r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_f90_integer_f08
end interface  PMPI_Type_create_f90_integer

interface  PMPI_Type_create_f90_real
subroutine PMPI_Type_create_f90_real_f08(p,r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: p, r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_create_f90_real_f08
end interface  PMPI_Type_create_f90_real

interface  PMPI_Type_match_size
subroutine PMPI_Type_match_size_f08(typeclass,size,datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: typeclass, size
   TYPE(MPI_Datatype), INTENT(OUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Type_match_size_f08
end interface  PMPI_Type_match_size

interface  PMPI_Pcontrol
subroutine PMPI_Pcontrol_f08(level)
   implicit none
   INTEGER, INTENT(IN) :: level
end subroutine PMPI_Pcontrol_f08
end interface  PMPI_Pcontrol


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! New routines to MPI-3
!

interface  PMPI_Comm_split_type
subroutine PMPI_Comm_split_type_f08(comm,split_type,key,info,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: split_type
   INTEGER, INTENT(IN) :: key
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Comm_split_type_f08
end interface  PMPI_Comm_split_type

interface  PMPI_F_sync_reg
subroutine PMPI_F_sync_reg_f08(buf)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
end subroutine PMPI_F_sync_reg_f08
end interface  PMPI_F_sync_reg

interface  PMPI_Get_library_version
subroutine PMPI_Get_library_version_f08(version,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_LIBRARY_VERSION_STRING
   implicit none
   character(len=MPI_MAX_LIBRARY_VERSION_STRING), intent(out) :: version
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine PMPI_Get_library_version_f08
end interface  PMPI_Get_library_version

interface  PMPI_Mprobe
subroutine PMPI_Mprobe_f08(source,tag,comm,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Message, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Message), INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Mprobe_f08
end interface  PMPI_Mprobe

interface  PMPI_Improbe
subroutine PMPI_Improbe_f08(source,tag,comm,flag,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Message, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Message), INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Improbe_f08
end interface  PMPI_Improbe

interface  PMPI_Imrecv
subroutine PMPI_Imrecv_f08(buf,count,datatype,message,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Message, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Message), INTENT(INOUT) :: message
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Imrecv_f08
end interface  PMPI_Imrecv

interface  PMPI_Mrecv
subroutine PMPI_Mrecv_f08(buf,count,datatype,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Message, MPI_Status
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Message), INTENT(INOUT) :: message
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Mrecv_f08
end interface  PMPI_Mrecv

interface  PMPI_Neighbor_allgather
subroutine PMPI_Neighbor_allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Neighbor_allgather_f08
end interface  PMPI_Neighbor_allgather

interface  PMPI_Ineighbor_allgather
subroutine PMPI_Ineighbor_allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ineighbor_allgather_f08
end interface  PMPI_Ineighbor_allgather

interface  PMPI_Neighbor_allgatherv
subroutine PMPI_Neighbor_allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Neighbor_allgatherv_f08
end interface  PMPI_Neighbor_allgatherv

interface  PMPI_Ineighbor_allgatherv
subroutine PMPI_Ineighbor_allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ineighbor_allgatherv_f08
end interface  PMPI_Ineighbor_allgatherv

interface  PMPI_Neighbor_alltoall
subroutine PMPI_Neighbor_alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Neighbor_alltoall_f08
end interface  PMPI_Neighbor_alltoall

interface  PMPI_Ineighbor_alltoall
subroutine PMPI_Ineighbor_alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ineighbor_alltoall_f08
end interface  PMPI_Ineighbor_alltoall

interface  PMPI_Neighbor_alltoallv
subroutine PMPI_Neighbor_alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Neighbor_alltoallv_f08
end interface  PMPI_Neighbor_alltoallv

interface  PMPI_Ineighbor_alltoallv
subroutine PMPI_Ineighbor_alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ineighbor_alltoallv_f08
end interface  PMPI_Ineighbor_alltoallv

interface  PMPI_Neighbor_alltoallw
subroutine PMPI_Neighbor_alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*), recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Neighbor_alltoallw_f08
end interface  PMPI_Neighbor_alltoallw

interface  PMPI_Ineighbor_alltoallw
subroutine PMPI_Ineighbor_alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
   !$PRAGMA IGNORE_TKR sendbuf, recvbuf
   !DIR$ IGNORE_TKR sendbuf, recvbuf
   !IBM* IGNORE_TKR sendbuf, recvbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*), recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine PMPI_Ineighbor_alltoallw_f08
end interface  PMPI_Ineighbor_alltoallw

end module pmpi_f08_interfaces
