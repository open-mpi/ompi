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

module mpi_f08_interfaces

interface  MPI_Bsend
subroutine MPI_Bsend_f08(buf,count,datatype,dest,tag,comm,ierror)
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
end subroutine MPI_Bsend_f08
end interface  MPI_Bsend

interface  MPI_Bsend_init
subroutine MPI_Bsend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Bsend_init_f08
end interface  MPI_Bsend_init

interface  MPI_Buffer_attach
subroutine MPI_Buffer_attach_f08(buffer,size,ierror)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
   !$PRAGMA IGNORE_TKR buffer
   !DIR$ IGNORE_TKR buffer
   !IBM* IGNORE_TKR buffer
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
   INTEGER, INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Buffer_attach_f08
end interface  MPI_Buffer_attach

interface  MPI_Buffer_detach
subroutine MPI_Buffer_detach_f08(buffer_addr,size,ierror)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer_addr
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer_addr
   !$PRAGMA IGNORE_TKR buffer_addr
   !DIR$ IGNORE_TKR buffer_addr
   !IBM* IGNORE_TKR buffer_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer_addr
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Buffer_detach_f08
end interface  MPI_Buffer_detach

interface  MPI_Cancel
subroutine MPI_Cancel_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cancel_f08
end interface  MPI_Cancel

interface  MPI_Get_count
subroutine MPI_Get_count_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_count_f08
end interface  MPI_Get_count

interface  MPI_Ibsend
subroutine MPI_Ibsend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Ibsend_f08
end interface  MPI_Ibsend

interface  MPI_Iprobe
subroutine MPI_Iprobe_f08(source,tag,comm,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Iprobe_f08
end interface  MPI_Iprobe

interface  MPI_Irecv
subroutine MPI_Irecv_f08(buf,count,datatype,source,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Irecv_f08
end interface  MPI_Irecv

interface  MPI_Irsend
subroutine MPI_Irsend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Irsend_f08
end interface  MPI_Irsend

interface  MPI_Isend
subroutine MPI_Isend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Isend_f08
end interface  MPI_Isend

interface  MPI_Issend
subroutine MPI_Issend_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Issend_f08
end interface  MPI_Issend

interface  MPI_Probe
subroutine MPI_Probe_f08(source,tag,comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Probe_f08
end interface  MPI_Probe

interface  MPI_Recv
subroutine MPI_Recv_f08(buf,count,datatype,source,tag,comm,status,ierror)
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
end subroutine MPI_Recv_f08
end interface  MPI_Recv

interface  MPI_Recv_init
subroutine MPI_Recv_init_f08(buf,count,datatype,source,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Recv_init_f08
end interface  MPI_Recv_init

interface  MPI_Request_free
subroutine MPI_Request_free_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Request_free_f08
end interface  MPI_Request_free

interface  MPI_Request_get_status
subroutine MPI_Request_get_status_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Request_get_status_f08
end interface  MPI_Request_get_status

interface  MPI_Rsend
subroutine MPI_Rsend_f08(buf,count,datatype,dest,tag,comm,ierror)
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
end subroutine MPI_Rsend_f08
end interface  MPI_Rsend

interface  MPI_Rsend_init
subroutine MPI_Rsend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Rsend_init_f08
end interface  MPI_Rsend_init

interface  MPI_Send
subroutine MPI_Send_f08(buf,count,datatype,dest,tag,comm,ierror)
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
end subroutine MPI_Send_f08
end interface  MPI_Send

interface  MPI_Sendrecv
subroutine MPI_Sendrecv_f08(sendbuf,sendcount,sendtype,dest,sendtag,recvbuf, &
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
end subroutine MPI_Sendrecv_f08
end interface  MPI_Sendrecv

interface  MPI_Sendrecv_replace
subroutine MPI_Sendrecv_replace_f08(buf,count,datatype,dest,sendtag,source,recvtag, &
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
end subroutine MPI_Sendrecv_replace_f08
end interface  MPI_Sendrecv_replace

interface  MPI_Send_init
subroutine MPI_Send_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Send_init_f08
end interface  MPI_Send_init

interface  MPI_Ssend
subroutine MPI_Ssend_f08(buf,count,datatype,dest,tag,comm,ierror)
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
end subroutine MPI_Ssend_f08
end interface  MPI_Ssend

interface  MPI_Ssend_init
subroutine MPI_Ssend_init_f08(buf,count,datatype,dest,tag,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Ssend_init_f08
end interface  MPI_Ssend_init

interface  MPI_Start
subroutine MPI_Start_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Start_f08
end interface  MPI_Start

interface  MPI_Startall
subroutine MPI_Startall_f08(count,array_of_requests,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Startall_f08
end interface  MPI_Startall

interface  MPI_Test
subroutine MPI_Test_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Test_f08
end interface  MPI_Test

interface  MPI_Testall
subroutine MPI_Testall_f08(count,array_of_requests,flag,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Testall_f08
end interface  MPI_Testall

interface  MPI_Testany
subroutine MPI_Testany_f08(count,array_of_requests,index,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Testany_f08
end interface  MPI_Testany

interface  MPI_Testsome
subroutine MPI_Testsome_f08(incount,array_of_requests,outcount, &
                        array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Testsome_f08
end interface  MPI_Testsome

interface  MPI_Test_cancelled
subroutine MPI_Test_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Test_cancelled_f08
end interface  MPI_Test_cancelled

interface  MPI_Wait
subroutine MPI_Wait_f08(request,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Wait_f08
end interface  MPI_Wait

interface  MPI_Waitall
subroutine MPI_Waitall_f08(count,array_of_requests,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Waitall_f08
end interface  MPI_Waitall

interface  MPI_Waitany
subroutine MPI_Waitany_f08(count,array_of_requests,index,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Waitany_f08
end interface  MPI_Waitany

interface  MPI_Waitsome
subroutine MPI_Waitsome_f08(incount,array_of_requests,outcount, &
                        array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
   TYPE(MPI_Status) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Waitsome_f08
end interface  MPI_Waitsome

interface  MPI_Get_address
subroutine MPI_Get_address_f08(location,address,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: location
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: location
   !$PRAGMA IGNORE_TKR location
   !DIR$ IGNORE_TKR location
   !IBM* IGNORE_TKR location
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: location
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: address
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_address_f08
end interface  MPI_Get_address

interface  MPI_Get_elements
subroutine MPI_Get_elements_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_elements_f08
end interface  MPI_Get_elements

interface  MPI_Get_elements_x
subroutine MPI_Get_elements_x_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_elements_x_f08
end interface  MPI_Get_elements_x

interface  MPI_Pack
subroutine MPI_Pack_f08(inbuf,incount,datatype,outbuf,outsize,position,comm,ierror)
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
end subroutine MPI_Pack_f08
end interface  MPI_Pack

interface  MPI_Pack_external
subroutine MPI_Pack_external_f08(datarep,inbuf,incount,datatype,outbuf,outsize, &
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
end subroutine MPI_Pack_external_f08
end interface  MPI_Pack_external

interface  MPI_Pack_external_size
subroutine MPI_Pack_external_size_f08(datarep,incount,datatype,size,ierror &
           )
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: incount
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Pack_external_size_f08
end interface  MPI_Pack_external_size

interface  MPI_Pack_size
subroutine MPI_Pack_size_f08(incount,datatype,comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Pack_size_f08
end interface  MPI_Pack_size

interface  MPI_Type_commit
subroutine MPI_Type_commit_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_commit_f08
end interface  MPI_Type_commit

interface  MPI_Type_contiguous
subroutine MPI_Type_contiguous_f08(count,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_contiguous_f08
end interface  MPI_Type_contiguous

interface  MPI_Type_create_darray
subroutine MPI_Type_create_darray_f08(size,rank,ndims,array_of_gsizes, &
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
end subroutine MPI_Type_create_darray_f08
end interface  MPI_Type_create_darray

interface  MPI_Type_create_hindexed
subroutine MPI_Type_create_hindexed_f08(count,array_of_blocklengths, &
                                        array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_hindexed_f08
end interface  MPI_Type_create_hindexed

interface  MPI_Type_create_hvector
subroutine MPI_Type_create_hvector_f08(count,blocklength,stride,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: stride
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_hvector_f08
end interface  MPI_Type_create_hvector

interface  MPI_Type_create_indexed_block
subroutine MPI_Type_create_indexed_block_f08(count,blocklength, &
                           array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER, INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_indexed_block_f08
end interface  MPI_Type_create_indexed_block

interface  MPI_Type_create_hindexed_block
subroutine MPI_Type_create_hindexed_block_f08(count,blocklength, &
                           array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_hindexed_block_f08
end interface  MPI_Type_create_hindexed_block

interface  MPI_Type_create_resized
subroutine MPI_Type_create_resized_f08(oldtype,lb,extent,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_resized_f08
end interface  MPI_Type_create_resized

interface  MPI_Type_create_struct
subroutine MPI_Type_create_struct_f08(count,array_of_blocklengths, &
                           array_of_displacements,array_of_types,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(count)
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_struct_f08
end interface  MPI_Type_create_struct

interface  MPI_Type_create_subarray
subroutine MPI_Type_create_subarray_f08(ndims,array_of_sizes,array_of_subsizes, &
                    array_of_starts,order,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: ndims, order
   INTEGER, INTENT(IN) :: array_of_sizes(ndims), array_of_subsizes(ndims)
   INTEGER, INTENT(IN) :: array_of_starts(ndims)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_subarray_f08
end interface  MPI_Type_create_subarray

interface  MPI_Type_dup
subroutine MPI_Type_dup_f08(oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_dup_f08
end interface  MPI_Type_dup

interface  MPI_Type_free
subroutine MPI_Type_free_f08(datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_free_f08
end interface  MPI_Type_free

interface  MPI_Type_get_contents
subroutine MPI_Type_get_contents_f08(datatype,max_integers,max_addresses,max_datatypes, &
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
end subroutine MPI_Type_get_contents_f08
end interface  MPI_Type_get_contents

interface  MPI_Type_get_envelope
subroutine MPI_Type_get_envelope_f08(datatype,num_integers,num_addresses,num_datatypes, &
                                     combiner,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes, combiner
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_envelope_f08
end interface  MPI_Type_get_envelope

interface  MPI_Type_get_extent
subroutine MPI_Type_get_extent_f08(datatype,lb,extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_extent_f08
end interface  MPI_Type_get_extent

interface  MPI_Type_get_extent_x
subroutine MPI_Type_get_extent_x_f08(datatype,lb,extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: lb, extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_extent_x_f08
end interface  MPI_Type_get_extent_x

interface  MPI_Type_get_true_extent
subroutine MPI_Type_get_true_extent_f08(datatype,true_lb,true_extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_true_extent_f08
end interface  MPI_Type_get_true_extent

interface  MPI_Type_get_true_extent_x
subroutine MPI_Type_get_true_extent_x_f08(datatype,true_lb,true_extent,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_true_extent_x_f08
end interface  MPI_Type_get_true_extent_x

interface  MPI_Type_indexed
subroutine MPI_Type_indexed_f08(count,array_of_blocklengths, &
                                array_of_displacements,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count), array_of_displacements(count)
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_indexed_f08
end interface  MPI_Type_indexed

interface  MPI_Type_size
subroutine MPI_Type_size_f08(datatype,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_size_f08
end interface  MPI_Type_size

interface  MPI_Type_size_x
subroutine MPI_Type_size_x_f08(datatype,size,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_size_x_f08
end interface  MPI_Type_size_x

interface  MPI_Type_vector
subroutine MPI_Type_vector_f08(count,blocklength,stride,oldtype,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength, stride
   TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_vector_f08
end interface  MPI_Type_vector

interface  MPI_Unpack
subroutine MPI_Unpack_f08(inbuf,insize,position,outbuf,outcount,datatype,comm, &
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
end subroutine MPI_Unpack_f08
end interface  MPI_Unpack

interface  MPI_Unpack_external
subroutine MPI_Unpack_external_f08(datarep,inbuf,insize,position,outbuf,outcount, &
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
end subroutine MPI_Unpack_external_f08
end interface  MPI_Unpack_external

interface  MPI_Allgather
subroutine MPI_Allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Allgather_f08
end interface  MPI_Allgather

interface  MPI_Iallgather
subroutine MPI_Iallgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Iallgather_f08
end interface  MPI_Iallgather

interface  MPI_Allgatherv
subroutine MPI_Allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Allgatherv_f08
end interface  MPI_Allgatherv

interface  MPI_Iallgatherv
subroutine MPI_Iallgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Iallgatherv_f08
end interface  MPI_Iallgatherv

interface  MPI_Allreduce
subroutine MPI_Allreduce_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
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
end subroutine MPI_Allreduce_f08
end interface  MPI_Allreduce

interface  MPI_Iallreduce
subroutine MPI_Iallreduce_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
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
end subroutine MPI_Iallreduce_f08
end interface  MPI_Iallreduce

interface  MPI_Alltoall
subroutine MPI_Alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Alltoall_f08
end interface  MPI_Alltoall

interface  MPI_Ialltoall
subroutine MPI_Ialltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Ialltoall_f08
end interface  MPI_Ialltoall

interface  MPI_Alltoallv
subroutine MPI_Alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
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
end subroutine MPI_Alltoallv_f08
end interface  MPI_Alltoallv

interface  MPI_Ialltoallv
subroutine MPI_Ialltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
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
end subroutine MPI_Ialltoallv_f08
end interface  MPI_Ialltoallv

interface  MPI_Alltoallw
subroutine MPI_Alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
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
end subroutine MPI_Alltoallw_f08
end interface  MPI_Alltoallw

interface  MPI_Ialltoallw
subroutine MPI_Ialltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
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
end subroutine MPI_Ialltoallw_f08
end interface  MPI_Ialltoallw

interface  MPI_Barrier
subroutine MPI_Barrier_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Barrier_f08
end interface  MPI_Barrier

interface  MPI_Ibarrier
subroutine MPI_Ibarrier_f08(comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Ibarrier_f08
end interface  MPI_Ibarrier

interface  MPI_Bcast
subroutine MPI_Bcast_f08(buffer,count,datatype,root,comm,ierror)
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
end subroutine MPI_Bcast_f08
end interface  MPI_Bcast

interface  MPI_Ibcast
subroutine MPI_Ibcast_f08(buffer,count,datatype,root,comm,request,ierror)
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
end subroutine MPI_Ibcast_f08
end interface  MPI_Ibcast

interface  MPI_Exscan
subroutine MPI_Exscan_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
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
end subroutine MPI_Exscan_f08
end interface  MPI_Exscan

interface  MPI_Iexscan
subroutine MPI_Iexscan_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
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
end subroutine MPI_Iexscan_f08
end interface  MPI_Iexscan

interface  MPI_Gather
subroutine MPI_Gather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Gather_f08
end interface  MPI_Gather

interface  MPI_Igather
subroutine MPI_Igather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Igather_f08
end interface  MPI_Igather

interface  MPI_Gatherv
subroutine MPI_Gatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Gatherv_f08
end interface  MPI_Gatherv

interface  MPI_Igatherv
subroutine MPI_Igatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Igatherv_f08
end interface  MPI_Igatherv

interface  MPI_Op_commutative
subroutine MPI_Op_commutative_f08(op,commute,ierror)
   use :: mpi_f08_types, only : MPI_Op
   implicit none
   TYPE(MPI_Op), INTENT(IN) :: op
   LOGICAL, INTENT(OUT) :: commute
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Op_commutative_f08
end interface  MPI_Op_commutative

interface  MPI_Op_create
subroutine MPI_Op_create_f08(user_fn,commute,op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   use :: mpi_f08_interfaces_callbacks, only : MPI_User_function
   implicit none
   PROCEDURE(MPI_User_function) :: user_fn
   LOGICAL, INTENT(IN) :: commute
   TYPE(MPI_Op), INTENT(OUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Op_create_f08
end interface  MPI_Op_create

interface  MPI_Op_free
subroutine MPI_Op_free_f08(op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   implicit none
   TYPE(MPI_Op), INTENT(INOUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Op_free_f08
end interface  MPI_Op_free

interface  MPI_Reduce
subroutine MPI_Reduce_f08(sendbuf,recvbuf,count,datatype,op,root,comm,ierror)
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
end subroutine MPI_Reduce_f08
end interface  MPI_Reduce

interface  MPI_Ireduce
subroutine MPI_Ireduce_f08(sendbuf,recvbuf,count,datatype,op,root,comm,request,ierror)
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
end subroutine MPI_Ireduce_f08
end interface  MPI_Ireduce

interface  MPI_Reduce_local
subroutine MPI_Reduce_local_f08(inbuf,inoutbuf,count,datatype,op,ierror)
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
end subroutine MPI_Reduce_local_f08
end interface  MPI_Reduce_local

interface  MPI_Reduce_scatter
subroutine MPI_Reduce_scatter_f08(sendbuf,recvbuf,recvcounts,datatype,op,comm, &
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
end subroutine MPI_Reduce_scatter_f08
end interface  MPI_Reduce_scatter

interface  MPI_Ireduce_scatter
subroutine MPI_Ireduce_scatter_f08(sendbuf,recvbuf,recvcounts,datatype,op,comm, &
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
end subroutine MPI_Ireduce_scatter_f08
end interface  MPI_Ireduce_scatter

interface  MPI_Reduce_scatter_block
subroutine MPI_Reduce_scatter_block_f08(sendbuf,recvbuf,recvcount,datatype,op,comm, &
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
end subroutine MPI_Reduce_scatter_block_f08
end interface  MPI_Reduce_scatter_block

interface  MPI_Ireduce_scatter_block
subroutine MPI_Ireduce_scatter_block_f08(sendbuf,recvbuf,recvcount,datatype,op,comm, &
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
end subroutine MPI_Ireduce_scatter_block_f08
end interface  MPI_Ireduce_scatter_block

interface  MPI_Scan
subroutine MPI_Scan_f08(sendbuf,recvbuf,count,datatype,op,comm,ierror)
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
end subroutine MPI_Scan_f08
end interface  MPI_Scan

interface  MPI_Iscan
subroutine MPI_Iscan_f08(sendbuf,recvbuf,count,datatype,op,comm,request,ierror)
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
end subroutine MPI_Iscan_f08
end interface  MPI_Iscan

interface  MPI_Scatter
subroutine MPI_Scatter_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Scatter_f08
end interface  MPI_Scatter

interface  MPI_Iscatter
subroutine MPI_Iscatter_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Iscatter_f08
end interface  MPI_Iscatter

interface  MPI_Scatterv
subroutine MPI_Scatterv_f08(sendbuf,sendcounts,displs,sendtype,recvbuf,recvcount, &
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
end subroutine MPI_Scatterv_f08
end interface  MPI_Scatterv

interface  MPI_Iscatterv
subroutine MPI_Iscatterv_f08(sendbuf,sendcounts,displs,sendtype,recvbuf,recvcount, &
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
end subroutine MPI_Iscatterv_f08
end interface  MPI_Iscatterv

interface  MPI_Comm_compare
subroutine MPI_Comm_compare_f08(comm1,comm2,result,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm1
   TYPE(MPI_Comm), INTENT(IN) :: comm2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_compare_f08
end interface  MPI_Comm_compare

interface  MPI_Comm_create
subroutine MPI_Comm_create_f08(comm,group,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(IN) :: group
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_create_f08
end interface  MPI_Comm_create

interface  MPI_Comm_create_group
subroutine MPI_Comm_create_group_f08(comm,group,tag,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: tag
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_create_group_f08
end interface  MPI_Comm_create_group

interface  MPI_Comm_create_keyval
subroutine MPI_Comm_create_keyval_f08(comm_copy_attr_fn,comm_delete_attr_fn,comm_keyval, &
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
end subroutine MPI_Comm_create_keyval_f08
end interface  MPI_Comm_create_keyval

interface  MPI_Comm_delete_attr
subroutine MPI_Comm_delete_attr_f08(comm,comm_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_delete_attr_f08
end interface  MPI_Comm_delete_attr

interface  MPI_Comm_dup
subroutine MPI_Comm_dup_f08(comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_dup_f08
end interface  MPI_Comm_dup

interface  MPI_Comm_dup_with_info
subroutine MPI_Comm_dup_with_info_f08(comm,info,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_dup_with_info_f08
end interface  MPI_Comm_dup_with_info

interface  MPI_Comm_idup
subroutine MPI_Comm_idup_f08(comm,newcomm,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_idup_f08
end interface  MPI_Comm_idup

interface  MPI_Comm_free
subroutine MPI_Comm_free_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(INOUT) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_free_f08
end interface  MPI_Comm_free

interface  MPI_Comm_free_keyval
subroutine MPI_Comm_free_keyval_f08(comm_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: comm_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_free_keyval_f08
end interface  MPI_Comm_free_keyval

interface  MPI_Comm_get_attr
subroutine MPI_Comm_get_attr_f08(comm,comm_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_get_attr_f08
end interface  MPI_Comm_get_attr

interface  MPI_Comm_get_info
subroutine MPI_Comm_get_info_f08(comm,info_used,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(OUT) :: info_used
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_get_info_f08
end interface  MPI_Comm_get_info

interface  MPI_Comm_get_name
subroutine MPI_Comm_get_name_f08(comm,comm_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: comm_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_get_name_f08
end interface  MPI_Comm_get_name

interface  MPI_Comm_group
subroutine MPI_Comm_group_f08(comm,group,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_group_f08
end interface  MPI_Comm_group

interface  MPI_Comm_rank
subroutine MPI_Comm_rank_f08(comm,rank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_rank_f08
end interface  MPI_Comm_rank

interface  MPI_Comm_remote_group
subroutine MPI_Comm_remote_group_f08(comm,group,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_remote_group_f08
end interface  MPI_Comm_remote_group

interface  MPI_Comm_remote_size
subroutine MPI_Comm_remote_size_f08(comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_remote_size_f08
end interface  MPI_Comm_remote_size

interface  MPI_Comm_set_attr
subroutine MPI_Comm_set_attr_f08(comm,comm_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_set_attr_f08
end interface  MPI_Comm_set_attr

interface  MPI_Comm_set_info
subroutine MPI_Comm_set_info_f08(comm,info,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_set_info_f08
end interface  MPI_Comm_set_info

interface  MPI_Comm_set_name
subroutine MPI_Comm_set_name_f08(comm,comm_name,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: comm_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_set_name_f08
end interface  MPI_Comm_set_name

interface  MPI_Comm_size
subroutine MPI_Comm_size_f08(comm,size,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_size_f08
end interface  MPI_Comm_size

interface  MPI_Comm_split
subroutine MPI_Comm_split_f08(comm,color,key,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: color, key
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_split_f08
end interface  MPI_Comm_split

interface  MPI_Comm_test_inter
subroutine MPI_Comm_test_inter_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_test_inter_f08
end interface  MPI_Comm_test_inter

interface  MPI_Group_compare
subroutine MPI_Group_compare_f08(group1,group2,result,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_compare_f08
end interface  MPI_Group_compare

interface  MPI_Group_difference
subroutine MPI_Group_difference_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_difference_f08
end interface  MPI_Group_difference

interface  MPI_Group_excl
subroutine MPI_Group_excl_f08(group,n,ranks,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranks(n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_excl_f08
end interface  MPI_Group_excl

interface  MPI_Group_free
subroutine MPI_Group_free_f08(group,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(INOUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_free_f08
end interface  MPI_Group_free

interface  MPI_Group_incl
subroutine MPI_Group_incl_f08(group,n,ranks,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   INTEGER, INTENT(IN) :: n, ranks(n)
   TYPE(MPI_Group), INTENT(IN) :: group
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_incl_f08
end interface  MPI_Group_incl

interface  MPI_Group_intersection
subroutine MPI_Group_intersection_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_intersection_f08
end interface  MPI_Group_intersection

interface  MPI_Group_range_excl
subroutine MPI_Group_range_excl_f08(group,n,ranges,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranges(3,n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_range_excl_f08
end interface  MPI_Group_range_excl

interface  MPI_Group_range_incl
subroutine MPI_Group_range_incl_f08(group,n,ranges,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n, ranges(3,n)
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_range_incl_f08
end interface  MPI_Group_range_incl

interface  MPI_Group_rank
subroutine MPI_Group_rank_f08(group,rank,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_rank_f08
end interface  MPI_Group_rank

interface  MPI_Group_size
subroutine MPI_Group_size_f08(group,size,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_size_f08
end interface  MPI_Group_size

interface  MPI_Group_translate_ranks
subroutine MPI_Group_translate_ranks_f08(group1,n,ranks1,group2,ranks2,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks1(n)
   INTEGER, INTENT(OUT) :: ranks2(n)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_translate_ranks_f08
end interface  MPI_Group_translate_ranks

interface  MPI_Group_union
subroutine MPI_Group_union_f08(group1,group2,newgroup,ierror)
   use :: mpi_f08_types, only : MPI_Group
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   TYPE(MPI_Group), INTENT(OUT) :: newgroup
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Group_union_f08
end interface  MPI_Group_union

interface  MPI_Intercomm_create
subroutine MPI_Intercomm_create_f08(local_comm,local_leader,peer_comm,remote_leader, &
                                    tag,newintercomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: local_comm, peer_comm
   INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
   TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Intercomm_create_f08
end interface  MPI_Intercomm_create

interface  MPI_Intercomm_merge
subroutine MPI_Intercomm_merge_f08(intercomm,high,newintracomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: intercomm
   LOGICAL, INTENT(IN) :: high
   TYPE(MPI_Comm), INTENT(OUT) :: newintracomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Intercomm_merge_f08
end interface  MPI_Intercomm_merge

interface  MPI_Type_create_keyval
subroutine MPI_Type_create_keyval_f08(type_copy_attr_fn,type_delete_attr_fn,type_keyval, &
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
end subroutine MPI_Type_create_keyval_f08
end interface  MPI_Type_create_keyval

interface  MPI_Type_delete_attr
subroutine MPI_Type_delete_attr_f08(datatype,type_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_delete_attr_f08
end interface  MPI_Type_delete_attr

interface  MPI_Type_free_keyval
subroutine MPI_Type_free_keyval_f08(type_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: type_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_free_keyval_f08
end interface  MPI_Type_free_keyval

interface  MPI_Type_get_attr
subroutine MPI_Type_get_attr_f08(datatype,type_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_attr_f08
end interface  MPI_Type_get_attr

interface  MPI_Type_get_name
subroutine MPI_Type_get_name_f08(datatype,type_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: type_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_get_name_f08
end interface  MPI_Type_get_name

interface  MPI_Type_set_attr
subroutine MPI_Type_set_attr_f08(datatype,type_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_set_attr_f08
end interface  MPI_Type_set_attr

interface  MPI_Type_set_name
subroutine MPI_Type_set_name_f08(datatype,type_name,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   CHARACTER(LEN=*), INTENT(IN) :: type_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_set_name_f08
end interface  MPI_Type_set_name

interface MPI_Win_allocate
subroutine MPI_Win_allocate_f08(size, disp_unit, info, comm, &
      baseptr, win, ierror)
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::  size
  INTEGER, INTENT(IN) ::  disp_unit
  TYPE(MPI_Info), INTENT(IN) ::  info
  TYPE(MPI_Comm), INTENT(IN) ::  comm
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  TYPE(MPI_Win), INTENT(OUT) ::  win
  INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
end subroutine MPI_Win_allocate_f08
end interface MPI_Win_allocate

interface MPI_Win_allocate_shared
subroutine MPI_Win_allocate_shared_f08(size, disp_unit, info, comm, &
      baseptr, win, ierror)
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::  size
  INTEGER, INTENT(IN) ::  disp_unit
  TYPE(MPI_Info), INTENT(IN) ::  info
  TYPE(MPI_Comm), INTENT(IN) ::  comm
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  TYPE(MPI_Win), INTENT(OUT) ::  win
  INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
end subroutine MPI_Win_allocate_shared_f08
end interface MPI_Win_allocate_shared

interface  MPI_Win_create_keyval
subroutine MPI_Win_create_keyval_f08(win_copy_attr_fn,win_delete_attr_fn,win_keyval, &
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
end subroutine MPI_Win_create_keyval_f08
end interface  MPI_Win_create_keyval

interface  MPI_Win_delete_attr
subroutine MPI_Win_delete_attr_f08(win,win_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_delete_attr_f08
end interface  MPI_Win_delete_attr

interface  MPI_Win_free_keyval
subroutine MPI_Win_free_keyval_f08(win_keyval,ierror)
   implicit none
   INTEGER, INTENT(INOUT) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_free_keyval_f08
end interface  MPI_Win_free_keyval

interface  MPI_Win_get_attr
subroutine MPI_Win_get_attr_f08(win,win_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_get_attr_f08
end interface  MPI_Win_get_attr

interface  MPI_Win_get_name
subroutine MPI_Win_get_name_f08(win,win_name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_MAX_OBJECT_NAME
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: win_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_get_name_f08
end interface  MPI_Win_get_name

interface  MPI_Win_set_attr
subroutine MPI_Win_set_attr_f08(win,win_keyval,attribute_val,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_set_attr_f08
end interface  MPI_Win_set_attr

interface  MPI_Win_set_name
subroutine MPI_Win_set_name_f08(win,win_name,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   CHARACTER(LEN=*), INTENT(IN) :: win_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_set_name_f08
end interface  MPI_Win_set_name

interface  MPI_Cartdim_get
subroutine MPI_Cartdim_get_f08(comm,ndims,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ndims
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cartdim_get_f08
end interface  MPI_Cartdim_get

interface  MPI_Cart_coords
subroutine MPI_Cart_coords_f08(comm,rank,maxdims,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxdims
   INTEGER, INTENT(OUT) :: coords(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_coords_f08
end interface  MPI_Cart_coords

interface  MPI_Cart_create
subroutine MPI_Cart_create_f08(comm_old,ndims,dims,periods,reorder,comm_cart,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: ndims, dims(ndims)
   LOGICAL, INTENT(IN) :: periods(ndims), reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_create_f08
end interface  MPI_Cart_create

interface  MPI_Cart_get
subroutine MPI_Cart_get_f08(comm,maxdims,dims,periods,coords,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxdims
   INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
   LOGICAL, INTENT(OUT) :: periods(maxdims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_get_f08
end interface  MPI_Cart_get

interface  MPI_Cart_map
subroutine MPI_Cart_map_f08(comm,ndims,dims,periods,newrank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: ndims, dims(ndims)
   LOGICAL, INTENT(IN) :: periods(ndims)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_map_f08
end interface  MPI_Cart_map

interface  MPI_Cart_rank
subroutine MPI_Cart_rank_f08(comm,coords,rank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: coords(*)
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_rank_f08
end interface  MPI_Cart_rank

interface  MPI_Cart_shift
subroutine MPI_Cart_shift_f08(comm,direction,disp,rank_source,rank_dest,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: direction, disp
   INTEGER, INTENT(OUT) :: rank_source, rank_dest
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_shift_f08
end interface  MPI_Cart_shift

interface  MPI_Cart_sub
subroutine MPI_Cart_sub_f08(comm,remain_dims,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(IN) :: remain_dims(*)
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Cart_sub_f08
end interface  MPI_Cart_sub

interface  MPI_Dims_create
subroutine MPI_Dims_create_f08(nnodes,ndims,dims,ierror)
   implicit none
   INTEGER, INTENT(IN) :: nnodes, ndims
   INTEGER, INTENT(INOUT) :: dims(ndims)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Dims_create_f08
end interface  MPI_Dims_create

interface  MPI_Dist_graph_create
subroutine MPI_Dist_graph_create_f08(comm_old,n,sources,degrees,destinations,weights, &
                                     info,reorder,comm_dist_graph,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: n, sources(n), degrees(n), destinations(*), weights(*)
   TYPE(MPI_Info), INTENT(IN) :: info
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Dist_graph_create_f08
end interface  MPI_Dist_graph_create

interface  MPI_Dist_graph_create_adjacent
subroutine MPI_Dist_graph_create_adjacent_f08(comm_old,indegree,sources,sourceweights, &
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
end subroutine MPI_Dist_graph_create_adjacent_f08
end interface  MPI_Dist_graph_create_adjacent

interface  MPI_Dist_graph_neighbors
subroutine MPI_Dist_graph_neighbors_f08(comm,maxindegree,sources,sourceweights, &
                                        maxoutdegree,destinations,destweights,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
   INTEGER, INTENT(OUT) :: sources(maxindegree), destinations(maxoutdegree)
   INTEGER, INTENT(OUT) :: sourceweights(maxindegree), destweights(maxoutdegree)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Dist_graph_neighbors_f08
end interface  MPI_Dist_graph_neighbors

interface  MPI_Dist_graph_neighbors_count
subroutine MPI_Dist_graph_neighbors_count_f08(comm,indegree,outdegree,weighted,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: indegree, outdegree
   LOGICAL, INTENT(OUT) :: weighted
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Dist_graph_neighbors_count_f08
end interface  MPI_Dist_graph_neighbors_count

interface  MPI_Graphdims_get
subroutine MPI_Graphdims_get_f08(comm,nnodes,nedges,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: nnodes, nedges
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graphdims_get_f08
end interface  MPI_Graphdims_get

interface  MPI_Graph_create
subroutine MPI_Graph_create_f08(comm_old,nnodes,index,edges,reorder,comm_graph, &
                                ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm_old
   INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
   LOGICAL, INTENT(IN) :: reorder
   TYPE(MPI_Comm), INTENT(OUT) :: comm_graph
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graph_create_f08
end interface  MPI_Graph_create

interface  MPI_Graph_get
subroutine MPI_Graph_get_f08(comm,maxindex,maxedges,index,edges,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindex, maxedges
   INTEGER, INTENT(OUT) :: index(maxindex), edges(maxedges)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graph_get_f08
end interface  MPI_Graph_get

interface  MPI_Graph_map
subroutine MPI_Graph_map_f08(comm,nnodes,index,edges,newrank,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graph_map_f08
end interface  MPI_Graph_map

interface  MPI_Graph_neighbors
subroutine MPI_Graph_neighbors_f08(comm,rank,maxneighbors,neighbors,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxneighbors
   INTEGER, INTENT(OUT) :: neighbors(maxneighbors)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graph_neighbors_f08
end interface  MPI_Graph_neighbors

interface  MPI_Graph_neighbors_count
subroutine MPI_Graph_neighbors_count_f08(comm,rank,nneighbors,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(OUT) :: nneighbors
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Graph_neighbors_count_f08
end interface  MPI_Graph_neighbors_count

interface  MPI_Topo_test
subroutine MPI_Topo_test_f08(comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Topo_test_f08
end interface  MPI_Topo_test

! MPI_Wtick is not a wrapper function
!
interface MPI_Wtick
function  MPI_Wtick_f08( ) BIND(C,name="MPI_Wtick")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: MPI_Wtick_f08
end function MPI_Wtick_f08
end interface MPI_Wtick

! MPI_Wtime is not a wrapper function
!
interface MPI_Wtime
function  MPI_Wtime_f08( ) BIND(C,name="MPI_Wtime")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: MPI_Wtime_f08
end function MPI_Wtime_f08
end interface MPI_Wtime

interface  MPI_Abort
subroutine MPI_Abort_f08(comm,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Abort_f08
end interface  MPI_Abort

interface  MPI_Add_error_class
subroutine MPI_Add_error_class_f08(errorclass,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Add_error_class_f08
end interface  MPI_Add_error_class

interface  MPI_Add_error_code
subroutine MPI_Add_error_code_f08(errorclass,errorcode,ierror)
   implicit none
   INTEGER, INTENT(IN) :: errorclass
   INTEGER, INTENT(OUT) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Add_error_code_f08
end interface  MPI_Add_error_code

interface  MPI_Add_error_string
subroutine MPI_Add_error_string_f08(errorcode,string,ierror)
   implicit none
   integer, intent(in) :: errorcode
   character(len=*), intent(in) :: string
   integer, optional, intent(out) :: ierror
end subroutine MPI_Add_error_string_f08
end interface  MPI_Add_error_string

interface  MPI_Alloc_mem
subroutine MPI_Alloc_mem_f08(size,info,baseptr,ierror)
   use, intrinsic :: ISO_C_BINDING, only : C_PTR
   use :: mpi_f08_types, only : MPI_Info, MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(C_PTR), INTENT(OUT) :: baseptr
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Alloc_mem_f08
end interface  MPI_Alloc_mem

interface  MPI_Comm_call_errhandler
subroutine MPI_Comm_call_errhandler_f08(comm,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_call_errhandler_f08
end interface  MPI_Comm_call_errhandler

interface  MPI_Comm_create_errhandler
subroutine MPI_Comm_create_errhandler_f08(comm_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_Comm_errhandler_function
   implicit none
   PROCEDURE(MPI_Comm_errhandler_function) :: comm_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_create_errhandler_f08
end interface  MPI_Comm_create_errhandler

interface  MPI_Comm_get_errhandler
subroutine MPI_Comm_get_errhandler_f08(comm,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Errhandler
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_get_errhandler_f08
end interface  MPI_Comm_get_errhandler

interface  MPI_Comm_set_errhandler
subroutine MPI_Comm_set_errhandler_f08(comm,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Errhandler
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_set_errhandler_f08
end interface  MPI_Comm_set_errhandler

interface  MPI_Errhandler_free
subroutine MPI_Errhandler_free_f08(errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   implicit none
   TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Errhandler_free_f08
end interface  MPI_Errhandler_free

interface  MPI_Error_class
subroutine MPI_Error_class_f08(errorcode,errorclass,ierror)
   implicit none
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Error_class_f08
end interface  MPI_Error_class

interface  MPI_Error_string
subroutine MPI_Error_string_f08(errorcode,string,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_ERROR_STRING
   implicit none
   integer, intent(in) :: errorcode
   character(len=MPI_MAX_ERROR_STRING), intent(out) :: string
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine MPI_Error_string_f08
end interface  MPI_Error_string

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

interface  MPI_File_call_errhandler
subroutine MPI_File_call_errhandler_f08(fh,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_call_errhandler_f08
end interface  MPI_File_call_errhandler

interface  MPI_File_create_errhandler
subroutine MPI_File_create_errhandler_f08(file_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_File_errhandler_function
   implicit none
   PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_create_errhandler_f08
end interface  MPI_File_create_errhandler

interface  MPI_File_get_errhandler
subroutine MPI_File_get_errhandler_f08(file,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Errhandler
   implicit none
   TYPE(MPI_File), INTENT(IN) :: file
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_errhandler_f08
end interface  MPI_File_get_errhandler

interface  MPI_File_set_errhandler
subroutine MPI_File_set_errhandler_f08(file,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Errhandler
   implicit none
   TYPE(MPI_File), INTENT(IN) :: file
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_set_errhandler_f08
end interface  MPI_File_set_errhandler

! endif for OMPI_PROVIDE_MPI_FILE_INTERFACE
#endif

interface  MPI_Finalize
subroutine MPI_Finalize_f08(ierror)
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Finalize_f08
end interface  MPI_Finalize

interface  MPI_Finalized
subroutine MPI_Finalized_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Finalized_f08
end interface  MPI_Finalized

! ASYNCHRONOUS had to removed from the base argument because
! the dummy argument is not an assumed-shape array.  This will
! be okay once the Interop TR is implemented.
interface  MPI_Free_mem
subroutine MPI_Free_mem_f08(base,ierror)
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
end subroutine MPI_Free_mem_f08
end interface  MPI_Free_mem

interface  MPI_Get_processor_name
subroutine MPI_Get_processor_name_f08(name,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_PROCESSOR_NAME
   implicit none
   character(len=MPI_MAX_PROCESSOR_NAME), intent(out) :: name
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine MPI_Get_processor_name_f08
end interface  MPI_Get_processor_name

interface  MPI_Get_version
subroutine MPI_Get_version_f08(version,subversion,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: version, subversion
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_version_f08
end interface  MPI_Get_version

interface  MPI_Init
subroutine MPI_Init_f08(ierror)
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Init_f08
end interface  MPI_Init

interface  MPI_Initialized
subroutine MPI_Initialized_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Initialized_f08
end interface  MPI_Initialized

interface  MPI_Win_call_errhandler
subroutine MPI_Win_call_errhandler_f08(win,errorcode,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_call_errhandler_f08
end interface  MPI_Win_call_errhandler

interface  MPI_Win_create_errhandler
subroutine MPI_Win_create_errhandler_f08(win_errhandler_fn,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_errhandler_function
   implicit none
   PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_create_errhandler_f08
end interface  MPI_Win_create_errhandler

interface  MPI_Win_get_errhandler
subroutine MPI_Win_get_errhandler_f08(win,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Errhandler
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_get_errhandler_f08
end interface  MPI_Win_get_errhandler

interface  MPI_Win_set_errhandler
subroutine MPI_Win_set_errhandler_f08(win,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Errhandler
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_set_errhandler_f08
end interface  MPI_Win_set_errhandler

interface  MPI_Info_create
subroutine MPI_Info_create_f08(info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(OUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_create_f08
end interface  MPI_Info_create

interface  MPI_Info_delete
subroutine MPI_Info_delete_f08(info,key,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_delete_f08
end interface  MPI_Info_delete

interface  MPI_Info_dup
subroutine MPI_Info_dup_f08(info,newinfo,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Info), INTENT(OUT) :: newinfo
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_dup_f08
end interface  MPI_Info_dup

interface  MPI_Info_free
subroutine MPI_Info_free_f08(info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(INOUT) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_free_f08
end interface  MPI_Info_free

interface  MPI_Info_get
subroutine MPI_Info_get_f08(info,key,valuelen,value,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(IN) :: valuelen
   CHARACTER(LEN=valuelen), INTENT(OUT) :: value
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_get_f08
end interface  MPI_Info_get

interface  MPI_Info_get_nkeys
subroutine MPI_Info_get_nkeys_f08(info,nkeys,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: nkeys
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_get_nkeys_f08
end interface  MPI_Info_get_nkeys

interface  MPI_Info_get_nthkey
subroutine MPI_Info_get_nthkey_f08(info,n,key,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: n
   CHARACTER(lEN=*), INTENT(OUT) :: key
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_get_nthkey_f08
end interface  MPI_Info_get_nthkey

interface  MPI_Info_get_valuelen
subroutine MPI_Info_get_valuelen_f08(info,key,valuelen,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(OUT) :: valuelen
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_get_valuelen_f08
end interface  MPI_Info_get_valuelen

interface  MPI_Info_set
subroutine MPI_Info_set_f08(info,key,value,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key, value
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Info_set_f08
end interface  MPI_Info_set

interface  MPI_Close_port
subroutine MPI_Close_port_f08(port_name,ierror)
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Close_port_f08
end interface  MPI_Close_port

interface  MPI_Comm_accept
subroutine MPI_Comm_accept_f08(port_name,info,root,comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_accept_f08
end interface  MPI_Comm_accept

interface  MPI_Comm_connect
subroutine MPI_Comm_connect_f08(port_name,info,root,comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_connect_f08
end interface  MPI_Comm_connect

interface  MPI_Comm_disconnect
subroutine MPI_Comm_disconnect_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(INOUT) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_disconnect_f08
end interface  MPI_Comm_disconnect

interface  MPI_Comm_get_parent
subroutine MPI_Comm_get_parent_f08(parent,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(OUT) :: parent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_get_parent_f08
end interface  MPI_Comm_get_parent

interface  MPI_Comm_join
subroutine MPI_Comm_join_f08(fd,intercomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   INTEGER, INTENT(IN) :: fd
   TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_join_f08
end interface  MPI_Comm_join

interface  MPI_Comm_spawn
subroutine MPI_Comm_spawn_f08(command,argv,maxprocs,info,root,comm,intercomm, &
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
end subroutine MPI_Comm_spawn_f08
end interface  MPI_Comm_spawn

interface  MPI_Comm_spawn_multiple
subroutine MPI_Comm_spawn_multiple_f08(count,array_of_commands,array_of_argv,array_of_maxprocs, &
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
end subroutine MPI_Comm_spawn_multiple_f08
end interface  MPI_Comm_spawn_multiple

interface  MPI_Lookup_name
subroutine MPI_Lookup_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_MAX_PORT_NAME
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: service_name
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Lookup_name_f08
end interface  MPI_Lookup_name

interface  MPI_Open_port
subroutine MPI_Open_port_f08(info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_MAX_PORT_NAME
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Open_port_f08
end interface  MPI_Open_port

interface  MPI_Publish_name
subroutine MPI_Publish_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Publish_name_f08
end interface  MPI_Publish_name

interface  MPI_Unpublish_name
subroutine MPI_Unpublish_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Unpublish_name_f08
end interface  MPI_Unpublish_name

interface  MPI_Accumulate
subroutine MPI_Accumulate_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                              target_disp,target_count,target_datatype,op,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_ADDRESS_KIND
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Accumulate_f08
end interface  MPI_Accumulate

interface  MPI_Raccumulate
subroutine MPI_Raccumulate_f08(origin_addr,origin_count,origin_datatype,target_rank, &
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
end subroutine MPI_Raccumulate_f08
end interface  MPI_Raccumulate

interface  MPI_Get
subroutine MPI_Get_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                               target_disp,target_count,target_datatype,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Get_f08
end interface  MPI_Get

interface  MPI_Rget
subroutine MPI_Rget_f08(origin_addr,origin_count,origin_datatype,target_rank, &
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
end subroutine MPI_Rget_f08
end interface  MPI_Rget

interface  MPI_Get_accumulate
subroutine MPI_Get_accumulate_f08(origin_addr,origin_count,origin_datatype,result_addr, &
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
end subroutine MPI_Get_accumulate_f08
end interface  MPI_Get_accumulate

interface  MPI_Rget_accumulate
subroutine MPI_Rget_accumulate_f08(origin_addr,origin_count,origin_datatype,result_addr, &
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
end subroutine MPI_Rget_accumulate_f08
end interface  MPI_Rget_accumulate

interface  MPI_Put
subroutine MPI_Put_f08(origin_addr,origin_count,origin_datatype,target_rank, &
                               target_disp,target_count,target_datatype,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Put_f08
end interface  MPI_Put

interface  MPI_Rput
subroutine MPI_Rput_f08(origin_addr,origin_count,origin_datatype,target_rank, &
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
end subroutine MPI_Rput_f08
end interface  MPI_Rput

interface  MPI_Fetch_and_op
subroutine MPI_Fetch_and_op_f08(origin_addr,result_addr,datatype,target_rank, &
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
end subroutine MPI_Fetch_and_op_f08
end interface  MPI_Fetch_and_op

interface  MPI_Compare_and_swap
subroutine MPI_Compare_and_swap_f08(origin_addr,compare_addr,result_addr,datatype, &
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
end subroutine MPI_Compare_and_swap_f08
end interface  MPI_Compare_and_swap

interface  MPI_Win_complete
subroutine MPI_Win_complete_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_complete_f08
end interface  MPI_Win_complete

interface  MPI_Win_create
subroutine MPI_Win_create_f08(base,size,disp_unit,info,comm,win,ierror)
   use :: mpi_f08_types, only : MPI_Info, MPI_Comm, MPI_Win, MPI_ADDRESS_KIND
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: base
   !$PRAGMA IGNORE_TKR base
   !DIR$ IGNORE_TKR base
   !IBM* IGNORE_TKR base
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   INTEGER, INTENT(IN) :: disp_unit
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Win), INTENT(OUT) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_create_f08
end interface  MPI_Win_create

interface  MPI_Win_fence
subroutine MPI_Win_fence_f08(assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_fence_f08
end interface  MPI_Win_fence

interface  MPI_Win_free
subroutine MPI_Win_free_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(INOUT) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_free_f08
end interface  MPI_Win_free

interface  MPI_Win_get_group
subroutine MPI_Win_get_group_f08(win,group,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Group
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_get_group_f08
end interface  MPI_Win_get_group

interface  MPI_Win_lock
subroutine MPI_Win_lock_f08(lock_type,rank,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: lock_type, rank, assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_lock_f08
end interface  MPI_Win_lock

interface  MPI_Win_lock_all
subroutine MPI_Win_lock_all_f08(assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_lock_all_f08
end interface  MPI_Win_lock_all

interface  MPI_Win_post
subroutine MPI_Win_post_f08(group,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_post_f08
end interface  MPI_Win_post

interface MPI_Win_shared_query
subroutine MPI_Win_shared_query_f08(win, rank, size, disp_unit, baseptr,&
      ierror)
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
  TYPE(MPI_Win), INTENT(IN) ::  win
  INTEGER, INTENT(IN) ::  rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::  size
  INTEGER, INTENT(OUT) ::  disp_unit
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
end subroutine MPI_Win_shared_query_f08
end interface

interface  MPI_Win_start
subroutine MPI_Win_start_f08(group,assert,win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_start_f08
end interface  MPI_Win_start

interface  MPI_Win_sync
subroutine MPI_Win_sync_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Group, MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_sync_f08
end interface  MPI_Win_sync

interface  MPI_Win_test
subroutine MPI_Win_test_f08(win,flag,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_test_f08
end interface  MPI_Win_test

interface  MPI_Win_unlock
subroutine MPI_Win_unlock_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_unlock_f08
end interface  MPI_Win_unlock

interface  MPI_Win_unlock_all
subroutine MPI_Win_unlock_all_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_unlock_all_f08
end interface  MPI_Win_unlock_all

interface  MPI_Win_wait
subroutine MPI_Win_wait_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_wait_f08
end interface  MPI_Win_wait

interface  MPI_Win_flush
subroutine MPI_Win_flush_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_flush_f08
end interface  MPI_Win_flush

interface  MPI_Win_flush_local
subroutine MPI_Win_flush_local_f08(rank,win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   INTEGER, INTENT(IN) :: rank
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_flush_local_f08
end interface  MPI_Win_flush_local

interface  MPI_Win_flush_all_local
subroutine MPI_Win_flush_all_local_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_flush_all_local_f08
end interface  MPI_Win_flush_all_local

interface  MPI_Win_flush_all
subroutine MPI_Win_flush_all_f08(win,ierror)
   use :: mpi_f08_types, only : MPI_Win
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Win_flush_all_f08
end interface  MPI_Win_flush_all

interface  MPI_Grequest_complete
subroutine MPI_Grequest_complete_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Grequest_complete_f08
end interface  MPI_Grequest_complete

interface  MPI_Grequest_start
subroutine MPI_Grequest_start_f08(query_fn,free_fn,cancel_fn,extra_state,request, &
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
end subroutine MPI_Grequest_start_f08
end interface  MPI_Grequest_start

interface  MPI_Init_thread
subroutine MPI_Init_thread_f08(required,provided,ierror)
   implicit none
   INTEGER, INTENT(IN) :: required
   INTEGER, INTENT(OUT) :: provided
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Init_thread_f08
end interface  MPI_Init_thread

interface  MPI_Is_thread_main
subroutine MPI_Is_thread_main_f08(flag,ierror)
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Is_thread_main_f08
end interface  MPI_Is_thread_main

interface  MPI_Query_thread
subroutine MPI_Query_thread_f08(provided,ierror)
   implicit none
   INTEGER, INTENT(OUT) :: provided
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Query_thread_f08
end interface  MPI_Query_thread

interface  MPI_Status_set_cancelled
subroutine MPI_Status_set_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Status_set_cancelled_f08
end interface  MPI_Status_set_cancelled

interface  MPI_Status_set_elements
subroutine MPI_Status_set_elements_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Status_set_elements_f08
end interface  MPI_Status_set_elements

interface  MPI_Status_set_elements_x
subroutine MPI_Status_set_elements_x_f08(status,datatype,count,ierror)
   use :: mpi_f08_types, only : MPI_Status, MPI_Datatype, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(IN) :: count
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Status_set_elements_x_f08
end interface  MPI_Status_set_elements_x

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

interface  MPI_File_close
subroutine MPI_File_close_f08(fh,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(INOUT) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_close_f08
end interface  MPI_File_close

interface  MPI_File_delete
subroutine MPI_File_delete_f08(filename,info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: filename
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_delete_f08
end interface  MPI_File_delete

interface  MPI_File_get_amode
subroutine MPI_File_get_amode_f08(fh,amode,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: amode
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_amode_f08
end interface  MPI_File_get_amode

interface  MPI_File_get_atomicity
subroutine MPI_File_get_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_atomicity_f08
end interface  MPI_File_get_atomicity

interface  MPI_File_get_byte_offset
subroutine MPI_File_get_byte_offset_f08(fh,offset,disp,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_byte_offset_f08
end interface  MPI_File_get_byte_offset

interface  MPI_File_get_group
subroutine MPI_File_get_group_f08(fh,group,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Group
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Group), INTENT(OUT) :: group
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_group_f08
end interface  MPI_File_get_group

interface  MPI_File_get_info
subroutine MPI_File_get_info_f08(fh,info_used,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Info
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Info), INTENT(OUT) :: info_used
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_info_f08
end interface  MPI_File_get_info

interface  MPI_File_get_position
subroutine MPI_File_get_position_f08(fh,offset,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_position_f08
end interface  MPI_File_get_position

interface  MPI_File_get_position_shared
subroutine MPI_File_get_position_shared_f08(fh,offset,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_position_shared_f08
end interface  MPI_File_get_position_shared

interface  MPI_File_get_size
subroutine MPI_File_get_size_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_size_f08
end interface  MPI_File_get_size

interface  MPI_File_get_type_extent
subroutine MPI_File_get_type_extent_f08(fh,datatype,extent,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_ADDRESS_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_type_extent_f08
end interface  MPI_File_get_type_extent

interface  MPI_File_get_view
subroutine MPI_File_get_view_f08(fh,disp,etype,filetype,datarep,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   TYPE(MPI_Datatype), INTENT(OUT) :: etype
   TYPE(MPI_Datatype), INTENT(OUT) :: filetype
   CHARACTER(LEN=*), INTENT(OUT) :: datarep
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_get_view_f08
end interface  MPI_File_get_view

interface  MPI_File_iread
subroutine MPI_File_iread_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iread_f08
end interface  MPI_File_iread

interface  MPI_File_iread_at
subroutine MPI_File_iread_at_f08(fh,offset,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request, MPI_OFFSET_KIND
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iread_at_f08
end interface  MPI_File_iread_at

interface  MPI_File_iread_shared
subroutine MPI_File_iread_shared_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iread_shared_f08
end interface  MPI_File_iread_shared

interface  MPI_File_iwrite
subroutine MPI_File_iwrite_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iwrite_f08
end interface  MPI_File_iwrite

interface  MPI_File_iwrite_at
subroutine MPI_File_iwrite_at_f08(fh,offset,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request, MPI_OFFSET_KIND
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iwrite_at_f08
end interface  MPI_File_iwrite_at

interface  MPI_File_iwrite_shared
subroutine MPI_File_iwrite_shared_f08(fh,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_iwrite_shared_f08
end interface  MPI_File_iwrite_shared

interface  MPI_File_open
subroutine MPI_File_open_f08(comm,filename,amode,info,fh,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info, MPI_File
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: filename
   INTEGER, INTENT(IN) :: amode
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_File), INTENT(OUT) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_open_f08
end interface  MPI_File_open

interface  MPI_File_preallocate
subroutine MPI_File_preallocate_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_preallocate_f08
end interface  MPI_File_preallocate

interface  MPI_File_read
subroutine MPI_File_read_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_f08
end interface  MPI_File_read

interface  MPI_File_read_all
subroutine MPI_File_read_all_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_all_f08
end interface  MPI_File_read_all

interface  MPI_File_read_all_begin
subroutine MPI_File_read_all_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_all_begin_f08
end interface  MPI_File_read_all_begin

interface  MPI_File_read_all_end
subroutine MPI_File_read_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_all_end_f08
end interface  MPI_File_read_all_end

interface  MPI_File_read_at
subroutine MPI_File_read_at_f08(fh,offset,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_at_f08
end interface  MPI_File_read_at

interface  MPI_File_read_at_all
subroutine MPI_File_read_at_all_f08(fh,offset,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_at_all_f08
end interface  MPI_File_read_at_all

interface  MPI_File_read_at_all_begin
subroutine MPI_File_read_at_all_begin_f08(fh,offset,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_at_all_begin_f08
end interface  MPI_File_read_at_all_begin

interface  MPI_File_read_at_all_end
subroutine MPI_File_read_at_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_at_all_end_f08
end interface  MPI_File_read_at_all_end

interface  MPI_File_read_ordered
subroutine MPI_File_read_ordered_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_ordered_f08
end interface  MPI_File_read_ordered

interface  MPI_File_read_ordered_begin
subroutine MPI_File_read_ordered_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_ordered_begin_f08
end interface  MPI_File_read_ordered_begin

interface  MPI_File_read_ordered_end
subroutine MPI_File_read_ordered_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_read_ordered_end_f08
end interface  MPI_File_read_ordered_end

interface  MPI_File_read_shared
subroutine MPI_File_read_shared_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_read_shared_f08
end interface  MPI_File_read_shared

interface  MPI_File_seek
subroutine MPI_File_seek_f08(fh,offset,whence,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_seek_f08
end interface  MPI_File_seek

interface  MPI_File_seek_shared
subroutine MPI_File_seek_shared_f08(fh,offset,whence,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_seek_shared_f08
end interface  MPI_File_seek_shared

interface  MPI_File_set_atomicity
subroutine MPI_File_set_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(IN) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_set_atomicity_f08
end interface  MPI_File_set_atomicity

interface  MPI_File_set_info
subroutine MPI_File_set_info_f08(fh,info,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Info
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_set_info_f08
end interface  MPI_File_set_info

interface  MPI_File_set_size
subroutine MPI_File_set_size_f08(fh,size,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_set_size_f08
end interface  MPI_File_set_size

interface  MPI_File_set_view
subroutine MPI_File_set_view_f08(fh,disp,etype,filetype,datarep,info,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Info, MPI_OFFSET_KIND
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
   TYPE(MPI_Datatype), INTENT(IN) :: etype
   TYPE(MPI_Datatype), INTENT(IN) :: filetype
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_set_view_f08
end interface  MPI_File_set_view

interface  MPI_File_sync
subroutine MPI_File_sync_f08(fh,ierror)
   use :: mpi_f08_types, only : MPI_File
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_sync_f08
end interface  MPI_File_sync

interface  MPI_File_write
subroutine MPI_File_write_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_f08
end interface  MPI_File_write

interface  MPI_File_write_all
subroutine MPI_File_write_all_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_all_f08
end interface  MPI_File_write_all

interface  MPI_File_write_all_begin
subroutine MPI_File_write_all_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_all_begin_f08
end interface  MPI_File_write_all_begin

interface  MPI_File_write_all_end
subroutine MPI_File_write_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_all_end_f08
end interface  MPI_File_write_all_end

interface  MPI_File_write_at
subroutine MPI_File_write_at_f08(fh,offset,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_at_f08
end interface  MPI_File_write_at

interface  MPI_File_write_at_all
subroutine MPI_File_write_at_all_f08(fh,offset,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_at_all_f08
end interface  MPI_File_write_at_all

interface  MPI_File_write_at_all_begin
subroutine MPI_File_write_at_all_begin_f08(fh,offset,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_at_all_begin_f08
end interface  MPI_File_write_at_all_begin

interface  MPI_File_write_at_all_end
subroutine MPI_File_write_at_all_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_at_all_end_f08
end interface  MPI_File_write_at_all_end

interface  MPI_File_write_ordered
subroutine MPI_File_write_ordered_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_ordered_f08
end interface  MPI_File_write_ordered

interface  MPI_File_write_ordered_begin
subroutine MPI_File_write_ordered_begin_f08(fh,buf,count,datatype,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype
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
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_ordered_begin_f08
end interface  MPI_File_write_ordered_begin

interface  MPI_File_write_ordered_end
subroutine MPI_File_write_ordered_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_File_write_ordered_end_f08
end interface  MPI_File_write_ordered_end

interface  MPI_File_write_shared
subroutine MPI_File_write_shared_f08(fh,buf,count,datatype,status,ierror)
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
end subroutine MPI_File_write_shared_f08
end interface  MPI_File_write_shared

! endif for OMPI_PROVIDE_MPI_FILE_INTERFACE
#endif

interface  MPI_Register_datarep
subroutine MPI_Register_datarep_f08(datarep,read_conversion_fn,write_conversion_fn, &
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
end subroutine MPI_Register_datarep_f08
end interface  MPI_Register_datarep

!
! MPI_Sizeof is generic for numeric types.  This ignore TKR interface
! is replaced by the specific generics.  Implemented in mpi_sizeof_mod.F90.
!
!subroutine MPI_Sizeof(x,size,ierror)
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
!end subroutine MPI_Sizeof

interface  MPI_Type_create_f90_complex
subroutine MPI_Type_create_f90_complex_f08(p,r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: p, r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_f90_complex_f08
end interface  MPI_Type_create_f90_complex

interface  MPI_Type_create_f90_integer
subroutine MPI_Type_create_f90_integer_f08(r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_f90_integer_f08
end interface  MPI_Type_create_f90_integer

interface  MPI_Type_create_f90_real
subroutine MPI_Type_create_f90_real_f08(p,r,newtype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: p, r
   TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_create_f90_real_f08
end interface  MPI_Type_create_f90_real

interface  MPI_Type_match_size
subroutine MPI_Type_match_size_f08(typeclass,size,datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype
   implicit none
   INTEGER, INTENT(IN) :: typeclass, size
   TYPE(MPI_Datatype), INTENT(OUT) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Type_match_size_f08
end interface  MPI_Type_match_size

interface  MPI_Pcontrol
subroutine MPI_Pcontrol_f08(level)
   implicit none
   INTEGER, INTENT(IN) :: level
end subroutine MPI_Pcontrol_f08
end interface  MPI_Pcontrol


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! New routines to MPI-3
!

interface  MPI_Comm_split_type
subroutine MPI_Comm_split_type_f08(comm,split_type,key,info,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: split_type
   INTEGER, INTENT(IN) :: key
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Comm_split_type_f08
end interface  MPI_Comm_split_type

interface  MPI_F_sync_reg
subroutine MPI_F_sync_reg_f08(buf)
   implicit none
   !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !GCC$ ATTRIBUTES NO_ARG_CHECK :: buf
   !$PRAGMA IGNORE_TKR buf
   !DIR$ IGNORE_TKR buf
   !IBM* IGNORE_TKR buf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
end subroutine MPI_F_sync_reg_f08
end interface  MPI_F_sync_reg

interface  MPI_Get_library_version
subroutine MPI_Get_library_version_f08(version,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_LIBRARY_VERSION_STRING
   implicit none
   character(len=MPI_MAX_LIBRARY_VERSION_STRING), intent(out) :: version
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
end subroutine MPI_Get_library_version_f08
end interface  MPI_Get_library_version

interface  MPI_Mprobe
subroutine MPI_Mprobe_f08(source,tag,comm,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Message, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Message), INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Mprobe_f08
end interface  MPI_Mprobe

interface  MPI_Improbe
subroutine MPI_Improbe_f08(source,tag,comm,flag,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Message, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Message), INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Improbe_f08
end interface  MPI_Improbe

interface  MPI_Imrecv
subroutine MPI_Imrecv_f08(buf,count,datatype,message,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Message, MPI_Request
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
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine MPI_Imrecv_f08
end interface  MPI_Imrecv

interface  MPI_Mrecv
subroutine MPI_Mrecv_f08(buf,count,datatype,message,status,ierror)
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
end subroutine MPI_Mrecv_f08
end interface  MPI_Mrecv

interface  MPI_Neighbor_allgather
subroutine MPI_Neighbor_allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Neighbor_allgather_f08
end interface  MPI_Neighbor_allgather

interface  MPI_Ineighbor_allgather
subroutine MPI_Ineighbor_allgather_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Ineighbor_allgather_f08
end interface  MPI_Ineighbor_allgather

interface  MPI_Neighbor_allgatherv
subroutine MPI_Neighbor_allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Neighbor_allgatherv_f08
end interface  MPI_Neighbor_allgatherv

interface  MPI_Ineighbor_allgatherv
subroutine MPI_Ineighbor_allgatherv_f08(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
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
end subroutine MPI_Ineighbor_allgatherv_f08
end interface  MPI_Ineighbor_allgatherv

interface  MPI_Neighbor_alltoall
subroutine MPI_Neighbor_alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Neighbor_alltoall_f08
end interface  MPI_Neighbor_alltoall

interface  MPI_Ineighbor_alltoall
subroutine MPI_Ineighbor_alltoall_f08(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
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
end subroutine MPI_Ineighbor_alltoall_f08
end interface  MPI_Ineighbor_alltoall

interface  MPI_Neighbor_alltoallv
subroutine MPI_Neighbor_alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
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
end subroutine MPI_Neighbor_alltoallv_f08
end interface  MPI_Neighbor_alltoallv

interface  MPI_Ineighbor_alltoallv
subroutine MPI_Ineighbor_alltoallv_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
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
end subroutine MPI_Ineighbor_alltoallv_f08
end interface  MPI_Ineighbor_alltoallv

interface  MPI_Neighbor_alltoallw
subroutine MPI_Neighbor_alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
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
end subroutine MPI_Neighbor_alltoallw_f08
end interface  MPI_Neighbor_alltoallw

interface  MPI_Ineighbor_alltoallw
subroutine MPI_Ineighbor_alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
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
end subroutine MPI_Ineighbor_alltoallw_f08
end interface  MPI_Ineighbor_alltoallw

end module mpi_f08_interfaces
