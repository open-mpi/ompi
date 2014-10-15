! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2012      The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2012      Inria.  All rights reserved.
! $COPYRIGHT$
!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the back-end OMPI implementation subroutine name (e.g.,
! "ompi_init_f").
!

#include "ompi/mpi/fortran/configure-fortran-output.h"

!
! Most of the "wrapper" subroutines in the mpi_f08 module (i.e., all
! the ones prototyped in this file) are simple routines that simply
! invoke a back-end ompi_*_f() subroutine, which is BIND(C)-bound to a
! back-end C function.  Easy-peasy.
!
! However, a bunch of MPI Fortran subroutines use LOGICAL dummy
! parameters, and Fortran disallows passing LOGICAL parameters to
! BIND(C) routines (because the .TRUE. and .FALSE. values are not
! standardized (!)).  Hence, for these
! subroutines-with-LOGICAL-params, we have to be creative on how to
! invoke the back-end ompi_*_f() C function.  There are 2 cases:

! 1. If the Fortran interface has a LOGICAL parameter and no
! TYPE(MPI_Status) parameter, the individual wrapper implementation
! files (e.g., finalized_f08.F90) use the "mpi" module to get a
! interface for the subroutine and call the PMPI_* name of the
! function, which then invokes the corresponding function in the
! ompi/mpi/fortran/mpif-h directory.
!
! This is a bit of a hack: the "mpi" module will provide the right
! Fortran interface so that the compiler can verify that we're passing
! the right types (e.g., convert MPI handles from comm to
! comm%MPI_VAL).  But here's the hack part: when we pass *unbounded
! arrays* of handles (e.g., the sendtypes and recvtypes arrays
! MPI_Alltoallw), we declare that the corresponding ompi_*_f()
! subroutine takes a *scalar*, and then we pass sendtypes(0)%MPI_VAL.
!
! >>>THIS IS A LIE!<<< We're passing a scalar to something that
! expects an array.
!
! However, remember that Fortran passes by reference.  So the compiler
! will pass a pointer to sendtypes(0)%MPI_VAL (i.e., the first integer
! in the array).  And since the mpi_f08 handles were cleverly designed
! to be exactly equivalent to a single INTEGER, an array of mpi_f08
! handles is exactly equivalent to an array of INTEGERS.  So passing
! an address to the first MPI_VAL is exactly the same as passing an
! array of INTEGERS.
!
! Specifically: the back-end C function (in *.c files in
! ompi/mpi/fortran/mpif-h) gets an (MPI_Fint*), and it's all good.
!
! The key here is that there is a disconnect between Fortran and C:
! we're telling the Fortran compiler what the C interface is, and
! we're lying.  But just a little bit.  No one gets hurt.
!
! Yes, this is a total hack.  But Craig Rasumussen tells me that this
! is actually quite a common hack in the Fortran developer world, so
! we shouldn't feel bad about using it.  Shrug.
!
! 2. If the Fortran interface has both LOGICAL and TYPE(MPI_Status)
! parameters, then we have to do even more tricks than we described
! above. :-(
!
! The problem occurs because in the mpi_f08 module, an MPI_Status is
! TYPE(MPI_Status), but in the mpi module, it's INTEGER,
! DIMENSION(MPI_STATUS_SIZE).  Just like MPI handles, TYPE(MPI_Status)
! was cleverly designed so that it can be identical (in terms of a
! memory map) to INTEGER, DIMENSION(MPI_STATUS_SIZE).  So we just have
! to fool the compiler into accepting it (it's the same C<-->Fortran
! disconnect from #1).
!
! So in this case, we actually don't "use mpi" at all -- we just add
! an "interface" block for the PMPI_* subroutine that we want to call.
! And we lie in that interface block, saying that the status argument
! is TYPE(MPI_Status) (rather than an INTEGER,
! DIMENSION(MPI_STATUS_SIZE), which is what it *really* is) -- i.e.,
! the same type that we already have.
!
! For the C programmers reading this, this is very much analogous to
! something like this:
!
! $ cat header.h
! void foo(int *param);
! $ cat source.c
! #include "header.h"
! // Pretend that we *know* somehow that param will point to exactly
! // sizeof(int) bytes.
! void bar(char *param) {
!     foo(param); // <-- This generates a compiler warning
! }
!
! To fix the compiler warning, instead of including "header.h", we
! just put a byte-equivalent prototype in source.c:
!
! $ cat source.c
! void foo(char *param);
! void bar(char *param) {
!     foo(param);
! }
!
! And now it compiles without warning.
!
! The main difference here is that in Fortran, it is an error -- not a
! warning.
!
! Again, we're making the Fortran compiler happy, but we're lying
! because we know the back-end memory representation of the two types
! is the same.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Wasn't that simple?  Here's the list of subroutines that are not
! prototyped in this file because they fall into case #1 or #2, above.
!
! MPI_Cart_create
! MPI_Cart_get
! MPI_Cart_map
! MPI_Cart_sub
! MPI_Comm_get_attr
! MPI_Comm_test_inter
! MPI_Dist_graph_create
! MPI_Dist_graph_create_adjacent
! MPI_Dist_graph_neighbors_count
! MPI_File_get_atomicity
! MPI_File_set_atomicity
! MPI_Finalized
! MPI_Graph_create
! MPI_Improbe
! MPI_Info_get
! MPI_Info_get_valuelen
! MPI_Initialized
! MPI_Intercomm_merge
! MPI_Iprobe
! MPI_Is_thread_main
! MPI_Op_commutative
! MPI_Op_create
! MPI_Request_get_status
! MPI_Status_set_cancelled
! MPI_Test
! MPI_Testall
! MPI_Testany
! MPI_Testsome
! MPI_Test_cancelled
! MPI_Type_get_attr
! MPI_Win_get_attr
! MPI_Win_test
!

interface

subroutine ompi_bsend_f(buf,count,datatype,dest,tag,comm,ierror) &
   BIND(C, name="ompi_bsend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_bsend_f

subroutine ompi_bsend_init_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_bsend_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_bsend_init_f

subroutine ompi_buffer_attach_f(buffer,size,ierror) &
   BIND(C, name="ompi_buffer_attach_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer
   INTEGER, INTENT(IN) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_buffer_attach_f

subroutine ompi_buffer_detach_f(buffer_addr,size,ierror) &
   BIND(C, name="ompi_buffer_detach_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer_addr
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_buffer_detach_f

subroutine ompi_cancel_f(request,ierror) &
   BIND(C, name="ompi_cancel_f")
   implicit none
   INTEGER, INTENT(IN) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_cancel_f

subroutine ompi_get_count_f(status,datatype,count,ierror) &
   BIND(C, name="ompi_get_count_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_count_f

subroutine ompi_ibsend_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_ibsend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ibsend_f

subroutine ompi_irecv_f(buf,count,datatype,source,tag,comm,request,ierror) &
   BIND(C, name="ompi_irecv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_irecv_f

subroutine ompi_irsend_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_irsend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_irsend_f

subroutine ompi_isend_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_isend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_isend_f

subroutine ompi_issend_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_issend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_issend_f

subroutine ompi_probe_f(source,tag,comm,status,ierror) &
   BIND(C, name="ompi_probe_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   INTEGER, INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_probe_f

subroutine ompi_recv_f(buf,count,datatype,source,tag,comm,status,ierror) &
   BIND(C, name="ompi_recv_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_recv_f

subroutine ompi_recv_init_f(buf,count,datatype,source,tag,comm,request,ierror) &
   BIND(C, name="ompi_recv_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, source, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_recv_init_f

subroutine ompi_request_free_f(request,ierror) &
   BIND(C, name="ompi_request_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_request_free_f

subroutine ompi_rsend_f(buf,count,datatype,dest,tag,comm,ierror) &
   BIND(C, name="ompi_rsend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_rsend_f

subroutine ompi_rsend_init_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_rsend_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_rsend_init_f

subroutine ompi_send_f(buf,count,datatype,dest,tag,comm,ierror) &
   BIND(C, name="ompi_send_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_send_f

subroutine ompi_sendrecv_f(sendbuf,sendcount,sendtype,dest,sendtag,recvbuf, &
                           recvcount,recvtype,source,recvtag,comm,status,ierror) &
   BIND(C, name="ompi_sendrecv_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_sendrecv_f

subroutine ompi_sendrecv_replace_f(buf,count,datatype,dest,sendtag,source, &
                                   recvtag,comm,status,ierror) &
   BIND(C, name="ompi_sendrecv_replace_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_sendrecv_replace_f

subroutine ompi_send_init_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_send_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_send_init_f

subroutine ompi_ssend_f(buf,count,datatype,dest,tag,comm,ierror) &
   BIND(C, name="ompi_ssend_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ssend_f

subroutine ompi_ssend_init_f(buf,count,datatype,dest,tag,comm,request,ierror) &
   BIND(C, name="ompi_ssend_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count, dest, tag
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ssend_init_f

subroutine ompi_start_f(request,ierror) &
   BIND(C, name="ompi_start_f")
   implicit none
   INTEGER, INTENT(INOUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_start_f

subroutine ompi_startall_f(count,array_of_requests,ierror) &
   BIND(C, name="ompi_startall_f")
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_startall_f

subroutine ompi_wait_f(request,status,ierror) &
   BIND(C, name="ompi_wait_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(INOUT) :: request
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_wait_f

subroutine ompi_waitall_f(count,array_of_requests,array_of_statuses,ierror) &
   BIND(C, name="ompi_waitall_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(INOUT) :: array_of_requests(count)
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(count)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_waitall_f

subroutine ompi_waitany_f(count,array_of_requests,index,status,ierror) &
   BIND(C, name="ompi_waitany_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_waitany_f

subroutine ompi_waitsome_f(incount,array_of_requests,outcount, &
                           array_of_indices,array_of_statuses,ierror) &
   BIND(C, name="ompi_waitsome_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   INTEGER, INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount
   INTEGER, INTENT(OUT) :: array_of_indices(*)
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_waitsome_f

subroutine ompi_get_address_f(location,address,ierror) &
   BIND(C, name="ompi_get_address_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: location
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: address
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_address_f

subroutine ompi_get_elements_f(status,datatype,count,ierror) &
   BIND(C, name="ompi_get_elements_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: count
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_elements_f

subroutine ompi_get_elements_x_f(status,datatype,count,ierror) &
   BIND(C, name="ompi_get_elements_x_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: count
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_elements_x_f

subroutine ompi_pack_f(inbuf,incount,datatype,outbuf,outsize, &
                       position,comm,ierror) &
   BIND(C, name="ompi_pack_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER, INTENT(IN) :: incount, outsize
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(INOUT) :: position
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_pack_f

subroutine ompi_pack_external_f(datarep,inbuf,incount,datatype, &
                                outbuf,outsize,position,ierror,datarep_len) &
   BIND(C, name="ompi_pack_external_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: datarep
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER, INTENT(IN) :: incount
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: outsize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_pack_external_f

subroutine ompi_pack_external_size_f(datarep,incount,datatype,size,ierror,datarep_len) &
   BIND(C, name="ompi_pack_external_size_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: incount
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_pack_external_size_f

subroutine ompi_pack_size_f(incount,datatype,comm,size,ierror) &
   BIND(C, name="ompi_pack_size_f")
   implicit none
   INTEGER, INTENT(IN) :: incount
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_pack_size_f

subroutine ompi_type_commit_f(datatype,ierror) &
   BIND(C, name="ompi_type_commit_f")
   implicit none
   INTEGER, INTENT(INOUT) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_commit_f

subroutine ompi_type_contiguous_f(count,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_contiguous_f")
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_contiguous_f

subroutine ompi_type_create_darray_f(size,rank,ndims,&
                    array_of_gsizes,array_of_distribs,array_of_dargs,array_of_psizes,&
                    order,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_darray_f")
   implicit none
   INTEGER, INTENT(IN) :: size, rank, ndims, order
   INTEGER, INTENT(IN) :: array_of_gsizes(ndims), array_of_distribs(ndims)
   INTEGER, INTENT(IN) :: array_of_dargs(ndims), array_of_psizes(ndims)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_darray_f

subroutine ompi_type_create_hindexed_f(count,array_of_blocklengths, &
                                       array_of_displacements,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_hindexed_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_hindexed_f

subroutine ompi_type_create_hvector_f(count,blocklength,stride,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_hvector_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: stride
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_hvector_f

subroutine ompi_type_create_indexed_block_f(count,blocklength, &
                            array_of_displacements,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_indexed_block_f")
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER, INTENT(IN) :: array_of_displacements(count)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_indexed_block_f

subroutine ompi_type_create_hindexed_block_f(count,blocklength, &
                            array_of_displacements,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_hindexed_block_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_hindexed_block_f

subroutine ompi_type_create_resized_f(oldtype,lb,extent,newtype,ierror) &
   BIND(C, name="ompi_type_create_resized_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_resized_f

subroutine ompi_type_create_struct_f(count,array_of_blocklengths, &
                           array_of_displacements,array_of_types,newtype,ierror) &
   BIND(C, name="ompi_type_create_struct_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
   INTEGER, INTENT(IN) :: array_of_types(count)
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_struct_f

subroutine ompi_type_create_subarray_f(ndims,array_of_sizes, &
                    array_of_subsizes,array_of_starts,order,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_create_subarray_f")
   implicit none
   INTEGER, INTENT(IN) :: ndims, order
   INTEGER, INTENT(IN) :: array_of_sizes(*), array_of_subsizes(*), array_of_starts(*)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_subarray_f

subroutine ompi_type_dup_f(type,newtype,ierror) &
   BIND(C, name="ompi_type_dup_f")
   implicit none
   INTEGER, INTENT(IN) :: type
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_dup_f

subroutine ompi_type_free_f(datatype,ierror) &
   BIND(C, name="ompi_type_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_free_f

subroutine ompi_type_get_contents_f(datatype,max_integers,max_addresses, &
                    max_datatypes,array_of_integers,array_of_addresses, &
                    array_of_datatypes,ierror) &
   BIND(C, name="ompi_type_get_contents_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
   INTEGER, INTENT(OUT) :: array_of_integers(max_integers)
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: array_of_addresses(max_addresses)
   INTEGER, INTENT(OUT) :: array_of_datatypes(max_datatypes)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_contents_f

subroutine ompi_type_get_envelope_f(datatype,num_integers, &
                                    num_addresses,num_datatypes,combiner,ierror) &
   BIND(C, name="ompi_type_get_envelope_f")
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes, combiner
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_envelope_f

subroutine ompi_type_get_extent_f(datatype,lb,extent,ierror) &
   BIND(C, name="ompi_type_get_extent_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_extent_f

subroutine ompi_type_get_extent_x_f(datatype,lb,extent,ierror) &
   BIND(C, name="ompi_type_get_extent_x_f")
   use :: mpi_f08_types, only : MPI_COUNT_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: lb, extent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_extent_x_f

subroutine ompi_type_get_true_extent_f(datatype,true_lb,true_extent,ierror) &
   BIND(C, name="ompi_type_get_true_extent_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_true_extent_f

subroutine ompi_type_get_true_extent_x_f(datatype,true_lb,true_extent,ierror) &
   BIND(C, name="ompi_type_get_true_extent_x_f")
   use :: mpi_f08_types, only : MPI_COUNT_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_get_true_extent_x_f

subroutine ompi_type_indexed_f(count,array_of_blocklengths, &
                               array_of_displacements,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_indexed_f")
   implicit none
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: array_of_blocklengths(count), array_of_displacements(count)
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_indexed_f

subroutine ompi_type_size_f(datatype,size,ierror) &
   BIND(C, name="ompi_type_size_f")
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_size_f

subroutine ompi_type_size_x_f(datatype,size,ierror) &
   BIND(C, name="ompi_type_size_x_f")
   use :: mpi_f08_types, only : MPI_COUNT_KIND
   implicit none
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_size_x_f

subroutine ompi_type_vector_f(count,blocklength,stride,oldtype,newtype,ierror) &
   BIND(C, name="ompi_type_vector_f")
   implicit none
   INTEGER, INTENT(IN) :: count, blocklength, stride
   INTEGER, INTENT(IN) :: oldtype
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_vector_f

subroutine ompi_unpack_f(inbuf,insize,position,outbuf,outcount, &
                         datatype,comm,ierror) &
   BIND(C, name="ompi_unpack_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER, INTENT(IN) :: insize, outcount
   INTEGER, INTENT(INOUT) :: position
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_unpack_f

subroutine ompi_unpack_external_f(datarep,inbuf,insize,position, &
                                  outbuf,outcount,datatype,ierror,datarep_len) &
   BIND(C, name="ompi_unpack_external_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: datarep
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: insize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, INTENT(IN) :: outcount
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_unpack_external_f

subroutine ompi_allgather_f(sendbuf,sendcount,sendtype,recvbuf, &
                            recvcount,recvtype,comm,ierror) &
   BIND(C, name="ompi_allgather_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_allgather_f

subroutine ompi_iallgather_f(sendbuf,sendcount,sendtype,recvbuf, &
                            recvcount,recvtype,comm,request,ierror) &
   BIND(C, name="ompi_iallgather_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iallgather_f

subroutine ompi_allgatherv_f(sendbuf,sendcount,sendtype,recvbuf, &
                             recvcounts,displs,recvtype,comm,ierror) &
   BIND(C, name="ompi_allgatherv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_allgatherv_f

subroutine ompi_iallgatherv_f(sendbuf,sendcount,sendtype,recvbuf, &
                             recvcounts,displs,recvtype,comm,request,ierror) &
   BIND(C, name="ompi_iallgatherv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iallgatherv_f

subroutine ompi_allreduce_f(sendbuf,recvbuf,count,datatype,op,comm,ierror) &
   BIND(C, name="ompi_allreduce_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_allreduce_f

subroutine ompi_iallreduce_f(sendbuf,recvbuf,count,datatype,op,comm,request,ierror) &
   BIND(C, name="ompi_iallreduce_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iallreduce_f

subroutine ompi_alltoall_f(sendbuf,sendcount,sendtype,recvbuf, &
                           recvcount,recvtype,comm,ierror) &
   BIND(C, name="ompi_alltoall_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_alltoall_f

subroutine ompi_ialltoall_f(sendbuf,sendcount,sendtype,recvbuf, &
                           recvcount,recvtype,comm,request,ierror) &
   BIND(C, name="ompi_ialltoall_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ialltoall_f

subroutine ompi_alltoallv_f(sendbuf,sendcounts,sdispls,sendtype, &
                            recvbuf,recvcounts,rdispls,recvtype,comm,ierror) &
   BIND(C, name="ompi_alltoallv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_alltoallv_f

subroutine ompi_ialltoallv_f(sendbuf,sendcounts,sdispls,sendtype, &
                            recvbuf,recvcounts,rdispls,recvtype,comm,request,ierror) &
   BIND(C, name="ompi_ialltoallv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ialltoallv_f

subroutine ompi_alltoallw_f(sendbuf,sendcounts,sdispls,sendtypes, &
                            recvbuf,recvcounts,rdispls,recvtypes,comm,ierror) &
   BIND(C, name="ompi_alltoallw_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes
   INTEGER, INTENT(IN) :: recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_alltoallw_f

subroutine ompi_ialltoallw_f(sendbuf,sendcounts,sdispls,sendtypes, &
                            recvbuf,recvcounts,rdispls,recvtypes,comm,request,ierror) &
   BIND(C, name="ompi_ialltoallw_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes
   INTEGER, INTENT(IN) :: recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ialltoallw_f

subroutine ompi_barrier_f(comm,ierror) &
   BIND(C, name="ompi_barrier_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_barrier_f

subroutine ompi_ibarrier_f(comm,request,ierror) &
   BIND(C, name="ompi_ibarrier_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ibarrier_f

subroutine ompi_bcast_f(buffer,count,datatype,root,comm,ierror) &
   BIND(C, name="ompi_bcast_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_bcast_f

subroutine ompi_ibcast_f(buffer,count,datatype,root,comm,request,ierror) &
   BIND(C, name="ompi_ibcast_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ibcast_f

subroutine ompi_exscan_f(sendbuf,recvbuf,count,datatype,op,comm,ierror) &
   BIND(C, name="ompi_exscan_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_exscan_f

subroutine ompi_iexscan_f(sendbuf,recvbuf,count,datatype,op,comm,request,ierror) &
   BIND(C, name="ompi_iexscan_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iexscan_f

subroutine ompi_gather_f(sendbuf,sendcount,sendtype,recvbuf, &
                         recvcount,recvtype,root,comm,ierror) &
   BIND(C, name="ompi_gather_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_gather_f

subroutine ompi_igather_f(sendbuf,sendcount,sendtype,recvbuf, &
                         recvcount,recvtype,root,comm,request,ierror) &
   BIND(C, name="ompi_igather_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_igather_f

subroutine ompi_gatherv_f(sendbuf,sendcount,sendtype,recvbuf, &
                          recvcounts,displs,recvtype,root,comm,ierror) &
   BIND(C, name="ompi_gatherv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, root
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_gatherv_f

subroutine ompi_igatherv_f(sendbuf,sendcount,sendtype,recvbuf, &
                          recvcounts,displs,recvtype,root,comm,request,ierror) &
   BIND(C, name="ompi_igatherv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, root
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_igatherv_f

subroutine ompi_op_free_f(op,ierror) &
   BIND(C, name="ompi_op_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: op
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_op_free_f

subroutine ompi_reduce_f(sendbuf,recvbuf,count,datatype,op,root,comm,ierror) &
   BIND(C, name="ompi_reduce_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_reduce_f

subroutine ompi_ireduce_f(sendbuf,recvbuf,count,datatype,op,root,comm,request,ierror) &
   BIND(C, name="ompi_ireduce_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ireduce_f

subroutine ompi_reduce_local_f(inbuf,inoutbuf,count,datatype,op,ierror) &
   BIND(C, name="ompi_reduce_local_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, inoutbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_reduce_local_f

subroutine ompi_reduce_scatter_f(sendbuf,recvbuf,recvcounts, &
                                 datatype,op,comm,ierror) &
   BIND(C, name="ompi_reduce_scatter_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcounts(*)
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_reduce_scatter_f

subroutine ompi_ireduce_scatter_f(sendbuf,recvbuf,recvcounts, &
                                 datatype,op,comm,request,ierror) &
   BIND(C, name="ompi_ireduce_scatter_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcounts(*)
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ireduce_scatter_f

subroutine ompi_reduce_scatter_block_f(sendbuf,recvbuf,recvcount, &
                                       datatype,op,comm,ierror) &
   BIND(C, name="ompi_reduce_scatter_block_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_reduce_scatter_block_f

subroutine ompi_ireduce_scatter_block_f(sendbuf,recvbuf,recvcount, &
                                       datatype,op,comm,request,ierror) &
   BIND(C, name="ompi_ireduce_scatter_block_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ireduce_scatter_block_f

subroutine ompi_scan_f(sendbuf,recvbuf,count,datatype,op,comm,ierror) &
   BIND(C, name="ompi_scan_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_scan_f

subroutine ompi_iscan_f(sendbuf,recvbuf,count,datatype,op,comm,request,ierror) &
   BIND(C, name="ompi_iscan_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iscan_f

subroutine ompi_scatter_f(sendbuf,sendcount,sendtype,recvbuf, &
                          recvcount,recvtype,root,comm,ierror) &
   BIND(C, name="ompi_scatter_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_scatter_f

subroutine ompi_iscatter_f(sendbuf,sendcount,sendtype,recvbuf, &
                          recvcount,recvtype,root,comm,request,ierror) &
   BIND(C, name="ompi_iscatter_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iscatter_f

subroutine ompi_scatterv_f(sendbuf,sendcounts,displs,sendtype, &
                           recvbuf,recvcount,recvtype,root,comm,ierror) &
   BIND(C, name="ompi_scatterv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount, root
   INTEGER, INTENT(IN) :: sendcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_scatterv_f

subroutine ompi_iscatterv_f(sendbuf,sendcounts,displs,sendtype, &
                           recvbuf,recvcount,recvtype,root,comm,request,ierror) &
   BIND(C, name="ompi_iscatterv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount, root
   INTEGER, INTENT(IN) :: sendcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_iscatterv_f

subroutine ompi_comm_compare_f(comm1,comm2,result,ierror) &
   BIND(C, name="ompi_comm_compare_f")
   implicit none
   INTEGER, INTENT(IN) :: comm1
   INTEGER, INTENT(IN) :: comm2
   INTEGER, INTENT(OUT) :: result
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_compare_f

subroutine ompi_comm_create_f(comm,group,newcomm,ierror) &
   BIND(C, name="ompi_comm_create_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_create_f

subroutine ompi_comm_create_group_f(comm, group, tag, newcomm, ierror) &
   BIND(C, name="ompi_comm_create_group_f")
   implicit none
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(in) :: tag
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine ompi_comm_create_group_f

subroutine ompi_comm_create_keyval_f(comm_copy_attr_fn,comm_delete_attr_fn, &
                                     comm_keyval,extra_state,ierror) &
   BIND(C, name="ompi_comm_create_keyval_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: comm_copy_attr_fn
   type(c_funptr), value :: comm_delete_attr_fn
   INTEGER, INTENT(OUT) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_create_keyval_f

subroutine ompi_comm_delete_attr_f(comm,comm_keyval,ierror) &
   BIND(C, name="ompi_comm_delete_attr_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_delete_attr_f

subroutine ompi_comm_dup_f(comm,newcomm,ierror) &
   BIND(C, name="ompi_comm_dup_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_dup_f

subroutine ompi_comm_dup_with_info_f(comm, info, newcomm, ierror) &
   BIND(C, name="ompi_comm_dup_with_info_f")
   implicit none
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine ompi_comm_dup_with_info_f

subroutine ompi_comm_free_f(comm,ierror) &
   BIND(C, name="ompi_comm_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_free_f

subroutine ompi_comm_free_keyval_f(comm_keyval,ierror) &
   BIND(C, name="ompi_comm_free_keyval_f")
   implicit none
   INTEGER, INTENT(INOUT) :: comm_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_free_keyval_f

subroutine ompi_comm_get_info_f(comm,info_used,ierror) &
   BIND(C, name="ompi_comm_get_info_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: info_used
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_get_info_f

subroutine ompi_comm_get_name_f(comm,comm_name,resultlen,ierror,comm_name_len) &
   BIND(C, name="ompi_comm_get_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: comm
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: comm_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: comm_name_len
end subroutine ompi_comm_get_name_f

subroutine ompi_comm_group_f(comm,group,ierror) &
   BIND(C, name="ompi_comm_group_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: group
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_group_f

subroutine ompi_comm_idup_f(comm, newcomm, request, ierror) &
   BIND(C, name="ompi_comm_idup_f")
   implicit none
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine ompi_comm_idup_f

subroutine ompi_comm_rank_f(comm,rank,ierror) &
   BIND(C, name="ompi_comm_rank_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: rank
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_rank_f

subroutine ompi_comm_remote_group_f(comm,group,ierror) &
   BIND(C, name="ompi_comm_remote_group_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: group
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_remote_group_f

subroutine ompi_comm_remote_size_f(comm,size,ierror) &
   BIND(C, name="ompi_comm_remote_size_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_remote_size_f

subroutine ompi_comm_set_attr_f(comm,comm_keyval,attribute_val,ierror) &
   BIND(C, name="ompi_comm_set_attr_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_set_attr_f

subroutine ompi_comm_set_info_f(comm,info,ierror) &
   BIND(C, name="ompi_comm_get_info_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_set_info_f

subroutine ompi_comm_set_name_f(comm,comm_name,ierror,comm_name_len) &
   BIND(C, name="ompi_comm_set_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: comm
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: comm_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: comm_name_len
end subroutine ompi_comm_set_name_f

subroutine ompi_comm_size_f(comm,size,ierror) &
   BIND(C, name="ompi_comm_size_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_size_f

subroutine ompi_comm_split_f(comm,color,key,newcomm,ierror) &
   BIND(C, name="ompi_comm_split_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: color, key
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_split_f

subroutine ompi_group_compare_f(group1,group2,result,ierror) &
   BIND(C, name="ompi_group_compare_f")
   implicit none
   INTEGER, INTENT(IN) :: group1
   INTEGER, INTENT(IN) :: group2
   INTEGER, INTENT(OUT) :: result
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_compare_f

subroutine ompi_group_difference_f(group1,group2,newgroup,ierror) &
   BIND(C, name="ompi_group_difference_f")
   implicit none
   INTEGER, INTENT(IN) :: group1
   INTEGER, INTENT(IN) :: group2
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_difference_f

subroutine ompi_group_excl_f(group,n,ranks,newgroup,ierror) &
   BIND(C, name="ompi_group_excl_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks(*)
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_excl_f

subroutine ompi_group_free_f(group,ierror) &
   BIND(C, name="ompi_group_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: group
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_free_f

subroutine ompi_group_incl_f(group,n,ranks,newgroup,ierror) &
   BIND(C, name="ompi_group_incl_f")
   implicit none
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks(*)
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_incl_f

subroutine ompi_group_intersection_f(group1,group2,newgroup,ierror) &
   BIND(C, name="ompi_group_intersection_f")
   implicit none
   INTEGER, INTENT(IN) :: group1
   INTEGER, INTENT(IN) :: group2
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_intersection_f

subroutine ompi_group_range_excl_f(group,n,ranges,newgroup,ierror) &
   BIND(C, name="ompi_group_range_excl_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranges(*)
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_range_excl_f

subroutine ompi_group_range_incl_f(group,n,ranges,newgroup,ierror) &
   BIND(C, name="ompi_group_range_incl_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranges(*)
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_range_incl_f

subroutine ompi_group_rank_f(group,rank,ierror) &
   BIND(C, name="ompi_group_rank_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: rank
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_rank_f

subroutine ompi_group_size_f(group,size,ierror) &
   BIND(C, name="ompi_group_size_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_size_f

subroutine ompi_group_translate_ranks_f(group1,n,ranks1,group2,ranks2,ierror) &
   BIND(C, name="ompi_group_translate_ranks_f")
   implicit none
   INTEGER, INTENT(IN) :: group1, group2
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks1(*)
   INTEGER, INTENT(OUT) :: ranks2(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_translate_ranks_f

subroutine ompi_group_union_f(group1,group2,newgroup,ierror) &
   BIND(C, name="ompi_group_union_f")
   implicit none
   INTEGER, INTENT(IN) :: group1, group2
   INTEGER, INTENT(OUT) :: newgroup
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_group_union_f

subroutine ompi_intercomm_create_f(local_comm,local_leader,peer_comm, &
                                   remote_leader,tag,newintercomm,ierror) &
   BIND(C, name="ompi_intercomm_create_f")
   implicit none
   INTEGER, INTENT(IN) :: local_comm, peer_comm
   INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
   INTEGER, INTENT(OUT) :: newintercomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_intercomm_create_f

subroutine ompi_type_create_keyval_f(type_copy_attr_fn,type_delete_attr_fn, &
                                     type_keyval,extra_state,ierror) &
   BIND(C, name="ompi_type_create_keyval_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: type_copy_attr_fn
   type(c_funptr), value :: type_delete_attr_fn
   INTEGER, INTENT(OUT) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_keyval_f

subroutine ompi_type_delete_attr_f(type,type_keyval,ierror) &
   BIND(C, name="ompi_type_delete_attr_f")
   implicit none
   INTEGER, INTENT(IN) :: type
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_delete_attr_f

subroutine ompi_type_free_keyval_f(type_keyval,ierror) &
   BIND(C, name="ompi_type_free_keyval_f")
   implicit none
   INTEGER, INTENT(INOUT) :: type_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_free_keyval_f

subroutine ompi_type_get_name_f(type,type_name,resultlen,ierror,type_name_len) &
   BIND(C, name="ompi_type_get_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: type
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: type_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: type_name_len
end subroutine ompi_type_get_name_f

subroutine ompi_type_set_attr_f(type,type_keyval,attribute_val,ierror) &
   BIND(C, name="ompi_type_set_attr_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: type
   INTEGER, INTENT(IN) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_set_attr_f

subroutine ompi_type_set_name_f(type,type_name,ierror,type_name_len) &
   BIND(C, name="ompi_type_set_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: type
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: type_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: type_name_len
end subroutine ompi_type_set_name_f

subroutine ompi_win_allocate_f(size, disp_unit, info, comm, &
      baseptr, win, ierror) BIND(C, name="ompi_win_allocate_f")
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_ADDRESS_KIND
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::  size
  INTEGER, INTENT(IN) ::  disp_unit
  INTEGER, INTENT(IN) ::  info
  INTEGER, INTENT(IN) ::  comm
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, INTENT(OUT) ::  win
  INTEGER, INTENT(OUT) ::  ierror
end subroutine ompi_win_allocate_f

subroutine ompi_win_allocate_shared_f(size, disp_unit, info, comm, &
      baseptr, win, ierror) BIND(C, name="ompi_win_allocate_shared_f")
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_ADDRESS_KIND
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::  size
  INTEGER, INTENT(IN) ::  disp_unit
  INTEGER, INTENT(IN) ::  info
  INTEGER, INTENT(IN) ::  comm
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, INTENT(OUT) ::  win
  INTEGER, INTENT(OUT) ::  ierror
end subroutine ompi_win_allocate_shared_f

subroutine ompi_win_create_keyval_f(win_copy_attr_fn,win_delete_attr_fn, &
                                    win_keyval,extra_state,ierror) &
   BIND(C, name="ompi_win_create_keyval_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: win_copy_attr_fn
   type(c_funptr), value :: win_delete_attr_fn
   INTEGER, INTENT(OUT) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_create_keyval_f

subroutine ompi_win_delete_attr_f(win,win_keyval,ierror) &
   BIND(C, name="ompi_win_delete_attr_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_delete_attr_f

subroutine ompi_win_free_keyval_f(win_keyval,ierror) &
   BIND(C, name="ompi_win_free_keyval_f")
   implicit none
   INTEGER, INTENT(INOUT) :: win_keyval
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_free_keyval_f

subroutine ompi_win_get_name_f(win,win_name,resultlen,ierror,win_name_len) &
   BIND(C, name="ompi_win_get_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: win
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: win_name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: win_name_len
end subroutine ompi_win_get_name_f

subroutine ompi_win_set_attr_f(win,win_keyval,attribute_val,ierror) &
   BIND(C, name="ompi_win_set_attr_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_set_attr_f

subroutine ompi_win_set_name_f(win,win_name,ierror,win_name_len) &
   BIND(C, name="ompi_win_set_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: win
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: win_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: win_name_len
end subroutine ompi_win_set_name_f

subroutine ompi_cartdim_get_f(comm,ndims,ierror) &
   BIND(C, name="ompi_cartdim_get_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ndims
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_cartdim_get_f

subroutine ompi_cart_coords_f(comm,rank,maxdims,coords,ierror) &
   BIND(C, name="ompi_cart_coords_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxdims
   INTEGER, INTENT(OUT) :: coords(maxdims)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_cart_coords_f

subroutine ompi_cart_rank_f(comm,coords,rank,ierror) &
   BIND(C, name="ompi_cart_rank_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: coords(*)
   INTEGER, INTENT(OUT) :: rank
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_cart_rank_f

subroutine ompi_cart_shift_f(comm,direction,disp,rank_source,rank_dest,ierror) &
   BIND(C, name="ompi_cart_shift_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: direction, disp
   INTEGER, INTENT(OUT) :: rank_source, rank_dest
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_cart_shift_f

subroutine ompi_dims_create_f(nnodes,ndims,dims,ierror) &
   BIND(C, name="ompi_dims_create_f")
   implicit none
   INTEGER, INTENT(IN) :: nnodes, ndims
   INTEGER, INTENT(INOUT) :: dims(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_dims_create_f

subroutine ompi_dist_graph_neighbors_f(comm,maxindegree,sources,sourceweights, &
                                       maxoutdegree,destinations,destweights,ierror) &
   BIND(C, name="ompi_dist_graph_neighbors_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
   INTEGER, INTENT(OUT) :: sources(maxindegree), destinations(maxoutdegree)
   INTEGER, INTENT(OUT) :: sourceweights(maxindegree), destweights(maxoutdegree)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_dist_graph_neighbors_f

subroutine ompi_graphdims_get_f(comm,nnodes,nedges,ierror) &
   BIND(C, name="ompi_graphdims_get_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: nnodes, nedges
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_graphdims_get_f

subroutine ompi_graph_get_f(comm,maxindex,maxedges,index,edges,ierror) &
   BIND(C, name="ompi_graph_get_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: maxindex, maxedges
   INTEGER, INTENT(OUT) :: index(*), edges(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_graph_get_f

subroutine ompi_graph_map_f(comm,nnodes,index,edges,newrank,ierror) &
   BIND(C, name="ompi_graph_map_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: nnodes
   INTEGER, INTENT(IN) :: index(*), edges(*)
   INTEGER, INTENT(OUT) :: newrank
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_graph_map_f

subroutine ompi_graph_neighbors_f(comm,rank,maxneighbors,neighbors,ierror) &
   BIND(C, name="ompi_graph_neighbors_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank, maxneighbors
   INTEGER, INTENT(OUT) :: neighbors(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_graph_neighbors_f

subroutine ompi_graph_neighbors_count_f(comm,rank,nneighbors,ierror) &
   BIND(C, name="ompi_graph_neighbors_count_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(OUT) :: nneighbors
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_graph_neighbors_count_f

subroutine ompi_topo_test_f(comm,status,ierror) &
   BIND(C, name="ompi_topo_test_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_topo_test_f

function  ompi_wtick_f() &
   BIND(C, name="ompi_wtick_f")
   implicit none
   DOUBLE PRECISION :: ompi_wtick_f
end function  ompi_wtick_f

function  ompi_wtime_f() &
   BIND(C, name="ompi_wtime_f")
   implicit none
   DOUBLE PRECISION :: ompi_wtime_f
end function  ompi_wtime_f

subroutine ompi_abort_f(comm,errorcode,ierror) &
   BIND(C, name="ompi_abort_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_abort_f

subroutine ompi_add_error_class_f(errorclass,ierror) &
   BIND(C, name="ompi_add_error_class_f")
   implicit none
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_add_error_class_f

subroutine ompi_add_error_code_f(errorclass,errorcode,ierror) &
   BIND(C, name="ompi_add_error_code_f")
   implicit none
   INTEGER, INTENT(IN) :: errorclass
   INTEGER, INTENT(OUT) :: errorcode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_add_error_code_f

subroutine ompi_add_error_string_f(errorcode,string,ierror,str_len) &
   BIND(C, name="ompi_add_error_string_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: errorcode
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: string
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: str_len
end subroutine ompi_add_error_string_f

subroutine ompi_alloc_mem_f(size,info,baseptr,ierror) &
   BIND(C, name="ompi_alloc_mem_f")
   use, intrinsic :: ISO_C_BINDING, only : C_PTR
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   INTEGER, INTENT(IN) :: info
   TYPE(C_PTR), INTENT(OUT) :: baseptr
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_alloc_mem_f

subroutine ompi_comm_call_errhandler_f(comm,errorcode,ierror) &
   BIND(C, name="ompi_comm_call_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_call_errhandler_f

subroutine ompi_comm_create_errhandler_f(comm_errhandler_fn,errhandler,ierror) &
   BIND(C, name="ompi_comm_create_errhandler_f")
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: comm_errhandler_fn
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_create_errhandler_f

subroutine ompi_comm_get_errhandler_f(comm,errhandler,ierror) &
   BIND(C, name="ompi_comm_get_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_get_errhandler_f

subroutine ompi_comm_set_errhandler_f(comm,errhandler,ierror) &
   BIND(C, name="ompi_comm_set_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_set_errhandler_f

subroutine ompi_errhandler_free_f(errhandler,ierror) &
   BIND(C, name="ompi_errhandler_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_errhandler_free_f

subroutine ompi_error_class_f(errorcode,errorclass,ierror) &
   BIND(C, name="ompi_error_class_f")
   implicit none
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_error_class_f

subroutine ompi_error_string_f(errorcode,string,resultlen,ierror,str_len) &
   BIND(C, name="ompi_error_string_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: errorcode
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: string
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: str_len
end subroutine ompi_error_string_f

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

subroutine ompi_file_call_errhandler_f(fh,errorcode,ierror) &
   BIND(C, name="ompi_file_call_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_call_errhandler_f

subroutine ompi_file_create_errhandler_f(file_errhandler_fn,errhandler,ierror) &
   BIND(C, name="ompi_file_create_errhandler_f")
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: file_errhandler_fn
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_create_errhandler_f

subroutine ompi_file_get_errhandler_f(file,errhandler,ierror) &
   BIND(C, name="ompi_file_get_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: file
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_errhandler_f

subroutine ompi_file_set_errhandler_f(file,errhandler,ierror) &
   BIND(C, name="ompi_file_set_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: file
   INTEGER, INTENT(IN) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_set_errhandler_f

! OMPI_PROFILE_MPI_FILE_INTERFACE
#endif

subroutine ompi_finalize_f(ierror) &
   BIND(C, name="ompi_finalize_f")
   implicit none
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_finalize_f

subroutine ompi_free_mem_f(base,ierror) &
   BIND(C, name="ompi_free_mem_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER(MPI_ADDRESS_KIND), DIMENSION(*) OMPI_ASYNCHRONOUS :: base
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_free_mem_f

subroutine ompi_get_processor_name_f(name,resultlen,ierror,name_len) &
   BIND(C, name="ompi_get_processor_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: name_len
end subroutine ompi_get_processor_name_f

subroutine ompi_get_version_f(version,subversion,ierror) &
   BIND(C, name="ompi_get_version_f")
   implicit none
   INTEGER, INTENT(OUT) :: version, subversion
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_version_f

subroutine ompi_init_f(ierror) &
   BIND(C, name="ompi_init_f")
   implicit none
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_init_f

subroutine ompi_win_call_errhandler_f(win,errorcode,ierror) &
   BIND(C, name="ompi_win_call_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(IN) :: errorcode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_call_errhandler_f

subroutine ompi_win_create_errhandler_f(win_errhandler_fn,errhandler,ierror) &
   BIND(C, name="ompi_win_create_errhandler_f")
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: win_errhandler_fn
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_create_errhandler_f

subroutine ompi_win_get_errhandler_f(win,errhandler,ierror) &
   BIND(C, name="ompi_win_get_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_get_errhandler_f

subroutine ompi_win_set_errhandler_f(win,errhandler,ierror) &
   BIND(C, name="ompi_win_set_errhandler_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(IN) :: errhandler
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_set_errhandler_f

subroutine ompi_info_create_f(info,ierror) &
   BIND(C, name="ompi_info_create_f")
   implicit none
   INTEGER, INTENT(OUT) :: info
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_info_create_f

subroutine ompi_info_delete_f(info,key,ierror,key_len) &
   BIND(C, name="ompi_info_delete_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: info
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: key_len
end subroutine ompi_info_delete_f

subroutine ompi_info_dup_f(info,newinfo,ierror) &
   BIND(C, name="ompi_info_dup_f")
   implicit none
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: newinfo
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_info_dup_f

subroutine ompi_info_free_f(info,ierror) &
   BIND(C, name="ompi_info_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: info
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_info_free_f

subroutine ompi_info_get_nkeys_f(info,nkeys,ierror) &
   BIND(C, name="ompi_info_get_nkeys_f")
   implicit none
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: nkeys
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_info_get_nkeys_f

subroutine ompi_info_get_nthkey_f(info,n,key,ierror,key_len) &
   BIND(C, name="ompi_info_get_nthkey_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(IN) :: n
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: key
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: key_len
end subroutine ompi_info_get_nthkey_f

subroutine ompi_info_set_f(info,key,value,ierror,key_len,value_len) &
   BIND(C, name="ompi_info_set_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: info
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key, value
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: key_len, value_len
end subroutine ompi_info_set_f

subroutine ompi_close_port_f(port_name,ierror,port_name_len) &
   BIND(C, name="ompi_close_port_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: port_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: port_name_len
end subroutine ompi_close_port_f

subroutine ompi_comm_accept_f(port_name,info,root,comm,newcomm,ierror,port_name_len) &
   BIND(C, name="ompi_comm_accept_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: port_name
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: port_name_len
end subroutine ompi_comm_accept_f

subroutine ompi_comm_connect_f(port_name,info,root,comm,newcomm,ierror,port_name_len) &
   BIND(C, name="ompi_comm_connect_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: port_name
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(IN) :: root
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: port_name_len
end subroutine ompi_comm_connect_f

subroutine ompi_comm_disconnect_f(comm,ierror) &
   BIND(C, name="ompi_comm_disconnect_f")
   implicit none
   INTEGER, INTENT(INOUT) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_disconnect_f

subroutine ompi_comm_get_parent_f(parent,ierror) &
   BIND(C, name="ompi_comm_get_parent_f")
   implicit none
   INTEGER, INTENT(OUT) :: parent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_get_parent_f

subroutine ompi_comm_join_f(fd,intercomm,ierror) &
   BIND(C, name="ompi_comm_join_f")
   implicit none
   INTEGER, INTENT(IN) :: fd
   INTEGER, INTENT(OUT) :: intercomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_join_f

subroutine ompi_comm_spawn_f(command,argv,maxprocs,info,root,comm, &
                             intercomm, array_of_errcodes,ierror,cmd_len,argv_len) &
   BIND(C, name="ompi_comm_spawn_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: command, argv
   INTEGER, INTENT(IN) :: maxprocs, root
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: intercomm
   INTEGER, INTENT(OUT) :: array_of_errcodes(*)
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: cmd_len, argv_len
end subroutine ompi_comm_spawn_f


! TODO - FIXME to use arrays of strings and pass strlen
subroutine ompi_comm_spawn_multiple_f(count,array_of_commands, &
                                      array_of_argv, array_of_maxprocs,array_of_info,root, &
                                      comm,intercomm,array_of_errcodes,ierror) &
   BIND(C, name="ompi_comm_spawn_multiple_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: array_of_maxprocs(count)
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: array_of_commands(*), array_of_argv(*)
   INTEGER, INTENT(IN) :: array_of_info(count)
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: intercomm
   INTEGER, INTENT(OUT) :: array_of_errcodes(*)
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_spawn_multiple_f

subroutine ompi_lookup_name_f(service_name,info,port_name,ierror, &
                              service_name_len,port_name_len) &
   BIND(C, name="ompi_lookup_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: service_name
   INTEGER, INTENT(IN) :: info
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: port_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: service_name_len, port_name_len
end subroutine ompi_lookup_name_f

subroutine ompi_open_port_f(info,port_name,ierror,port_name_len) &
   BIND(C, name="ompi_open_port_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: info
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: port_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: port_name_len
end subroutine ompi_open_port_f

subroutine ompi_publish_name_f(service_name,info,port_name,ierror, &
                               service_name_len,port_name_len) &
   BIND(C, name="ompi_publish_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: info
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: service_name, port_name
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: service_name_len, port_name_len
end subroutine ompi_publish_name_f

subroutine ompi_unpublish_name_f(service_name,info,port_name, &
                                 ierror,service_name_len,port_name_len) &
   BIND(C, name="ompi_unpublish_name_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: service_name, port_name
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: service_name_len, port_name_len
end subroutine ompi_unpublish_name_f

subroutine ompi_accumulate_f(origin_addr,origin_count,origin_datatype, &
                             target_rank,target_disp, &
                             target_count,target_datatype,op,win,ierror) &
   BIND(C, name="ompi_accumulate_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_accumulate_f

subroutine ompi_raccumulate_f(origin_addr,origin_count,origin_datatype, &
                              target_rank,target_disp, &
                              target_count,target_datatype,op,win, &
                              request,ierror) &
   BIND(C, name="ompi_raccumulate_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_raccumulate_f

subroutine ompi_get_f(origin_addr,origin_count,origin_datatype,target_rank, &
                      target_disp,target_count,target_datatype,win,ierror) &
   BIND(C, name="ompi_get_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_f

subroutine ompi_rget_f(origin_addr,origin_count,origin_datatype,target_rank, &
                       target_disp,target_count,target_datatype,win,request, &
                       ierror) &
   BIND(C, name="ompi_rget_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_rget_f

subroutine ompi_get_accumulate_f(origin_addr,origin_count,origin_datatype, &
                                 result_addr,result_count,result_datatype, &
                                 target_rank,target_disp, &
                                 target_count,target_datatype,op,win, &
                                 ierror) &
   BIND(C, name="ompi_get_accumulate_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   INTEGER, INTENT(IN) :: result_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_get_accumulate_f

subroutine ompi_rget_accumulate_f(origin_addr,origin_count,origin_datatype, &
                                  result_addr,result_count,result_datatype, &
                                  target_rank,target_disp, &
                                  target_count,target_datatype,op,win, &
                                  request,ierror) &
   BIND(C, name="ompi_rget_accumulate_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   INTEGER, INTENT(IN) :: result_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_rget_accumulate_f

subroutine ompi_put_f(origin_addr,origin_count,origin_datatype,target_rank, &
                      target_disp,target_count,target_datatype,win,ierror) &
   BIND(C, name="ompi_put_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_put_f

subroutine ompi_rput_f(origin_addr,origin_count,origin_datatype,target_rank, &
                       target_disp,target_count,target_datatype,win,request, &
                       ierror) &
   BIND(C, name="ompi_rput_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   INTEGER, INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: target_datatype
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_rput_f

subroutine ompi_win_complete_f(win,ierror) &
   BIND(C, name="ompi_win_complete_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_complete_f

subroutine ompi_compare_and_swap_f(origin_addr,compare_addr,result_addr, &
                                   datatype,target_rank,target_disp, win, &
                                   ierror) &
   BIND(C, name="ompi_compare_and_swap_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr, compare_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: target_rank
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_compare_and_swap_f

subroutine ompi_fetch_and_op_f(origin_addr,result_addr,datatype,target_rank, &
                               target_disp,op,win,ierror) &
   BIND(C, name="ompi_fetch_and_op_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: target_rank
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_fetch_and_op_f

subroutine ompi_win_create_f(base,size,disp_unit,info,comm,win,ierror) &
   BIND(C, name="ompi_win_create_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: size
   INTEGER, INTENT(IN) :: disp_unit
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_create_f

subroutine ompi_win_flush_f(rank,win,ierror) &
   BIND(C, name="ompi_win_flush_f")
   implicit none
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_flush_f

subroutine ompi_win_flush_all_f(win,ierror) &
   BIND(C, name="ompi_win_flush_all_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_flush_all_f

subroutine ompi_win_flush_local_f(rank,win,ierror) &
   BIND(C, name="ompi_win_flush_local_f")
   implicit none
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_flush_local_f

subroutine ompi_win_flush_local_all_f(win,ierror) &
   BIND(C, name="ompi_win_flush_local_all_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_flush_local_all_f

subroutine ompi_win_fence_f(assert,win,ierror) &
   BIND(C, name="ompi_win_fence_f")
   implicit none
   INTEGER, INTENT(IN) :: assert
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_fence_f

subroutine ompi_win_free_f(win,ierror) &
   BIND(C, name="ompi_win_free_f")
   implicit none
   INTEGER, INTENT(INOUT) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_free_f

subroutine ompi_win_get_group_f(win,group,ierror) &
   BIND(C, name="ompi_win_get_group_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: group
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_get_group_f

subroutine ompi_win_lock_f(lock_type,rank,assert,win,ierror) &
   BIND(C, name="ompi_win_lock_f")
   implicit none
   INTEGER, INTENT(IN) :: lock_type, rank, assert
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_lock_f

subroutine ompi_win_lock_all_f(assert,win,ierror) &
   BIND(C, name="ompi_win_lock_all_f")
   implicit none
   INTEGER, INTENT(IN) :: assert
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_lock_all_f

subroutine ompi_win_post_f(group,assert,win,ierror) &
   BIND(C, name="ompi_win_post_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_post_f

subroutine ompi_win_shared_query_f(win, rank, size, disp_unit, baseptr,&
      ierror) BIND(C, name="ompi_win_shared_query_f")
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_ADDRESS_KIND
  INTEGER, INTENT(IN) ::  win
  INTEGER, INTENT(IN) ::  rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::  size
  INTEGER, INTENT(OUT) ::  disp_unit
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, INTENT(OUT) ::  ierror
end subroutine ompi_win_shared_query_f

subroutine ompi_win_start_f(group,assert,win,ierror) &
   BIND(C, name="ompi_win_start_f")
   implicit none
   INTEGER, INTENT(IN) :: group
   INTEGER, INTENT(IN) :: assert
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_start_f

subroutine ompi_win_sync_f(win,ierror) &
   BIND(C, name="ompi_win_sync_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_sync_f

subroutine ompi_win_unlock_f(rank,win,ierror) &
   BIND(C, name="ompi_win_unlock_f")
   implicit none
   INTEGER, INTENT(IN) :: rank
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_unlock_f

subroutine ompi_win_unlock_all_f(win,ierror) &
   BIND(C, name="ompi_win_unlock_all_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_unlock_all_f

subroutine ompi_win_wait_f(win,ierror) &
   BIND(C, name="ompi_win_wait_f")
   implicit none
   INTEGER, INTENT(IN) :: win
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_win_wait_f

subroutine ompi_grequest_complete_f(request,ierror) &
   BIND(C, name="ompi_grequest_complete_f")
   implicit none
   INTEGER, INTENT(IN) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_grequest_complete_f

subroutine ompi_grequest_start_f(query_fn,free_fn,cancel_fn, &
                                 extra_state,request,ierror) &
   BIND(C, name="ompi_grequest_start_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: query_fn
   type(c_funptr), value :: free_fn
   type(c_funptr), value :: cancel_fn
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_grequest_start_f

subroutine ompi_init_thread_f(required,provided,ierror) &
   BIND(C, name="ompi_init_thread_f")
   implicit none
   INTEGER, INTENT(IN) :: required
   INTEGER, INTENT(OUT) :: provided
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_init_thread_f

subroutine ompi_query_thread_f(provided,ierror) &
   BIND(C, name="ompi_query_thread_f")
   implicit none
   INTEGER, INTENT(OUT) :: provided
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_query_thread_f

subroutine ompi_status_set_elements_f(status,datatype,count,ierror) &
   BIND(C, name="ompi_status_set_elements_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_status_set_elements_f

subroutine ompi_status_set_elements_x_f(status,datatype,count,ierror) &
   BIND(C, name="ompi_status_set_elements_x_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_COUNT_KIND
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_COUNT_KIND), INTENT(IN) :: count
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_status_set_elements_x_f

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

subroutine ompi_file_close_f(fh,ierror) &
   BIND(C, name="ompi_file_close_f")
   implicit none
   INTEGER, INTENT(INOUT) :: fh
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_close_f

subroutine ompi_file_delete_f(filename,info,ierror,filename_len) &
   BIND(C, name="ompi_file_delete_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: filename_len
end subroutine ompi_file_delete_f

subroutine ompi_file_get_amode_f(fh,amode,ierror) &
   BIND(C, name="ompi_file_get_amode_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: amode
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_amode_f

subroutine ompi_file_get_byte_offset_f(fh,offset,disp,ierror) &
   BIND(C, name="ompi_file_get_byte_offset_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_byte_offset_f

subroutine ompi_file_get_group_f(fh,group,ierror) &
   BIND(C, name="ompi_file_get_group_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: group
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_group_f

subroutine ompi_file_get_info_f(fh,info_used,ierror) &
   BIND(C, name="ompi_file_get_info_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: info_used
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_info_f

subroutine ompi_file_get_position_f(fh,offset,ierror) &
   BIND(C, name="ompi_file_get_position_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_position_f

subroutine ompi_file_get_position_shared_f(fh,offset,ierror) &
   BIND(C, name="ompi_file_get_position_shared_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: offset
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_position_shared_f

subroutine ompi_file_get_size_f(fh,size,ierror) &
   BIND(C, name="ompi_file_get_size_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_size_f

subroutine ompi_file_get_type_extent_f(fh,datatype,extent,ierror) &
   BIND(C, name="ompi_file_get_type_extent_f")
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: extent
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_get_type_extent_f

subroutine ompi_file_get_view_f(fh,disp,etype,filetype,datarep,ierror,datarep_len) &
   BIND(C, name="ompi_file_get_view_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   INTEGER, INTENT(OUT) :: etype
   INTEGER, INTENT(OUT) :: filetype
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: datarep
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_file_get_view_f

subroutine ompi_file_iread_f(fh,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iread_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iread_f

subroutine ompi_file_iread_at_f(fh,offset,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iread_at_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iread_at_f

subroutine ompi_file_iread_shared_f(fh,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iread_shared_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iread_shared_f

subroutine ompi_file_iwrite_f(fh,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iwrite_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iwrite_f

subroutine ompi_file_iwrite_at_f(fh,offset,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iwrite_at_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iwrite_at_f

subroutine ompi_file_iwrite_shared_f(fh,buf,count,datatype,request,ierror) &
   BIND(C, name="ompi_file_iwrite_shared_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_iwrite_shared_f

subroutine ompi_file_open_f(comm,filename,amode,info,fh,ierror,filename_len) &
   BIND(C, name="ompi_file_open_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   INTEGER, INTENT(IN) :: comm
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
   INTEGER, INTENT(IN) :: amode
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: fh
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: filename_len
end subroutine ompi_file_open_f

subroutine ompi_file_preallocate_f(fh,size,ierror) &
   BIND(C, name="ompi_file_preallocate_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_preallocate_f

subroutine ompi_file_read_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_f

subroutine ompi_file_read_all_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_all_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_all_f

subroutine ompi_file_read_all_begin_f(fh,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_read_all_begin_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_all_begin_f

subroutine ompi_file_read_all_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_read_all_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_all_end_f

subroutine ompi_file_read_at_f(fh,offset,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_at_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_at_f

subroutine ompi_file_read_at_all_f(fh,offset,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_at_all_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_at_all_f

subroutine ompi_file_read_at_all_begin_f(fh,offset,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_read_at_all_begin_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_at_all_begin_f

subroutine ompi_file_read_at_all_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_read_at_all_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_at_all_end_f

subroutine ompi_file_read_ordered_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_ordered_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_ordered_f

subroutine ompi_file_read_ordered_begin_f(fh,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_read_ordered_begin_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_ordered_begin_f

subroutine ompi_file_read_ordered_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_read_ordered_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_ordered_end_f

subroutine ompi_file_read_shared_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_read_shared_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_read_shared_f

subroutine ompi_file_seek_f(fh,offset,whence,ierror) &
   BIND(C, name="ompi_file_seek_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_seek_f

subroutine ompi_file_seek_shared_f(fh,offset,whence,ierror) &
   BIND(C, name="ompi_file_seek_shared_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER, INTENT(IN) :: whence
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_seek_shared_f

subroutine ompi_file_set_info_f(fh,info,ierror) &
   BIND(C, name="ompi_file_set_info_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_set_info_f

subroutine ompi_file_set_size_f(fh,size,ierror) &
   BIND(C, name="ompi_file_set_size_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: size
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_set_size_f

subroutine ompi_file_set_view_f(fh,disp,etype,filetype,datarep,info,ierror,datarep_len) &
   BIND(C, name="ompi_file_set_view_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
   INTEGER, INTENT(IN) :: etype
   INTEGER, INTENT(IN) :: filetype
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: datarep
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_file_set_view_f

subroutine ompi_file_sync_f(fh,ierror) &
   BIND(C, name="ompi_file_sync_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_sync_f

subroutine ompi_file_write_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_f

subroutine ompi_file_write_all_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_all_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_all_f

subroutine ompi_file_write_all_begin_f(fh,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_write_all_begin_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_all_begin_f

subroutine ompi_file_write_all_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_write_all_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_all_end_f

subroutine ompi_file_write_at_f(fh,offset,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_at_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_at_f

subroutine ompi_file_write_at_all_f(fh,offset,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_at_all_f")
   use :: mpi_f08_types, only : MPI_Status, MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_at_all_f

subroutine ompi_file_write_at_all_begin_f(fh,offset,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_write_at_all_begin_f")
   use :: mpi_f08_types, only : MPI_OFFSET_KIND
   implicit none
   INTEGER, INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_at_all_begin_f

subroutine ompi_file_write_at_all_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_write_at_all_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_at_all_end_f

subroutine ompi_file_write_ordered_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_ordered_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_ordered_f

subroutine ompi_file_write_ordered_begin_f(fh,buf,count,datatype,ierror) &
   BIND(C, name="ompi_file_write_ordered_begin_f")
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_ordered_begin_f

subroutine ompi_file_write_ordered_end_f(fh,buf,status,ierror) &
   BIND(C, name="ompi_file_write_ordered_end_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_ordered_end_f

subroutine ompi_file_write_shared_f(fh,buf,count,datatype,status,ierror) &
   BIND(C, name="ompi_file_write_shared_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_file_write_shared_f

! OMPI_PROVIDE_MPI_FILE_INTERFACE
#endif

subroutine ompi_register_datarep_f(datarep,read_conversion_fn, &
                                   write_conversion_fn,dtype_file_extent_fn, &
                                   extra_state,ierror,datarep_len) &
   BIND(C, name="ompi_register_datarep_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use, intrinsic :: iso_c_binding, only: c_funptr
   implicit none
   type(c_funptr), value :: read_conversion_fn
   type(c_funptr), value :: write_conversion_fn
   type(c_funptr), value :: dtype_file_extent_fn
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: datarep_len
end subroutine ompi_register_datarep_f

!
! MPI_Sizeof is generic for numeric types.  This ignore TKR interface
! is replaced by the specific generics.
!
!subroutine ompi_sizeof(x,size,ierror) &
!   BIND(C, name="ompi_sizeof_f")
!   implicit none
!   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: x
!   INTEGER, INTENT(OUT) :: size
!   INTEGER, INTENT(OUT) :: ierror
!end subroutine ompi_sizeof

subroutine ompi_type_create_f90_complex_f(p,r,newtype,ierror) &
   BIND(C, name="ompi_type_create_f90_complex_f")
   implicit none
   INTEGER, INTENT(IN) :: p, r
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_f90_complex_f

subroutine ompi_type_create_f90_integer_f(r,newtype,ierror) &
   BIND(C, name="ompi_type_create_f90_integer_f")
   implicit none
   INTEGER, INTENT(IN) :: r
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_f90_integer_f

subroutine ompi_type_create_f90_real_f(p,r,newtype,ierror) &
   BIND(C, name="ompi_type_create_f90_real_f")
   implicit none
   INTEGER, INTENT(IN) :: p, r
   INTEGER, INTENT(OUT) :: newtype
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_create_f90_real_f

subroutine ompi_type_match_size_f(typeclass,size,type,ierror) &
   BIND(C, name="ompi_type_match_size_f")
   implicit none
   INTEGER, INTENT(IN) :: typeclass, size
   INTEGER, INTENT(OUT) :: type
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_type_match_size_f

subroutine ompi_pcontrol_f(level) &
   BIND(C, name="ompi_pcontrol_f")
   implicit none
   INTEGER, INTENT(IN) :: level
end subroutine ompi_pcontrol_f


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! New routines to MPI-3
!

subroutine ompi_comm_split_type_f(comm,split_type,key,info,newcomm,ierror) &
   BIND(C, name="ompi_comm_split_type_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: split_type
   INTEGER, INTENT(IN) :: key
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: newcomm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_comm_split_type_f

subroutine ompi_f_sync_reg_f(buf) &
   BIND(C, name="ompi_f_sync_reg_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
end subroutine ompi_f_sync_reg_f

subroutine ompi_get_library_version_f(name,resultlen,ierror,name_len) &
   BIND(C, name="ompi_get_library_version_f")
   use, intrinsic :: ISO_C_BINDING, only : C_CHAR
   implicit none
   CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
   INTEGER, INTENT(OUT) :: resultlen
   INTEGER, INTENT(OUT) :: ierror
   INTEGER, VALUE, INTENT(IN) :: name_len
end subroutine ompi_get_library_version_f

subroutine ompi_mprobe_f(source,tag,comm,message,status,ierror) &
   BIND(C, name="ompi_mprobe_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: source, tag
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: message
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_mprobe_f

subroutine ompi_imrecv_f(buf,count,datatype,message,request,ierror) &
   BIND(C, name="ompi_imrecv_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(INOUT) :: message
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_imrecv_f

subroutine ompi_mrecv_f(buf,count,datatype,message,status,ierror) &
   BIND(C, name="ompi_mrecv_f")
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(INOUT) :: message
   TYPE(MPI_Status) :: status
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_mrecv_f

subroutine ompi_neighbor_allgather_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,ierror) &
                             BIND(C, name="ompi_neighbor_allgather_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_neighbor_allgather_f

subroutine ompi_ineighbor_allgather_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                             comm,request,ierror) &
                             BIND(C, name="ompi_ineighbor_allgather_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ineighbor_allgather_f

subroutine ompi_neighbor_allgatherv_f(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,ierror) &
                              BIND(C, name="ompi_neighbor_allgatherv_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_neighbor_allgatherv_f

subroutine ompi_ineighbor_allgatherv_f(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                              recvtype,comm,request,ierror) &
                              BIND(C, name="ompi_ineighbor_allgatherv_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ineighbor_allgatherv_f

subroutine ompi_neighbor_alltoall_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,ierror) &
                            BIND(C, name="ompi_neighbor_alltoall_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_neighbor_alltoall_f

subroutine ompi_ineighbor_alltoall_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                            comm,request,ierror) &
                            BIND(C, name="ompi_ineighbor_alltoall_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ineighbor_alltoall_f

subroutine ompi_neighbor_alltoallv_f(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,ierror) &
                             BIND(C, name="ompi_neighbor_alltoallv_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_neighbor_alltoallv_f

subroutine ompi_ineighbor_alltoallv_f(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                             rdispls,recvtype,comm,request,ierror) &
                             BIND(C, name="ompi_ineighbor_alltoallv_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ineighbor_alltoallv_f

subroutine ompi_neighbor_alltoallw_f(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,ierror) &
                             BIND(C, name="ompi_neighbor_alltoallw_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes, recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_neighbor_alltoallw_f

subroutine ompi_ineighbor_alltoallw_f(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                             rdispls,recvtypes,comm,request,ierror) &
                             BIND(C, name="ompi_ineighbor_alltoallw_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request, MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes, recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompi_ineighbor_alltoallw_f

end interface
